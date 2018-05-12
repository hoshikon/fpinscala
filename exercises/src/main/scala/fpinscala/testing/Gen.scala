package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.datastructures.{Branch, Leaf, Tree}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop {
//  def checkBool: Boolean
//  def &&(p: Prop): Prop = {
//    def thisCheck = checkBool
//    new Prop {
//      override def checkBool: Boolean = thisCheck && p.checkBool
//    }
//  }
//
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop((maxSize, testCases, rng) => {
    this.run(maxSize, testCases, rng) match {
      case Passed|Proved => p.run(maxSize, testCases, rng)
      case f => f
    }
  })

  def ||(p: Prop): Prop = Prop((maxSize, testCases, rng) => {
    this.run(maxSize, testCases, rng) match {
      case Falsified(msg,_) => p.tag(msg).run(maxSize, testCases, rng)
      case p => p
    }
  })

  def tag(msg: String) = Prop {
    (maxSize, n,rng) => run(maxSize, n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

case class PropWithTag(run: (TestCases, RNG) => Result) {
    def &&(p: PropWithTag): PropWithTag = PropWithTag((testCases, rng) => {
      this.run(testCases, rng) match {
        case f@FalsifiedWithTag(_, _, _) => f
        case _ => p.run(testCases, rng)
      }
    })

    def ||(p: PropWithTag): PropWithTag = PropWithTag((testCases, rng) => {
      this.run(testCases, rng) match {
        case p@Passed => p
        case f1@FalsifiedWithTag(_, _, s) => p.run(testCases, rng) match {
          case p@Passed => p
          case f2@FalsifiedWithTag(_, _, s2) => if (s <= s2) f1 else f2 // returns info of the first failure
        }}
    })
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

//Stores all historical results and reuse them if re-requested
//  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//    (_, n, rng) =>
//      val results = mutable.Set[A]()
//      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//        case (a, i) => try {
//          if (results.contains(a) || f(a)) {
//            results.add(a)
//            Passed
//          } else Falsified(a.toString, i)
//        } catch {
//          case e: Exception => Falsified(buildMsg(a, e), i)
//        }
//      }.find(_.isFalsified).getOrElse(Passed)
//  }

  private val fixedThreadPools: Map[Int, ExecutorService] = (1 to 4).map(n => n -> Executors.newFixedThreadPool(n)).toMap
  private val cachedThreadPool: ExecutorService = Executors.newCachedThreadPool

  def shutdownAllPools = (cachedThreadPool :: fixedThreadPools.values.toList).foreach(_.shutdown())

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(fixedThreadPools) -> .75,
    unit(cachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case s ** a => f(a)(s).get }
  def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop = forAll(S.unsized ** g) { case s ** a => f(a)(s).get }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): String =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => s"! Falsified after $n passed tests:\n $msg"
      case Passed => s"+ OK, passed $testCases tests."
      case Proved => s"+ OK, proved property."
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Passed else Falsified("()", 0) }
  def checkPar(p: => Par[Boolean]): Prop = check(p(cachedThreadPool).get)
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p,p2)(_ == _)

  val pint = Gen.choose(0,10) map (Par.unit)
  lazy val pint2: Gen[Par[Int]] = choose(0,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
  val p4WithPint2 = forAllPar(pint2)(n => equal(Par.map(n)(y => y), n))

  val pFork = forAllPar(pint)((n: Par[Int]) => equal(Par.fork(n), n))
}

object PropWithTag {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAllWithTag[A](tag: String, as: Gen[A])(f: A => Boolean): PropWithTag = PropWithTag {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else FalsifiedWithTag(tag, a.toString, i)
      } catch { case e: Exception => FalsifiedWithTag(tag, buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}
case object Proved extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
case class FalsifiedWithTag(tag: String = "", failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Gen[+A](sample: State[RNG,A]) {
  def value(implicit rng: RNG): A = sample.run(rng)._1
  def map[B](f: A => B) = Gen(sample.map(f))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.flatMap(a => g.sample.map(f(a, _))))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))
  def streamOf[A](g: Gen[A]): SGen[Stream[A]] = SGen(n => Gen(State.streamSequence(Stream.constant(g.sample).take(n))))
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(_ => Gen.listOfN(1, g))

  def treeOfN[A](n: Int, g: Gen[A]): Gen[Tree[A]] = {
    if (n <= 1) g.map(Leaf.apply)
    else Gen.choose(1, n).flatMap(i => treeOfN(i, g).map2(treeOfN(n-i, g))(Branch.apply))
  }

  def treeOf[A](g: Gen[A]): SGen[Tree[A]] = SGen(Gen.treeOfN(_, g))


  //  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
//    val state = State((rng: RNG) => {
//      (0 to n).foldLeft((List.empty[A], rng))((acc, _) => {
//        val (list, r) = acc
//        val (x, r2) = g.sample.run(r)
//        (x :: list, r2)
//      })
//    })
//    Gen(state)
//  }

  def int: Gen[Int] = Gen(State(RNG.int))
  def intTuple: Gen[(Int, Int)] = Gen(State(RNG.map2(RNG.int, RNG.int)((_, _))))
  def toOption[A](g: Gen[A]): Gen[Option[A]] = g.copy(sample = g.sample.map(Some(_)))
  def fromOption[A](g: Gen[Option[A]]) = g.copy(sample = g.sample.map(_.get)) //This is not useful because it cannot handle None...
  def string(length: Int): Gen[String] = {
    val genIntList: Gen[List[Int]] = Gen.listOfN(length, Gen.int)
    genIntList.copy(sample = genIntList.sample.map(_.map(_.toChar).mkString))
  }

  def stringN(n: Int): Gen[String] = listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)).flatMap(d => if (d < (g1._2/(g1._2 + g2._2))) g1._1 else g2._1)

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => (s => i))

  def aFunctionThatUsesItsArgumentInSomewayToSelectWhichIntToReturn[A](arg: A)(someWayToSelectWhichIntToReturn: A => Int): Int = someWayToSelectWhichIntToReturn(arg)
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(n => forSize(n).map(f))
  def map2[B, C](s: SGen[B])(f: (A, B) => C): SGen[C] = this.flatMap(a => s.map(f(a, _)))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
  def **[B](g: SGen[B]): SGen[(A,B)] = (this map2 g)((_,_))
}

object SGen {
  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized
  def choose(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose(start, stopExclusive).unsized

  def boolean: SGen[Boolean] = Gen.boolean.unsized
  def int: SGen[Int] = Gen.int.unsized
  def intTuple: SGen[(Int, Int)] = Gen.intTuple.unsized
  def string(length: Int): SGen[String] = Gen.string(length).unsized

  def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] = boolean.flatMap(if(_) g1 else g2)
  def weighted[A](g1: (SGen[A], Double), g2: (SGen[A], Double)): SGen[A] =
    Gen(State(RNG.double)).unsized.flatMap(d => if (d < (g1._2/(g1._2 + g2._2))) g1._1 else g2._1)

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }
}

//trait SGen[+A] {
//
//}

