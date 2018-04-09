package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

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
      case Passed => p.run(maxSize, testCases, rng)
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

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
case class FalsifiedWithTag(tag: String = "", failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Gen[+A](sample: State[RNG,A]) {
  def value(implicit rng: RNG): A = sample.run(rng)._1
  def map[B](f: A => B) = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)).flatMap(d => if (d < (g1._2/(g1._2 + g2._2))) g1._1 else g2._1)
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(n => forSize(n).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
}

object SGen {
  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized
  def choose(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose(start, stopExclusive).unsized

  def boolean: SGen[Boolean] = Gen.boolean.unsized
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))

  def int: SGen[Int] = Gen.int.unsized
  def intTuple: SGen[(Int, Int)] = Gen.intTuple.unsized
  def string(length: Int): SGen[String] = Gen.string(length).unsized

  def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] = boolean.flatMap(if(_) g1 else g2)
  def weighted[A](g1: (SGen[A], Double), g2: (SGen[A], Double)): SGen[A] =
    Gen(State(RNG.double)).unsized.flatMap(d => if (d < (g1._2/(g1._2 + g2._2))) g1._1 else g2._1)
}

//trait SGen[+A] {
//
//}

