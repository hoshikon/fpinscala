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

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop((testCases, rng) => {
    this.run(testCases, rng) match {
      case Passed => p.run(testCases, rng)
      case f => f
    }
  })

  def ||(p: Prop): Prop = Prop((testCases, rng) => {
    this.run(testCases, rng) match {
      case Falsified(msg,_) => p.tag(msg).run(testCases, rng)
      case p => p
    }
  })

  def tag(msg: String) = Prop {
    (n,rng) => run(n,rng) match {
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
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
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

case class Gen[A](sample: State[RNG,A]) {
  def value(implicit rng: RNG) = sample.run(rng)._1
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
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

trait SGen[+A] {

}

