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

trait Prop {
//  def checkBool: Boolean
//  def &&(p: Prop): Prop = {
//    def thisCheck = checkBool
//    new Prop {
//      override def checkBool: Boolean = thisCheck && p.checkBool
//    }
//  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

