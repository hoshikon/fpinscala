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

case class Gen[A](sample: State[RNG,A])

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ >= 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val state = State((rng: RNG) => {
      (0 to n).foldLeft((List.empty[A], rng))((acc, _) => {
        val (list, r) = acc
        val (x, r2) = g.sample.run(r)
        (x :: list, r2)
      })
    })
    Gen(state)
    //Gen(State.sequence(List.fill(n)(g.sample))) //from answer
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

