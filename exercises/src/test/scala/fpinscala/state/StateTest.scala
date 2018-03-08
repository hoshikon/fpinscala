package fpinscala.state

import fpinscala.SimpleBooleanTest
import fpinscala.state.RNG.Simple

import scala.collection.immutable.Stream.cons

object StateTest extends App with SimpleBooleanTest{
  private def createStream[A](rng: RNG)(f: RNG => (A, RNG)): Stream[A] = {
    val (v, r) = f(rng)
    cons(v, createStream(r)(f))
  }

  private def createList[A](rng: RNG)(f: RNG => (A, RNG)): List[A] = createStream(rng)(f).take(10000).toList

  override def run: Unit = {
    val rng: RNG = Simple(1)

    val nonNegs = createList(rng)(RNG.nonNegativeInt)
    val nonNegs2 = createList(rng)(RNG.nonNegativeInt)
    val nonNegativeIntTest = nonNegs.forall(_ >= 0) && nonNegs == nonNegs2
    println(nonNegativeIntTest + ": non negative int")

    val doubles = createList(rng)(RNG.double)
    val doubles2 = createList(rng)(RNG.double)
    val doubleTest = doubles.forall(d => 0 <= d && d < 1) && doubles == doubles2
    println(doubleTest + ": double")

    val intDoubles = createList(rng)(RNG.intDouble)
    val intDoubles2 = createList(rng)(RNG.intDouble)
    val intDoubleTest = intDoubles == intDoubles2
    println(intDoubleTest + ": intDouble")

    val doubleInts = createList(rng)(RNG.doubleInt)
    val doubleInts2 = createList(rng)(RNG.doubleInt)
    val doubleIntTest = doubleInts == doubleInts2
    println(doubleIntTest + ": doubleInt")

    val double3s = createList(rng)(RNG.double3)
    val double3s2 = createList(rng)(RNG.double3)
    val double3Test = double3s == double3s2
    println(double3Test + ": double3")

    val ints1 = RNG.ints(1000)(rng)._1
    val ints2 = RNG.ints(1000)(rng)._1
    val ints3 = createList(rng)(_.nextInt).take(1000)
    val intsTest = ints1 == ints2 && ints2 == ints3
    println(intsTest + ": ints")

    val anotherDoubles = createList(rng)(RNG.doubleWithMap)
    val anotherDoubles2 = createList(rng)(RNG.doubleWithMap)
    val doubleWithMapTest = anotherDoubles.forall(d => 0 <= d && d < 1) && anotherDoubles == anotherDoubles2
    println(doubleWithMapTest + ": double with map")

    val intDoublesWithMap2 = createList(rng)(RNG.map2(RNG.int, RNG.double)((_,_)))
    val map2Test = intDoubles == intDoublesWithMap2
    println(map2Test + ": map2")

    val sequence = RNG.sequence(List(RNG.int, r => RNG.nonNegativeInt(r)))(rng)
    val sequence2 = RNG.sequenceWithFoldRight(List(RNG.int, r => RNG.nonNegativeInt(r)))(rng)
    val (i, rngi) = RNG.int(rng)
    val (j, rngj) = RNG.nonNegativeInt(rngi)
    val sequenceTest = sequence == (List(i, j), rngj)
    val sequenceTest2 = sequence == sequence2
    println((sequenceTest && sequenceTest2) + ": sequence")

    val intsWithSequence: List[Int] = RNG.intsWithSequence(1000)(rng)._1
    val intsWithSequenceTest = ints1 == intsWithSequence
    println(intsWithSequenceTest + ": ints with sequence")
  }
  run
}
