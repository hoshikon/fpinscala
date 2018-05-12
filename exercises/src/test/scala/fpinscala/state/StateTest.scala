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
    printTest(nonNegativeIntTest, "non negative int")

    val doubles = createList(rng)(RNG.double)
    val doubles2 = createList(rng)(RNG.double)
    val doubleTest = doubles.forall(d => 0 <= d && d < 1) && doubles == doubles2
    printTest(doubleTest, "double")

    val intDoubles = createList(rng)(RNG.intDouble)
    val intDoubles2 = createList(rng)(RNG.intDouble)
    val intDoubleTest = intDoubles == intDoubles2
    printTest(intDoubleTest, "intDouble")

    val doubleInts = createList(rng)(RNG.doubleInt)
    val doubleInts2 = createList(rng)(RNG.doubleInt)
    val doubleIntTest = doubleInts == doubleInts2
    printTest(doubleIntTest, "doubleInt")

    val double3s = createList(rng)(RNG.double3)
    val double3s2 = createList(rng)(RNG.double3)
    val double3Test = double3s == double3s2
    printTest(double3Test, "double3")

    val ints1 = RNG.ints(1000)(rng)._1
    val ints2 = RNG.ints(1000)(rng)._1
    val ints3 = createList(rng)(_.nextInt).take(1000)
    val intsTest = ints1 == ints2 && ints2 == ints3
    printTest(intsTest, "ints")

    val anotherDoubles = createList(rng)(RNG.doubleWithMap)
    val anotherDoubles2 = createList(rng)(RNG.doubleWithMap)
    val doubleWithMapTest = anotherDoubles.forall(d => 0 <= d && d < 1) && anotherDoubles == anotherDoubles2
    printTest(doubleWithMapTest, "double with map")

    val intDoublesWithMap2 = createList(rng)(RNG.map2(RNG.int, RNG.double)((_,_)))
    val map2Test = intDoubles == intDoublesWithMap2
    printTest(map2Test, "map2")

    val sequence = RNG.sequence(List(RNG.int, r => RNG.nonNegativeInt(r)))(rng)
    val sequence2 = RNG.sequenceWithFoldRight(List(RNG.int, r => RNG.nonNegativeInt(r)))(rng)
    val (i, rngi) = RNG.int(rng)
    val (j, rngj) = RNG.nonNegativeInt(rngi)
    val sequenceTest = sequence == (List(i, j), rngj)
    val sequenceTest2 = sequence == sequence2
    printTest((sequenceTest && sequenceTest2), "sequence")

    val intsWithSequence: List[Int] = RNG.intsWithSequence(1000)(rng)._1
    val intsWithSequenceTest = ints1 == intsWithSequence
    printTest(intsWithSequenceTest, "ints with sequence")

    val nonNegativeLessThan1000 = createList(rng)(RNG.nonNegativeLessThan(1000))
    val nonNegativeLessThanTest = nonNegativeLessThan1000.forall(i => i >= 0 && i < 1000)
    printTest(nonNegativeLessThanTest, "non negative less than")

    val mapWithFlatMapTest = RNG.map(RNG.int)(_*2)(rng) == RNG.mapWithFlatMap(RNG.int)(_*2)(rng)
    printTest(mapWithFlatMapTest, "map with flatMap")

    val intDoublesWithMap2WithFlatMap = createList(rng)(RNG.map2WithFlatMap(RNG.int, RNG.double)((_,_)))
    val map2WithFlatMapTest = intDoublesWithMap2 == intDoublesWithMap2WithFlatMap
    printTest(map2WithFlatMapTest, "map2 with flatMap")

    val stateUnittest = State.unit[RNG, Int](2).run(rng)._1 == 2
    printTest(stateUnittest, "state unit")

    val stateMapTest = State(RNG.int).map(_.toString).run(rng)._1 == RNG.int(rng)._1.toString
    printTest(stateMapTest, "state map")

    val stateMap2Test = State(RNG.int).map2(State(RNG.double))(_.toString + _.toString).run(rng) == RNG.map2(RNG.int, RNG.double)(_.toString + _.toString)(rng)
    printTest(stateMap2Test, "state map2")

    val stateFlatMapTest = State(RNG.int).flatMap(i => State.unit(i.toDouble)).run(rng) == RNG.flatMap(RNG.int)(i => RNG.unit(i.toDouble))(rng)
    printTest(stateFlatMapTest, "state flatMap")

    val stateSeqTest = State.sequence(List(State(RNG.int), State(RNG.nonNegativeInt))).run(rng) == RNG.sequence(List(RNG.int, RNG.nonNegativeInt _))(rng)
    printTest(stateSeqTest, "state sequence")

    val machine = Machine(locked = true, 5, 10)
    val inputs = List(Coin, Turn, Turn, Coin, Turn, Coin, Coin, Turn, Turn)
    val ran = State.simulateMachine(inputs).run(machine)
    val machineTest = ran == ((13, 2), Machine(locked = true, 2, 13))
    printTest(machineTest, "candy dispenser")
  }
  run
}
