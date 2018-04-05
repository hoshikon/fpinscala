package fpinscala.testing

import fpinscala.SimpleBooleanTest
import fpinscala.state.RNG
import fpinscala.state.RNG.Simple

object GenTest extends App with SimpleBooleanTest {

  private def tryNTimes[A](n: Int, g: Gen[A], rng: RNG)(f: A => Boolean): Boolean =
    (0 to n).foldLeft((true, rng))((acc, _) => {
      val (bool, r) = acc
      val (a, r2) = g.sample.run(r)
      (bool && f(a), r2)
    })._1

  override def run: Unit = {
//    val intList = Gen.listOf(Gen.choose(0, 100))
//    val propReverse = forAll(intList) { ns =>
//      sum(ns.reverse.reverse) == sum(ns)
//    }
//
//    val randInt = Gen.choose(0, 100)
//    val intListSameValue = Gen.listOf(randInt)
//    val propSameValue = forAll(intListSameValue) { ns =>
//      sum(ns) == ns.head * ns.length
//    }
//
//    val propMax = forAll(intList)(ns => {
//      val max = max(ns)
//      ns.forAll(_ <= max)
//    })
    implicit val rng = Simple(1)

    val unitTest = Gen.unit(1).value == 1
    println(unitTest + ": unit")

    val chooseTest = tryNTimes(100, Gen.choose(10, 15), rng)(n => n >= 10 && n < 15)
    println(chooseTest + ": choose")

    val listOfNTest = Gen.listOfN(100, Gen.choose(10, 15)).value.forall(n => n >= 10 && n < 15)
    println(listOfNTest + ": list of n")

    val ints = Gen.listOfN(10, Gen.int).value
    println(true + ": int | output => " + ints)

    val booleans = Gen.listOfN(10, Gen.boolean).value
    println(true + ": boolean | output => " + booleans)

    val int = Gen.int
    val toOptionTest = Gen.toOption(int).value == Some(int.value)
    println(toOptionTest + ": toOption")

    val fromOptionTest = Gen.fromOption(Gen.unit[Option[Int]](Some(2))).value == 2
    println(fromOptionTest + ": fromOption")

    val strings = Gen.listOfN(10, Gen.string(3)).value
    println(true + ": string | output => " + strings)

    val flatMapTest = Gen.unit(3).flatMap(n => Gen.unit(n * n)).value == 9
    println(flatMapTest + ": flatMap")

    val listOfGenNTest = Gen.unit(1).listOfN(Gen.unit(3)).value == List(1, 1, 1)
    println(listOfGenNTest + ": list of n with Gen[Int]")

    val unions = Gen.listOfN(10, Gen.union(Gen.unit(1), Gen.unit(2))).value
    println(true + ": union | output => " + unions)
  }

  run
}
