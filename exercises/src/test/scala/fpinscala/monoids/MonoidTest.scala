package fpinscala.monoids

import java.util.concurrent.Executors

import fpinscala.SimpleBooleanTest
import fpinscala.monoids.Monoid._
import fpinscala.parallelism.Nonblocking.Par

object MonoidTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val foldRightTest = Monoid.foldRight(List("a","b","c"))("|")((a, b) => a + b) == "abc|"
    printTest(foldRightTest, "foldRight")

    val foldLeftTest = Monoid.foldLeft(List("a","b","c"))("|")((b, a) => b + a) == "|abc"
    printTest(foldLeftTest, "foldLeft")

    val foldMapVTest = Monoid.foldMapV(Vector("a", "b", "c"), stringMonoid)(identity) == "abc"
    printTest(foldMapVTest, "foldMapV")

    val es = Executors.newFixedThreadPool(10)
    val vec = Vector("a", "b", "c", "d", "e", "f", "g")
    val parFoldMapTest = Par.run(es)(Monoid.parFoldMap(vec, stringMonoid)(identity)) == "abcdefg"
    printTest(parFoldMapTest, "parFoldMap")

    val orderedTest = ordered(Vector(1,2,3,4,5,6,7,8)) && !ordered(Vector(1,2,3,4,5,3))
    printTest(orderedTest, "ordered")

    es.shutdown()
  }

  run
}
