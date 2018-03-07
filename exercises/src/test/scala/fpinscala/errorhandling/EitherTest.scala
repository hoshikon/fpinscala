package fpinscala.errorhandling

import fpinscala.SimpleBooleanTest

object EitherTest extends App with SimpleBooleanTest {
  import Either._

  def run = {
    val left = Left("oh no")

    val mapTest = Right(1).map(_ * 2) == Right(2) && left.map((n: Int) => n * 2) == left
    println(mapTest + ": map")

    val flatMapTest = Right(1).flatMap(n => Right(n * 2)) == Right(2) && left.flatMap((n: Int) => Right(n * 2)) == left
    println(flatMapTest + ": flatMap")

    val orElseTest = Right(2).orElse(Right(1)) == Right(2) && left.orElse(Right(3)) == Right(3)
    println(orElseTest + ": orElse")

    val map2TestRR = Right(2).map2(Right("2"))((n, str) => n + str.toInt) == Right(4)
    val map2TestLR = left.map2(Right(1))((x: Int, y: Int) => x + y) == left
    val map2TestRL = Right(3).map2(left)((x, y: Int) => x + y) == left
    val map2TestLL = left.map2(Left("oh wow"))((x: Int, y: Int) => x + y) == left
    val map2Test = map2TestRR && map2TestLR && map2TestRL && map2TestLL
    println(map2Test + ": map2")

    val traverseTest = traverse(List(1, 2))(n => Right(n.toString)) == Right(List("1", "2")) && traverse(List(1, 2))(n => Left("oops")) == Left("oops")
    println(traverseTest + ": traverse")

    val sequenceTest = sequence(List(Right(1), Right(3))) == Right(List(1, 3)) && sequence(List(Right(1), Left("oh no"), Left("oh man"))) == Left("oh no")
    println(sequenceTest + ": sequence")
  }
}