package fpinscala.errorhandling

import fpinscala.SimpleBooleanTest

object OptionTest extends App with SimpleBooleanTest {

  def run = {
    val mapTest = Some(1).map(_ * 2) == Some(2) && None.map((n: Int) => n * 2) == None
    printTest(mapTest, "map")

    val getOrElseTest = Some(1).getOrElse(0) == 1 && None.getOrElse("x") == "x"
    printTest(getOrElseTest, "getOrElse")

    val flatMapTest = Some(2).flatMap(n => Some(n * 2)) == Some(4) && None.flatMap((n: Int) => Some(n * 2)) == None
    printTest(flatMapTest, "flatMap")

    val orElseTest = Some(2).orElse(Some(4)) == Some(2) && None.orElse(Some(1)) == Some(1)
    printTest(orElseTest, "orElse")

    val filterTest = Some(3).filter(_ % 2 == 0) == None && None.filter((n: Int) => n % 2 == 0) == None
    printTest(filterTest, "filter")

    val varianceTest = Option.variance(Seq(1.0)) == Some(0.0)
    printTest(varianceTest, "variance")

    val map2Test = Option.map2(Some(1), Some(2))((a, b) => a + b) == Some(3) && Option.map2(None, Some(4))((a: Int, b: Int) => a + b) == None
    printTest(map2Test, "map2")

    val sequenceTest = Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)) && Option.sequence(List(Some(4), None)) == None
    printTest(sequenceTest, "sequence")

    val traverseTest = Option.traverse(List(1, 2, 3))(n => Some(n.toString)) == Some(List("1", "2", "3")) && Option.traverse(List(1, 2, 3))(_ => None) == None
    printTest(traverseTest, "traverse")
  }
}