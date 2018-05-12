package fpinscala.datastructures

import fpinscala.SimpleBooleanTest

object ListTest extends App with SimpleBooleanTest{
  import List._

  def run = {
    val reverseTest = reverse(List(1, 2, 3)) == List(3, 2, 1)
    printTest(reverseTest, "reverse")

    val appendWithFoldRightTest = appendWithFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)
    printTest(appendWithFoldRightTest, "append with foldright")

    val (a, b, c) = (List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    val flattenTest = flatten(List(a, b, c)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    printTest(flattenTest, "flatten")

    val addOneTest = addOne(List(0, 1, 2)) == List(1, 2, 3)
    printTest(addOneTest, "add one")

    val doubleToStringTest = doubleToString(List(1.1, 1.2, 1.3)) == List("1.1", "1.2", "1.3")
    printTest(doubleToStringTest, "double to string")

    val mapTest = map(List(1, 2, 3))(_ * 2) == List(2, 4, 6)
    printTest(mapTest, "map")

    val filterTest = filter(List(1, 2, 3))(_ % 2 == 0) == List(2)
    printTest(filterTest, "filter")

    val flatMapTest = flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)
    printTest(flatMapTest, "flatmap")

    val filterWithFlatMapTest = filterWithFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4)
    printTest(filterWithFlatMapTest, "filter with flatmap")

    val zipIntTest = zipInt(List(1, 2, 3), List(4, 5, 6, 7)) == List(5, 7, 9, 7)
    printTest(zipIntTest, "zip list of ints")

    val zipWithTest = zipWith(List(1, 2, 3), List(4, 5, 6, 7))(_ * _) == List(4, 10, 18, 7)
    printTest(zipWithTest, "zip with given function")

    val hasSubseqTest = hasSubsequence(List("a", "b", "c", "d", "e", "f"), List("d", "e"))
    printTest(hasSubseqTest, "has subsequences")
  }
}
