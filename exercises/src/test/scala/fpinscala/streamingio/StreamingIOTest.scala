package fpinscala.streamingio

import fpinscala.SimpleBooleanTest
import fpinscala.streamingio.SimpleStreamTransducers.Process

object StreamingIOTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val takeTest = Process.take(4)(Stream(1,2,3,4,5,6,7,8,9)).toList == List(1,2,3,4)
    printTest(takeTest, "take")

    val dropTest = Process.drop(3)(Stream(1,2,3,4,5,6,7)).toList == List(4,5,6,7)
    printTest(dropTest, "drop")

    val takeWhileTest = Process.takeWhile[Int](_%2 == 0)(Stream(2,4,6,7,8,9)).toList == List(2,4,6)
    printTest(takeWhileTest, "takeWhile")

    val dropWhileTest = Process.dropWhile[Int](_%2 == 0)(Stream(2,4,6,7,8,9)).toList == List(7,8,9)
    printTest(dropWhileTest, "dropWhile")

    val countTest = Process.count(Stream(1,1,1,1,1,1,1)).toList == List(1,2,3,4,5,6,7)
    printTest(countTest, "count")

    val meanTest = Process.mean(Stream(2.0,4.0,6.0,8.0)).toList == List(2.0,3.0,4.0,5.0)
    printTest(meanTest, "mean")

    val sum2Test = Process.sum2(Stream(1.0,2.0,3.0)).toList == List(1.0,3.0,6.0)
    printTest(sum2Test, "sum with loop")

    val count3Test = Process.count(Stream(1,1,1,1,1,1,1)).toList == List(1,2,3,4,5,6,7)
    printTest(count3Test, "count with loop")
  }
  run
}
