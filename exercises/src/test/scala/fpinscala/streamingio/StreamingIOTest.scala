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
  }
  run
}
