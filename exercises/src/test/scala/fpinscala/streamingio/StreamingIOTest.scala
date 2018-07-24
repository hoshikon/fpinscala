package fpinscala.streamingio

import java.util.concurrent.{ExecutorService, Executors}
import java.io.File

import fpinscala.SimpleBooleanTest
import fpinscala.iomonad.unsafePerformIO
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

    val composeTest1 = (Process.sum |> Process.sum)(Stream(1,2,3,4,5,6,7,8)).toList == List(1,4,10,20,35,56,84,120)
    printTest(composeTest1, "sum |> sum")

    val composeTest2 = (Process.filter[Double](_%2 == 0) |> Process.sum)(Stream(1,2,3,4,5,6,7,8)).toList == List(2,6,12,20)
    printTest(composeTest2, "filter |> sum")

    val composeTest3 = (Process.sum |> Process.filter(_.toInt%2 == 0))(Stream(1,2,3,4,5,6,7,8)).toList == List(6,10,28,36)
    printTest(composeTest3, "sum |> filter")

    val zipWithIndexTest = Process.lift[Int,Int](identity).zipWithIndex(Stream(0,1,2,3,4)).toList == List((0,0), (1,1), (2,2), (3,3), (4,4))
    printTest(zipWithIndexTest, "zipWithIndex")

    val zipTest = Process.lift[Double,Double](identity).zip(Process.sum)(Stream(1,2,3,4,5)).toList == List((1,1), (2,3), (3,6), (4,10), (5,15))
    printTest(zipTest, "zip")

    val mean2Test = Process.mean2(Stream(2.0,4.0,6.0,8.0)).toList == List(2.0,3.0,4.0,5.0)
    printTest(meanTest, "mean with zip")

    val existsTest = Process.my_exists[Int](_%2==0)(Stream(1,3,5,7,9,10)).toList == List(false, false, false, false, false, true)
    printTest(existsTest, "exists")

    val path = "./exercises/src/test/resources"
    val (from, to) = (s"$path/read-from.txt", s"$path/write-to.txt")
    val pool: ExecutorService = Executors.newCachedThreadPool()
    unsafePerformIO(
      Process.fromFahrenheitFileToCelsiusFile(
        new File(from),
        new File(to)
      ))(pool)

    val fileReadWriteTest = {
      val result = unsafePerformIO(
        Process.processFile(new File(to), Process.lift(identity), "")((acc, str) => acc + str + "\n")
      )(pool)
      val expected = Seq(100.0, 120.0, 72.4, 151.51, 48.0, 163.12)
        .map(Process.toCelsius(_).toString)
        .mkString("\n") + "\n"

      result == expected
    }
    printTest(fileReadWriteTest, "read fahrenheit, convert to celsius, and write to file")

    pool.shutdown()
  }
  run
}
