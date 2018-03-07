package fpinscala.laziness

import fpinscala.SimpleBooleanTest
import fpinscala.laziness.Stream._

object StreamTest extends App with SimpleBooleanTest {

  def run = {
    val stream1 = Stream(1, 2, 3)
    val stream2 = Stream(1, 2, 3, 4, 5, 6, 7)

    val toListTest = stream1.toList == List(1, 2, 3)
    println(toListTest + ": toList")

    val takeTest = stream1.take(2).toList == List(1, 2) && stream1.take(5).toList == stream1.toList
    println(takeTest + ": take")

    val dropTest = stream1.drop(2).toList == List(3) && stream1.drop(5).toList == List.empty
    println(dropTest + ": drop")

    val takeWhileTest = stream2.takeWhile(_ < 4).toList == List(1, 2, 3) && stream2.takeWhile(_ < 10).toList == stream2.toList
    println(takeWhileTest + ": takeWhile")

    val forAllTest = stream1.forAll(_ => true) && !stream1.forAll(_ % 2 == 0)
    println(forAllTest + ": forAll")

    val takeWhileWithFoldRightTest = stream2.takeWhileWithFoldRight(_ < 4).toList == stream2.takeWhile(_ < 4).toList && stream2.takeWhileWithFoldRight(_ < 10).toList == stream2.toList
    println(takeWhileWithFoldRightTest + ": takeWhile with foldRight")

    val headOptionTest = stream1.headOption == Some(1) && empty.headOption == None
    println(headOptionTest + ": headOption")

    val mapTest = stream1.map(_ * 2).toList == List(2, 4, 6)
    println(mapTest + ": map")

    val filterTest = stream1.filter(_ % 2 == 0).toList == List(2) && empty[Int].filter(_ % 2 == 0).toList == List.empty
    println(filterTest + ": filter")

    val appendTest = stream1.append(stream2).toList == List(1, 2, 3, 1, 2, 3, 4, 5, 6, 7)
    println(appendTest + ": append")

    val flatMapTest = stream1.flatMap(n => Stream(n, n, n)).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3) && empty[Int].flatMap(Stream(_)).toList == List.empty
    println(flatMapTest + ": flatMap")

    val constantTest = ones.take(5).toList == constant(1).take(5).toList
    println(constantTest + ": constant")

    val fromTest = from(3).take(3).toList == List(3, 4, 5)
    println(fromTest + ": from")

    val fibsTest = fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8)
    println(fibsTest + ": fibs")

    val unfoldTest = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }.take(7).toList == List(0, 1, 1, 2, 3, 5, 8)
    println(unfoldTest + ": unfold")

    val fibsWithUnfoldTest = fibs.take(100).toList == fibsWithUnfold.take(100).toList
    println(fibsWithUnfoldTest + ": fibs with unfold")

    val fromWithUnfoldTest = fromWithUnfold(3).take(100).toList == from(3).take(100).toList
    println(fromWithUnfoldTest + ": from with unfold")

    val constantWithUnfoldTest = constantWithUnfold("x").take(100).toList == constant("x").take(100).toList
    println(constantWithUnfoldTest + ": constant with unfold")

    val onesWithUnfoldTest = onesWithUnfold.take(100).toList == ones.take(100).toList
    println(onesWithUnfoldTest + ": ones with unfold")

    val startsWithTest = stream2.startsWith(stream1) && !stream1.startsWith(stream2)
    println(startsWithTest + ": startsWith")

    val mapWithUnfoldTest = stream1.mapWithUnfold(_ * 2).toList == List(2, 4, 6)
    println(mapWithUnfoldTest + ": map with unfold")

    val takeWithUnfoldTest = stream2.takeWithUnfold(4).toList == List(1,2,3,4)
    println(takeWithUnfoldTest + ": take with unfold")

    val takeWhileWithUnfoldTest = stream2.takeWhileWithUnfold(_ < 5).toList == List(1,2,3,4)
    println(takeWhileWithUnfoldTest + ": takeWhile with unfold")

    val zipAllTest = stream1.zipAll(stream2).toList == List(
      (Some(1), Some(1)),
      (Some(2), Some(2)),
      (Some(3), Some(3)),
      (None,    Some(4)),
      (None,    Some(5)),
      (None,    Some(6)),
      (None,    Some(7))
    )
    println(zipAllTest + ": zipAll")

    val tailsTest = stream1.tails.map(_.toList).toList == List(List(1,2,3), List(2,3), List(3))
    println(tailsTest + ": tails")

    val hasSubseqTest = stream2.hasSubsequence(stream1) && !stream1.hasSubsequence(stream2)
    println(hasSubseqTest + ": hasSubsequence")

    val scanRightTest = stream1.scanRight(0)((a,b) => {/*println(s"$a+$b"); */a+b}).toList == List(6,5,3,0)
    println(scanRightTest + ": scanRight")

    val scanRightTest2 = stream1.scanRight2(0)((a,b) => {/*println(s"$a+$b"); */a+b}).toList == List(6,5,3,0)
    println(scanRightTest2 + ": scanRight2")
  }

  run
}
