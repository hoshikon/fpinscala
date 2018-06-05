package fpinscala.applicative

import fpinscala.SimpleBooleanTest
import fpinscala.applicative.Applicative._
import fpinscala.applicative.Monad._
import fpinscala.applicative.Traverse._

object ApplicativeTest extends App with SimpleBooleanTest {
  override def run: Unit = {

    val listOfStreams = List(Stream.continually(1), Stream.continually(2), Stream.continually(3))
    val streamOfList: Stream[List[Int]] = streamApplicative.sequence(listOfStreams)
    val streamApplicativeSequenceTest = streamOfList.take(100).forall(_ == List(1,2,3))

    printTest(streamApplicativeSequenceTest, "streamApplicative sequence")

    val validationApplicativeTest =
      validationApplicative.map4[Int, Int, Int, Int, Int](
        Failure("fail1", Vector.empty),
        Success(1),
        Failure("fail2", Vector.empty),
        Failure("fail3", Vector.empty)
      )(_+_+_+_) == Failure("fail3", Vector("fail2", "fail1"))
    printTest(validationApplicativeTest, "validation applicative")

    val sequenceMapTest = validationApplicative.sequenceMap(Map(1 -> Success("one"), 2 -> Success("two"), 3 -> Success("three"))) == Success(Map(1 -> "one", 2 -> "two", 3 -> "three"))
    printTest(sequenceMapTest, "sequenceMap")

    val traverseSequenceTest = listTraverse.sequence(List[Option[Int]](Some(1), Some(2), Some(3)))(optionMonad) == Some(List(1,2,3))
    printTest(traverseSequenceTest, "traverse.sequence")

    val traverseReverseTest = listTraverse.reverse(List(1,2,3)) == List(3,2,1)
    printTest(traverseReverseTest, "traverse.reverse")

    val traverseFoldLeftTest = listTraverse.foldLeft(List("a","b","c"))("")(_+_) == "abc"
    printTest(traverseFoldLeftTest, "traverse.foldLeft")

    type EI[A] = Either[Nothing, A]
    val traverseFuseTest = listTraverse.fuse[Option, EI, Int, String](List(1,2,3))(a => Some(a.toString), a => Right(a.toString))(optionMonad, eitherMonad) == (Some(List("1", "2", "3")), Right(List("1", "2", "3")))
    printTest(traverseFuseTest, "traverse.fuse")

    val traverseComposeTest = listTraverse.compose(optionTraverse).traverse[EI,Int,String](List(Some(1)))(n => Right(n.toString))(eitherMonad) == Right(List(Some("1")))
    printTest(traverseComposeTest, "traverse.compose")

    val monadComposeMTest = Monad.composeM(optionMonad, listMonad, listTraverse).flatMap(Some(List(1,2,3)))(n => Some(List(n, n))) == Some(List(1,1,2,2,3,3))
    printTest(monadComposeMTest, "monad.composeM")

  }

  run
}
