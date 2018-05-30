package fpinscala.applicative

import fpinscala.SimpleBooleanTest
import fpinscala.applicative.Applicative._

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
  }

  run
}
