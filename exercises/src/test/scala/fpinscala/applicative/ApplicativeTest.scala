package fpinscala.applicative

import fpinscala.SimpleBooleanTest
import fpinscala.applicative.Applicative.streamApplicative

object ApplicativeTest extends App with SimpleBooleanTest {
  override def run: Unit = {

    val listOfStreams = List(Stream.continually(1), Stream.continually(2), Stream.continually(3))
    val streamOfList: Stream[List[Int]] = streamApplicative.sequence(listOfStreams)
    val streamApplicativeSequenceTest = streamOfList.take(100).forall(_ == List(1,2,3))

    printTest(streamApplicativeSequenceTest, "streamApplicative sequence")
  }

  run
}
