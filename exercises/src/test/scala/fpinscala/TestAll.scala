package fpinscala

import fpinscala.datastructures.{ListTest, TreeTest}
import fpinscala.errorhandling.{EitherTest, OptionTest}
import fpinscala.gettingstarted.GettingStartedTest
import fpinscala.laziness.StreamTest
import fpinscala.parallelism.ParTest
import fpinscala.state.StateTest

object TestAll extends App {
  val tests: List[SimpleBooleanTest] = List(
    GettingStartedTest,
    ListTest,
    TreeTest,
    OptionTest,
    EitherTest,
    StreamTest,
    StateTest,
    ParTest
  )

  tests.foreach(t => {
    println(s"********** ${t.getClass.getPackage.getName} **********")
    t.run
  })
}

trait SimpleBooleanTest {
  def run: Unit
  def println(s: String) = {
    val colour = if (s.startsWith("true")) Console.GREEN else Console.RED
    Predef.println(colour + s + Console.RESET)
  }
}
