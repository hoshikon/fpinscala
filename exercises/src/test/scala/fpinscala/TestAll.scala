package fpinscala

import fpinscala.datastructures.{ListTest, TreeTest}
import fpinscala.errorhandling.{EitherTest, OptionTest}
import fpinscala.gettingstarted.GettingStartedTest
import fpinscala.laziness.StreamTest
import fpinscala.parallelism.{NonblockingTest, ParTest}
import fpinscala.state.StateTest
import fpinscala.testing.GenTest

object TestAll extends App {
  val tests: List[SimpleBooleanTest] = List(
    GettingStartedTest,
    ListTest,
    TreeTest,
    OptionTest,
    EitherTest,
    StreamTest,
    StateTest,
    ParTest,
    NonblockingTest,
    GenTest
  )

  tests.foreach(t => {
    println(s"********** ${t.getClass.getPackage.getName} **********")
    t.run
  })
}

trait SimpleBooleanTest {
  def run: Unit
  def println(s: String) = {
    val colour = if (s.startsWith("true")) Console.GREEN else if (s.startsWith("false")) Console.RED else Console.YELLOW
    Predef.println(colour + s + Console.RESET)
  }
}
