package fpinscala

import fpinscala.datastructures.{ListTest, TreeTest}
import fpinscala.errorhandling.{EitherTest, OptionTest}
import fpinscala.gettingstarted.GettingStartedTest
import fpinscala.iomonad.IOTest
import fpinscala.laziness.StreamTest
import fpinscala.localeffects.LocalEffectTest
import fpinscala.monads.MonadTest
import fpinscala.monoids.MonoidTest
import fpinscala.parallelism.{NonblockingTest, ParTest}
import fpinscala.parsing.ParsersTest
import fpinscala.state.StateTest
import fpinscala.streamingio.StreamingIOTest
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
    GenTest,
    ParsersTest,
    MonoidTest,
    MonadTest,
    IOTest,
    LocalEffectTest,
    StreamingIOTest
  )

  tests.foreach(t => {
    println(s"********** ${t.getClass.getPackage.getName} **********")
    t.run
  })
}

trait SimpleBooleanTest {
  def run: Unit
  def printTest(passed: Boolean, name: String) = {
    val colour = if (passed) Console.GREEN else Console.RED
    Predef.println(colour + passed + ": " + name + Console.RESET)
  }
}
