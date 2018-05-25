package fpinscala.monads

import fpinscala.SimpleBooleanTest

object MonadTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val sequenceTest = Monad.optionMonad.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3))
    printTest(sequenceTest, "sequence")

    val traverseTest = Monad.optionMonad.traverse(List(1,2,3))(n => Some(n)) == Some(List(1,2,3))
    printTest(traverseTest, "traverse")
  }

  run
}
