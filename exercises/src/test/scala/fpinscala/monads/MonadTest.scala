package fpinscala.monads

import com.sun.xml.internal.xsom.impl.scd.Axis.ModelGroupAxis
import fpinscala.SimpleBooleanTest

object MonadTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val sequenceTest = Monad.optionMonad.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3))
    printTest(sequenceTest, "sequence")

    val traverseTest = Monad.optionMonad.traverse(List(1,2,3))(n => Some(n)) == Some(List(1,2,3))
    printTest(traverseTest, "traverse")

    val replicateMTest = Monad.optionMonad.replicateM(3, Some(1)) == Some(List.fill(3)(1))
    printTest(replicateMTest, "replicateM")

    val filterMTest = Monad.optionMonad.filterM(List(1,2,3))(n => Some(n%2 == 0)) == Some(List(2))
    printTest(filterMTest, "filterM")

    val _flatMapTest = Monad.optionMonad._flatMap(Some(1))(n => Some(n*2)) == Some(2)
    printTest(_flatMapTest, "flatMap in terms of compose")

    val joinTest = Monad.optionMonad.join(Some(Some(1))) == Some(1)
    printTest(joinTest, "join")
  }

  run
}
