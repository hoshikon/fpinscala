package fpinscala.localeffects

import fpinscala.SimpleBooleanTest

object LocalEffectTest extends App with SimpleBooleanTest {
  def run: Unit = {
    val quickSortTest = Immutable.quicksort(List(2,5,1,8,3)) == List(1,2,3,5,8)
    printTest(quickSortTest, "immutable quicksort")
  }

  run
}
