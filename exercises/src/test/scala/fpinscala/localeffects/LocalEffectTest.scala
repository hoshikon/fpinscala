package fpinscala.localeffects

import fpinscala.SimpleBooleanTest

object LocalEffectTest extends App with SimpleBooleanTest {
  def run: Unit = {
    val quickSortTest = Immutable.quicksort(List(2,5,1,8,3)) == List(1,2,3,5,8)
    printTest(quickSortTest, "immutable quicksort")

    val stMapSwapTest = ST.runST(new RunnableST[Map[Int, String]] {
      override def apply[S]: ST[S, Map[Int, String]] = for {
        map <- STMap[S,Int,String](1 -> "a", 2 -> "b")
        _ <- map.swap(1,2)
        swapped <- map.freeze
      } yield swapped
    }) == Map(1 -> "b", 2 -> "a")

    printTest(stMapSwapTest, "STMap swap")
  }

  run
}
