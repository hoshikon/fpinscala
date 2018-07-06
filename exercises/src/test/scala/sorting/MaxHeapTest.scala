package sorting

object MaxHeapTest extends App {

  private val unsortedList = List(9,5,7,1,8,4,6)
  private val maxHeap = MaxHeap.from(unsortedList)

  if (maxHeap.value.sameElements(Array(9,8,7,1,5,4,6))) println("PASSED!")
  else println("FAILED: " + maxHeap.value.toList)
}


object MaxHeapSortingTest extends  App {
  private val unsortedList = List(9,5,7,1,8,4,6)
  private val maxHeap = MaxHeap.from(unsortedList)
  maxHeap.sort()

  if(maxHeap.value.sameElements(Array(1,4,5,6,7,8,9))) println("PASSED")
  else println("FAILED: " + maxHeap.value.toList)
}