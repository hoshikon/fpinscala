package sorting

import sorting.MaxHeap.{checkParent, indexOfParent}

import scala.annotation.tailrec

class MaxHeap(val value: Array[Int]) {
  def sort() : Unit = {
    if (value.length > 1) sort(value.length)
  }

  @tailrec
  private def sort(length: Int): Unit = {
    if (length == 2) {
      swap(0,1)
    } else {
      swap(0, length-1)
      for (i <- 1 until length - 1) {
        if(checkParent(value, i) < value(i)) {
          swap(indexOfParent(i), i)
        }
      }
      sort(length-1)
    }
  }

  private def swap(i: Int, j: Int): Unit = {
    val temp = value(i)
    value.update(i, value(j))
    value.update(j, temp)
  }
}

object MaxHeap {
  def from(list: List[Int]) : MaxHeap = {
    val acc = new Array[Int](list.length)
    acc.update(0, list.head)
    for (i  <- 1 until list.length) {
      if(checkParent(acc, i) < list(i)) switchParentWithMe(i, acc, list) else acc.update(i, list(i))
    }
    new MaxHeap(acc)
  }

  private def checkParent(arr: Array[Int], childPos: Int) : Int = arr(indexOfParent(childPos))

  private def indexOfParent(childPos: Int): Int =
    if (childPos % 2 == 0) (childPos - 2) / 2 else (childPos - 1) / 2

  private def switchParentWithMe(i: Int, acc: Array[Int], original: List[Int]): Unit = {
    val p = indexOfParent(i)
    acc.update(i, original(p))
    acc.update(p, original(i))
  }
}

