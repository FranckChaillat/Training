package heapsort

import scala.collection.mutable
import scala.util.Random

object HeapSort {


  def main(args: Array[String]): Unit = {
     val rnd = new Random()
     val arr = 0.to(100000).map(_ => rnd.nextInt(1000000)).toArray
     val pq = heapify(Array(), arr, arr.length - 1)

    //val pq =  mutable.PriorityQueue[Int](arr: _*)
    pq
  }


  private def getParentIndex(i: Int) = {
    if(i % 2 == 0) {
      Math.floor((i + 1) / 2).toInt - 1
    } else {
      Math.floor(i/ 2).toInt
    }
  }

  def getGreaterChildIndex(currentIndex: Int, arr: Array[Int]) : Option[Int] = {
    val leftPos = currentIndex * 2
    val rightPos = currentIndex * 2 + 1

    (arr.lift(leftPos), arr.lift(rightPos)) match {
      case (Some(l), Some(r)) => Some(if(l > r) leftPos else rightPos)
      case (Some(l), None) => Some(leftPos)
      case (None, Some(r)) => Some(rightPos)
      case _ => None
    }
  }

  private def heapSort(arr: Array[Int], acc: Array[Int]) = {
    val swapped = arr.updated(arr.length - 1, arr.head)
    swapped.last
    //swap
    val r = getGreaterChildIndex(0, arr)
      .map(index => {
        arr.updated(0, arr(index)).updated(index, arr(0))
      }).getOrElse(arr)
  }

  //TODO: Optimize
  private def heapify(acc: Array[Int], arr: Array[Int], currentIndex: Int): Array[Int] = {
    arr.lift(currentIndex) match {
      case Some(currentValue) if currentIndex == 0 =>
        acc.+:(currentValue)
      case Some(currentValue) =>
        val parentIndex = getParentIndex(currentIndex)
        val parentValue = arr(parentIndex)
        val newAcc = acc.+:(if (currentValue > parentValue) parentValue else currentValue)
        newAcc.headOption match {
          case Some(`currentValue`) =>
            heapify(newAcc, arr, currentIndex - 1)
          case _ =>
            val newArr = arr.updated(parentIndex, currentValue)
                .updated(currentIndex, parentValue)
            heapify(newAcc, newArr, currentIndex - 1)
        }
      case None => acc
    }
  }
}
