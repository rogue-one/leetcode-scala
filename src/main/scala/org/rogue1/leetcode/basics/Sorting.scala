package org.rogue1.leetcode.basics

object Sorting {

  /**
   * bubble sort is the easiest inline sorting algorithm
   * @param arr
   */
  def bubbleSort(arr: Array[Int]): Unit = {
    for {
      i <- arr.indices
      j <- i until (arr.length - 1) if arr(j) > arr(j+1)
    } {
      val tmp = arr(j)
      arr(j) = arr(j+1)
      arr(j+1) = tmp
    }
  }

  /**
   * non-inline functional merge sort
   * @param arr
   * @return
   */
  def funcMergeSort(nums: List[Int]): List[Int] = {
    def merge(l1: List[Int], l2: List[Int]): List[Int] = {
      (l1, l2) match {
        case (Nil, Nil) => Nil
        case (x :: tail, Nil) => x :: merge(tail, Nil)
        case (Nil, x :: tail) => x :: merge(Nil, tail)
        case (x :: tail1, y :: tail2) if y < x => y :: x :: merge(tail1, tail2)
        case (x :: tail1, y :: tail2) => x :: y :: merge(tail1, tail2)
      }
    }
    nums match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case x :: y :: Nil if y < x => y :: x :: Nil
      case x :: y :: Nil => x :: y :: Nil
      case x =>  x.splitAt(x.length/2) match { case (l,r ) => merge(funcMergeSort(l), funcMergeSort(r)) }
    }
  }

}
