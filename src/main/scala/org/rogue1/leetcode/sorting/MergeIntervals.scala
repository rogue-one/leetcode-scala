package org.rogue1.leetcode.sorting

import sun.jvm.hotspot.gc_interface.CollectedHeap

import scala.collection.mutable

object MergeIntervals {

  private val heap = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    if (intervals.isEmpty) {
      intervals
    } else {
      val li = intervals.map({ case Array(x,y) => List(x,y) })
      li.tail
        .foldLeft(List(li.head))({ case (acc, x) => acc.flatMap(y => doMerge(x, y)) })
        .map({ case List(x,y) => Array(x,y) })
        .toArray
    }
  }

  def doMerge(arr1: List[Int], arr2: List[Int]):  Set[List[Int]] = {
    (arr1, arr2) match {
      case (List(x1, x2), List(y1, y2)) if ((x1 to x2).toSet & (y1 to y2).toSet).nonEmpty =>
        Set(List(Math.min(x1, y1), Math.max(x2, y2)))
      case (List(x1, x2), List(y1, y2)) => Set(List(x1,x2), List(y1,y2))
    }
  }


}
