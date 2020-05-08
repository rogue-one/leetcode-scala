package org.rogue1.leetcode.sorting

import scala.collection.mutable

object TopKFrequentElements {

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    val cache = nums.foldLeft(Map[Int, Int]())({ case (acc, x) => acc + (x -> (acc.getOrElse(x, 0)+1)) })
    val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._2 - y._2
      }
    }
    val heap = new mutable.PriorityQueue[(Int, Int)]()(ordering)
    for { i <-  cache } {
      heap.enqueue(i)
      if (heap.size == k) {
        heap.dequeue()
      }
    }
    heap.map(_._1).toSeq.reverse.toArray
  }


}
