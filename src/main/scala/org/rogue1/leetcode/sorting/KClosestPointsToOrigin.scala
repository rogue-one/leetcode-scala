package org.rogue1.leetcode.sorting

import scala.collection.mutable

object KClosestPointsToOrigin {

  val customOrdering: Ordering[Array[Int]] = new Ordering[Array[Int]] {
    override def compare(x: Array[Int], y: Array[Int]): Int = {
      val Array(x1, y1) = x
      val Array(x2, y2) = y
      (Math.sqrt(Math.pow(x2, 2) + Math.pow(y2, 2)) * 100 - Math.sqrt(Math.pow(x1, 2) + Math.pow(y1, 2)) * 100).toInt
    }
  }

  def kClosest(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val heap = new mutable.PriorityQueue[Array[Int]]()(customOrdering)
    points.foreach(heap.addOne)
    (1 to K).map(_ => heap.dequeue()).toArray
  }

}
