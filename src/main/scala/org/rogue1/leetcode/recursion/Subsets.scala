package org.rogue1.leetcode.recursion


object Subsets {
  /**
   * This is a really good explanation of this problem.
   * https://www.youtube.com/watch?v=bGC2fNALbNU
   *
   * visualize this as basically building a tree with root node as empty subset.
   * for each num in the original subset
   *
   * @param nums
   * @return
   */
  def subsets(nums: Array[Int]): List[List[Int]] = {
    @scala.annotation.tailrec
    def subset(list: List[List[Int]], data: List[Int]): List[List[Int]] = {
      data match {
        case Nil => list
        case head :: tail => subset(list.flatMap(x => List(head :: x, x)), tail)
      }
    }
    subset(List(Nil), nums.toList)
  }



}
