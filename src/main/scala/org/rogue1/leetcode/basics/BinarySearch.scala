package org.rogue1.leetcode.basics

object BinarySearch {

  /**
   * the recursive call should employ only mid - 1 and mid + 1 because that is a way to avoid infinite loops
   * @param arr
   * @param start
   * @param end
   * @param num
   * @return
   */
  @scala.annotation.tailrec
  def search(arr: Array[Int], start: Int, end: Int, num: Int): Boolean  = {
    if (end < start) {
      val mid = start + ((end - start)/2)
      if (arr(mid) == num) { true }
      else if (num < arr(mid)) { search(arr, start, mid - 1, num) }
      else search(arr, mid + 1 , end, num)
    } else {
      false
    }
  }

   /*
    * if index(parent) = N, index(left child) = 2*N+1
    * if index(parent) = N, index(right child) = 2*N+2
    * if index(child) = N, index(parent) = (N-1)/2 (integer division with truncation)
    */
  def arrayReprOfBST(): Unit = {

  }

}
