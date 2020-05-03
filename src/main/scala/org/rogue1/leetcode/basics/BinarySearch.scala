package org.rogue1.leetcode.basics

object BinarySearch {

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

}
