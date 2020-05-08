package org.rogue1.leetcode.sorting

/**
 * https://leetcode.com/problems/search-in-rotated-sorted-array/
 *
 * important points:
 * this is a sorted array that has been shifted or rotated without us knowing where the original start index is located.
 *
 * so this is function can be decomposed into 2 steps.
 *  * find the pivot location (location of the smallest number in the array)
 *  * break the array into two smaller array on the pivot location and run binary search on each of them.
 *
 * The entire process must be completed in log(n). so the real trick in here is that both the two operations
 * pivot location and binary search must happen in log(n).
 *
 */
object RotatedSortedArraySearch {

  object Solution {
    def search(nums: Array[Int], target: Int): Int = {
      if (nums.length == 0 || nums == null) return -1
      val pivot = findPivot(nums, 0, nums.length)
      val res1 = binarySearch(nums, 0, pivot-1, target)
      if (res1 != -1)  { res1 } else binarySearch(nums, pivot, nums.length-1, target)
    }


    @scala.annotation.tailrec
    def findPivot(nums: Array[Int], start: Int, end: Int): Int = {
      val mid = start + (end - start)/2
      if (start+1 == end) {
        return end
      }
      if (nums(start) > nums(mid)) {
        findPivot(nums, start, mid)
      } else {
        findPivot(nums, mid, end)
      }
    }

    @scala.annotation.tailrec
    def binarySearch(nums: Array[Int], start: Int, end: Int, search: Int): Int = {
      println(s"start $start end $end")
      val mid = start + (end - start)/2
      if (end < start) { -1 }
      else if (end == start) { if (nums(start) == search) start else -1 }
      else if (nums(mid) ==  search) { mid }
      else if (search < nums(mid)) { binarySearch(nums, start, mid - 1, search) }
      else { binarySearch(nums, mid + 1, end, search) }
    }
  }

}
