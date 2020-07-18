package org.rogue1.leetcode.arrays


/**
 * in this problem we have a unsorted array of numbers. we have to calculate the sum of any sub-array.
 * the sub-array sum calculation is called repeatedly with the same input.
 *
 * Solution:
 *  build a clone of the array with same dimensions.
 *  calculate cumulative running sum for the length of the array.
 *  sub-array sum for arr(i,j) = csum-arr(j) - if (i == 0) 0 else csum-arr(i-1)
 *
 */
object NumArraySum {

  /**
   * Given an integer array nums, find the sum of the elements between indices i and j (i ≤ j), inclusive.
   *
   * Example:
   * Given nums = [-2, 0, 3, -5, 2, -1]
   *
   * sumRange(0, 2) -> 1
   * sumRange(2, 5) -> -1
   * sumRange(0, 5) -> -3
   * Note:
   * You may assume that the array does not change.
   * There are many calls to sumRange function.
   *
   * @param _nums
   */
  class NumArraySum1D(_nums: Array[Int]) {

    val arr = new Array[Int](_nums.length)

    for {
      i <- 0 until _nums.length
    } {
      arr(i) = _nums(i) + (if (i == 0) 0 else arr(i - 1))
    }

    /**
     * Your NumArray object will be instantiated and called as such:
     * var obj = new NumArray(nums)
     * var param_1 = obj.sumRange(i,j)
     */
    def sumRange(i: Int, j: Int): Int = {
      if (i == 0) arr(j) else arr(j) - arr(i - 1)
    }

  }

  /**
   * This two dimensional version of the one dimensional array sum problem.
   * @param _matrix
   */
  class NumMatrix(_matrix: Array[Array[Int]]) {

    private val arr: Array[Array[Int]] =  if (_matrix.length == 0) Array.ofDim[Int](0,0) else
      Array.ofDim[Int](_matrix.length, _matrix(0).length+1)

    for {
      r <- 0 until _matrix.length
      c <- 0 until _matrix(0).length
    } {
      arr(r)(c) = _matrix(r)(c) + (if (c == 0) 0 else arr(r)(c-1) )
    }


    def sumRegion(row1: Int,
                  col1: Int,
                  row2: Int,
                  col2: Int): Int = {
      var sum = 0
      for { i <- row1 to row2} sum += (arr(i)(col2) - (if (col1 == 0) 0 else arr(i)(col1-1)))
      return sum
    }

  }


}