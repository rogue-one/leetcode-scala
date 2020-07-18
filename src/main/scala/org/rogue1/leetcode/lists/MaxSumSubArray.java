package org.rogue1.leetcode.lists;


/**
 * https://leetcode.com/problems/maximum-subarray/submissions/
 *
 * This is greedy solution.
 * currSum at i is the max possible sum in all possible sub-arrays from index 0 to i.
 * currSum at point i is calculated as
 *      max of
 *          1. currSum @ [i-1] + value @ [i]
 *          2. value @ [i]
 * at each point we decide whether should we include 0 - i-1 subarray or drop it.
 */
public class MaxSumSubArray {
    public int maxSubArray(int[] nums) {
        int currSum = nums[0], maxSum = nums[0];
        for (int i=0; i <= nums.length-1; i++) {
            currSum = Math.max(currSum+nums[i], nums[i]);
            maxSum = Math.max(maxSum, currSum);
        }
        return maxSum;
    }
}
