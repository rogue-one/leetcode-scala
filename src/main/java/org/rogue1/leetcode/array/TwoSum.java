package org.rogue1.leetcode.array;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class TwoSum {

    public int[] twoSumUnSortedBruteForce(int[] nums,
                                          int target) {
        int[] res = new int[2];
        for(int i = 0; i <= nums.length-2; i++) {
            for(int j = i+1; j <= nums.length-1; j++) {
                if (nums[i] + nums[j] == target) {
                    res[0] = i;
                    res[1] = j;
                    return res;
                }
            }
        }
        return res;
    }

    public int[] hashMapApproach(int[] nums,
                                 int target) {
        int[] res = new int[2];
        HashMap<Integer, Integer> map = new HashMap<Integer, Integer>();
        for (int i = 0; i <= nums.length-1; i++) {
            map.put(nums[i], i);
        }
        for (int i = 0; i <= nums.length-1; i++) {
            var data = map.get(target - nums[i]);
            if (data != null && data != i) {
                res[0] = i;
                res[1] = data;
                Arrays.sort(res);
            }
        }
        return res;
    }


    public int[] twoPtrApproach(int[] nums,
                                int target) {
        int ptr1 = 0;
        int ptr2 = nums.length;
    }




}