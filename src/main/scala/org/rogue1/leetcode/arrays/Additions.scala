package org.rogue1.leetcode.arrays

object Additions {

  /**
   * an input unsorted array is provided. find if sum of any two elements matches the target input parameter.
   * at-least one solution is guaranteed to exists. must return indexes of the elements
   */
  object TwoSumUnsorted {

    /**
     * normal nested for loop (combinations) approach. brute force approach.
     *
     * complexity: n squared
     *
     * @param numbers
     * @param target
     * @return
     */
    def twoSumUnSortedBruteForce(numbers: Array[Int], target: Int): Array[Int] = {
      val res = for {
        i <- numbers.indices.dropRight(1).iterator
        j <- ((i+1) until numbers.length) if numbers(i) + numbers(j) == target
      } yield Array(i+1,j+1)
      res.next()
    }

    /**
     * builds a HashMap of input array. for each number in the array check if complement number exists in the set.
     * so instead of a full table scan for each number we do only a one row look up.
     * @param numbers
     * @param target
     * @return
     */
    def twoSumUnSortedSetApproach(numbers: Array[Int], target: Int): Array[Int] = {
      import scala.collection.mutable
      val map = mutable.Map[Int, Int]()
      numbers.zipWithIndex.foreach({ case (x,y) => map(x) = y})
      val res = for {
        i <- numbers.indices.iterator
        j = map.get(target - numbers(i)) if j.nonEmpty
      } yield Array(i+1,j.get+1)
      res.next()
    }

    /**
     * we employ a strategy that is similar to BINARY SEARCH. but we have two pointers (ptr1, ptr2) instead of one.
     * we start at the mid point. but what is the mid point here for a two pointer approach?
     * ptr1 = START and ptr2 = START+1 is the low point. that is the lowest 2sum number you can generate.
     * ptr1 = END-1 and ptr2 = END is the high point. that is the highest 2 sum number you can generate.
     * ptr1 = START and ptr2 = END is the mid point.
     * now after the mid point if the resultant number matches target return the result.
     * else if number is lesser than target then we can increase ptr2 to increase our resultant value.
     * if it is lower we can reduce ptr2 to reduce our resultant value.
     *
     * complexity = N and not log n. because worst case ptr1 should reach ptr2 or vice versa which is O(N).
     *
     * THIS APPROACH IS COMMONLY CALLED AS TWO POINTER APPROACH.
     * @param numbers
     * @param target
     * @return
     */
    def twoSumSortedApproach(numbers: Array[Int], target: Int): Array[Int] = {
      var (ptr1, ptr2) = (0, numbers.length-1)
      while(ptr1 != ptr2) {
        val res = numbers(ptr1) + numbers(ptr2)
        println((ptr1, ptr2, res))
        if (res == target) {
           return Array(ptr1+1, ptr2+1)
        } else if (res < target) {
          ptr2 -= 1
        } else {
          ptr1 += 1
        }
      }
      Array(-1,-1)
    }


  }

  /**
   * tGiven an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0?
   * Find all unique triplets in the array which gives the sum of zero.
   *
   * Given array nums = [-1, 0, 1, 2, -1, -4],
   *
   * A solution set is:
   * [
   * [-1, 0, 1],
   * [-1, -1, 2]
   * ]
   */
  object ThreeSum {

    /**
     * 1. an extension of the 2sum hashmap solution.
     * 2. choose a number X which is <= 0.
     * 3. use 2sum function to find two other number in the array whose sum is -X (which makes the 3sum zero (intended  target))
     * 4. two conditions in the question makes possible an important optimization.
     *    4a) the triplets must be unique
     *    4b) the intended target is zero. (which means some at-least one number in the triplet must be <= zero)
     *    4c) because of these above conditions we can consider only X <= 0.
     * 5. The 2sum hashmap solution should have  HashMap<Int, List<Int>>. to store each value and the list of indicies
     *     the value is present in the array.
     * 6. you must make sure X is unique. calling the 2sum function for duplicate of X is unnecessary since we need only unique triplets.
     *    6a). the best way to accomplish #5 is use the keys in the Hash map which is already duplicated.
     * @param nums
     * @return
     */
    def threeSumHashMap(nums: Array[Int]): List[List[Int]] = {
      import scala.collection.mutable
      val map = mutable.HashMap[Int, List[Int]]()
      nums.zipWithIndex.foreach({ case (x, idx) => map(x) = idx :: map.getOrElse(x, Nil) })
      def twoSum(num1: Int, idx1: Int): List[List[Int]] = {
        val res = for {
          (num2, idx2) <- nums.iterator.zipWithIndex.collect({ case (x, idx) if idx != idx1 => x -> idx })
          num3 <- map.getOrElse(-num1 - num2, Nil).filterNot(List(idx1, idx2).contains(_)).map(nums).take(1)
        } yield {
          (num1 :: num2 :: num3 :: Nil).sorted
        }
        res.toList
      }
      map
        .flatMap({ case (key, value :: tail) => twoSum(key, value) case _ => Nil })
        .foldLeft(List[List[Int]]())({ case (acc, x) if ! acc.contains(x) => x :: acc case (acc, _) => acc })
    }


    def threeSum2Ptr(nums: Array[Int]): List[List[Int]] = {
      import scala.collection.mutable
      nums.sortInPlaceWith((x,y) => x < y)
      def twoSum(num1: Int, idx1: Int): List[List[Int]] = {
        println(s"$num1, $idx1")
        var (lo, hi) = ((idx1 + 1), nums.length - 1)
        var res = mutable.Set[List[Int]]()
        while(lo < hi) {
          val l1 = List(num1, nums(lo), nums(hi)).sorted
          val sum1 = num1 + nums(lo) + nums(hi)
          if((sum1 == 0) && !res(l1)) {
            res.add(l1)
          } else if (sum1 < 0) {
            lo += 1
          } else  {
            hi -= 1
          }
        }
        res.toList
      }
      for {
        (x1, idx1) <- nums.zipWithIndex.toList if (idx1 == 0 || nums(idx1) != nums(idx1-1))
        res <- twoSum(x1, idx1)
      } yield res
    }
  }





}
