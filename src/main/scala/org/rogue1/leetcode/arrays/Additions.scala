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












}
