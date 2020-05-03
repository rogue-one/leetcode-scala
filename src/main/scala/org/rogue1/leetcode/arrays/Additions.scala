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

  }












}
