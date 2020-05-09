package org.rogue1.leetcode.sorting

import scala.collection.mutable

object TopKFrequentElements {

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    val cache = nums.foldLeft(Map[Int, Int]())({ case (acc, x) => acc + (x -> (acc.getOrElse(x, 0)+1)) })
    val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._2 - y._2
      }
    }
    val heap = new mutable.PriorityQueue[(Int, Int)]()(ordering)
    for { i <-  cache } {
      heap.enqueue(i)
      if (heap.size == k) {
        heap.dequeue()
      }
    }
    heap.map(_._1).toSeq.reverse.toArray
  }

  /**
   * Given a non-empty list of words, return the k most frequent elements.
   *
   * Your answer should be sorted by frequency from highest to lowest. If two words have the same frequency,
   * then the word with the lower alphabetical order comes first.
   *
   * Example 1:
   * Input: ["i", "love", "leetcode", "i", "love", "coding"], k = 2
   * Output: ["i", "love"]
   * Explanation: "i" and "love" are the two most frequent words.
   * Note that "i" comes before "love" due to a lower alphabetical order.
   * Example 2:
   * Input: ["the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is"], k = 4
   * Output: ["the", "is", "sunny", "day"]
   * Explanation: "the", "is", "sunny" and "day" are the four most frequent words,
   * with the number of occurrence being 4, 3, 2 and 1 respectively.
   * Note:
   * You may assume k is always valid, 1 ≤ k ≤ number of unique elements.
   * Input words contain only lowercase letters.
   * Follow up:
   * Try to solve it in O(n log k) time and O(n) extra space.
   */
  object TopKFrequentWords {

    def topKFrequent(words: Array[String], count: Int): List[String] = {
      val map = words.foldLeft(Map[String, Int]())({ case (acc,x) => acc + (x -> (acc.getOrElse(x,0)+1)) })
      val heap = new scala.collection.mutable.PriorityQueue[(String, Int)]()(
        Ordering.by[(String,Int), Int](x => x._2).orElse(Ordering.by[(String,Int), String](x => x._1).reverse)
      )
      map.foreach({case (k,v) => heap.enqueue(k -> v)})
      for (_ <- (1 to count).toList) yield { heap.dequeue._1 }
    }

  }



}
