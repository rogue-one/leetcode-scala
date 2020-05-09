package org.rogue1.leetcode.dp

object LongestPalindrome {


  import scala.collection.mutable

  def longestPalindrome(s: String): String = {
    if (s.length <= 1) return s
    val cache = mutable.Map[(Int,Int), Boolean]()
    subStrings(s)
      .map({ case (x,y) => { println(x,y); (x,y)}})
      .filter({ case (x,y) => isPalindrome(s, x, y, cache)})
      .maxBy({case (x,y) => y - x}) match {
      case (x,y) => s.substring(x,y+1)
    }
  }


  def isPalindrome(str: String, start: Int, end: Int, cache: mutable.Map[(Int, Int), Boolean]): Boolean = {
    def query(start: Int, end: Int): Boolean = {
      if (end == start) { true }
      else if (end - start == 1) {
        cache.getOrElseUpdate(start -> end, str.charAt(end) == str.charAt(start))
      }
      else {
        val (s,e) = (start + 1, end - 1)
        cache.getOrElseUpdate((s,e), isPalindrome(str, s,e, cache)) && str.charAt(start) == str.charAt(end)
      }
    }
   query(start, end)
  }

  def subStrings(str: String): Iterator[(Int, Int)] = {
    for {
      i <- (0 until str.length).iterator
      j <- 0 until str.length if i + j < str.length
    } yield (i,i+j)
  }

}
