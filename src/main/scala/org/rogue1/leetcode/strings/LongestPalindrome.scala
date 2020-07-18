package org.rogue1.leetcode.strings

class LongestPalindrome {

  /**
   *
   * main key point avoid palindrome computation for each sub-string.
   * just use a function that takes string indexes to identify if it is a palindrome.
   * this function can be memoized so that we don't re-compute previously computed results.
   *
   * base cases : THIS IS THE MOST IMPORTANT part
   * 1. if start-index == end-index return True (handles odd sized strings)
   * 2. if start-index + 1 == end-index and charAt[start-index] == charAt[end-index] then True (handles even sized strings)
   * 3. sub-string S(i,j) is palindrome if S(i+1,j-1) is a palindrome and charAt[i] == charAt[j]
   * palindrome
   * @param str
   * @return
   */
  def longestPalindrome(str: String): String = {
    import scala.collection.mutable
    val cache = mutable.Map[(Int, Int), Boolean]()
    def isPalindrome(start: Int, end: Int): Boolean = {
      cache.getOrElseUpdate((start, end), {
        if (start == end) true
        else if (start + 1 == end) str.charAt(start) == str.charAt(end)
        else { isPalindrome(start + 1, end - 1) && str.charAt(start) == str.charAt(end) }
      })
    }
    if (str == "") ""
    else if (str.length == 1) str
    else {
      var res: (Int, Int) = (0,0)
      for {
        i <- (0 until str.length).iterator ; j <- (i until str.length).iterator if isPalindrome(i, j)
      } { if ((res._2 - res._1) < (j - i)) res = (i,j) }
      str.substring(res._1,res._2 + 1)
    }
  }








}
