package org.rogue1.leetcode.strings

object Strings {

  /**
   * longest substring in a string without duplicates.
   * https://www.geeksforgeeks.org/merge-sort/
   * logic:
   *  imagine a worm made up of characters. this worm is not allowed to have duplicates (duplicates = cancer).
   *  the worm's head and tail are at zero index at first.
   *  try to grow the head of the worm.
   *    if it results in duplicates shrink by the tail end
   *    if no duplicate move the head to the new position
   *    register the worm size if its bigger than ever.
   *  return the maximum size of the worm registered.
   * @param str
   * @return
   */
  def lengthOfLongestSubstring(str: String): Int = {
    var head, tail = 0
    val worm = scala.collection.mutable.Set[Char]()
    var res = 0
    while(head < str.length && tail < str.length) {
      if (worm.contains(str.charAt(head))) {
        worm.remove(str.charAt(tail))
        tail += 1
      } else {
        worm.add(str.charAt(head))
        res = Math.max(worm.size, res)
        head += 1
      }
    }
    res
  }

  /**
   * simple regex solution to convert a string to Int.
   * https://leetcode.com/explore/interview/card/amazon/76/array-and-strings/2963/
   * partially complete solution.
   * @param str
   * @return
   */
  def stringToInt(str: String): Int = {
    val rgx = "^[\\s]*(([\\+|\\-]?)([\\d]+))[^\\d]*.*$".r
    rgx.findFirstMatchIn(str).map(x => (Option(x.group(2)), x.group(3))) match {
      case Some((Some("-"), x)) if x.dropWhile(_.toString == "0").length > 10 || - x.toLong < Int.MinValue  => Int.MinValue
      case Some((_, x)) if x.dropWhile(_.toString == "0").length > 10 || x.toLong > Int.MaxValue =>  Int.MaxValue
      case Some((Some("-"), x))  =>  - x.toInt
      case Some((_, x)) => x.toInt
      case None  => 0
    }
  }

}
