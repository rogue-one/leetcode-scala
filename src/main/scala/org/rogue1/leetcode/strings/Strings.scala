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
      case Some((_, x)) => x.toIn t
      case None  => 0
    }
  }

  /**
   * Return the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.
   *
   * Example 1:
   * Input: haystack = "hello", needle = "ll"
   * Output: 2
   * Example 2:
   * Input: haystack = "aaaaa", needle = "bba"
   * Output: -1
   *
   * @param haystack
   * @param x
   * @return
   */
  def strStr(haystack: String, needle: String): Int = {
    haystack
      .sliding(needle.length)
      .zipWithIndex
      .collectFirst({ case (x, y) if x == needle => y})
      .getOrElse(-1)
  }


  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs
      .map(x => x -> x.foldLeft(Map[Char, Int]())({ case (acc, x) => acc + (x -> acc.getOrElse(x, 0)) }) )
      .groupMap({ case (k,v) => v })({ case (k,v) => k })
      .map({ case (_, v) => v.toList })
      .toList
  }


  /**
   * --------------------------------------------------------------
   * THIS SOLUTION IS INCOMPLETE
   * --------------------------------------------------------------
   * Given a string S and a string T, find the minimum window in S which will contain all the characters in T in complexity O(n).
   *
   * Example:
   *
   * Input: S = "ADOBECODEBANC", T = "ABC"
   * Output: "BANC"
   * Note:
   *
   * If there is no such window in S that covers all characters in T, return the empty string "".
   * If there is such window, you are guaranteed that there will always be only one unique minimum window in S.
   */
   object MinWindow {

    import scala.collection.mutable
    import scala.util.control.Breaks._


    def strToMap(str: String): Map[Char, Int] = {
      str.foldLeft(Map[Char, Int]())({ case (acc, x) => acc.updated(x ,acc.getOrElse(x, 0)) })
    }

    def seekWindow(pos: Int,
                   str: String,
                   ref: Map[Char, Int]): Option[(String, Int)] = {
      val state = mutable.Map[Char, Int]()
      var tail, head = pos
      while(head < str.length-1 && !matchMap(state, ref)) {
         val chr = str.charAt(head)
         state(chr) = state.getOrElse(chr, 0) + 1
         head += 1
      }
      if (!matchMap(state, ref) && head == str.length - 1) return None
      breakable {
        while (tail < head) {
          val chr = str.charAt(tail)
          if (!ref.contains(chr) || ref(chr) < state(chr)) {
            tail += 1
            state(chr) = state(chr) - 1
          } else {
            break
          }
        }
      }
      Some(str.substring(head, tail) -> head)
    }

    def matchMap(map: mutable.Map[Char, Int],
                 ref: Map[Char, Int]): Boolean = {
      ref.forall({ case (c, i) => map.getOrElse(c, 0) == i })
    }

    def process(pos: Int, str: String, ref: Map[Char, Int]): List[String] = {
      seekWindow(pos, str, ref) match {
        case Some((newStr, newPos)) if newPos == str.length - 1 => newStr :: Nil
        case Some((newStr, newPos)) => newStr :: process(newPos, str, ref)
        case None => Nil
      }
    }

    def minWindow(s: String, t: String): String = {
      process(0, s, strToMap(t)).minBy(_.length)
    }

  }

  /**
   * https://leetcode.com/problems/compare-version-numbers/
   *
   * @param version1
   * @param version2
   * @return
   */
  def compareVersion(version1: String, version2: String): Int = {
    val v1 = version1.split("\\.")
    val v2 = version2.split("\\.")
    v1.zipAll(v2, "0", "0")
      .map({ case (x,y) => x.toInt -> y.toInt })
      .foreach({
        case (x,y) if x < y => return -1
        case (x,y) if x > y => return 1
        case (x,y) if x == y => ()
      })
    0
  }


  def mostCommonWord(paragraph: String,
                     banned: Array[String]): String = {
    var res = ""
    var resCnt = 0
    "ball,".replaceAll("[^a-z]", "")
      paragraph
        .toLowerCase()
        .split("\\s")
        .filterNot(banned.contains)
        .foldLeft(Map[String, Int]())({ case (acc, x) => acc.updated(x, acc.getOrElse(x, 0)+1)})
        .foreach({ case (k,v) if (v > resCnt) => { res = k; resCnt = v } case _ => ()  })
    res
  }


}
