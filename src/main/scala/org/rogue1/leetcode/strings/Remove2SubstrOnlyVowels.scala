package org.rogue1.leetcode.strings

import scala.collection.mutable

object Remove2SubstrOnlyVowels {

  val cache: mutable.Map[String, Set[String]] = mutable.Map[String, Set[String]]()
  val vowels = Set('a', 'e', 'i', 'o', 'u')

  def vowelOnly(str: String): Int = {
    val prefixVowel = str.takeWhile(vowels(_))
    val sufficVowel = str.reverse.takeWhile(vowels(_))
    val midStr = str.dropWhile(vowels(_)) + str.dropWhile(vowels(_))
    val vowelOnlyMidStr = reduce(midStr, midStr.filterNot(vowels(_)).length)
    println(vowelOnlyMidStr)
    prefixVowel.length + sufficVowel.length + vowelOnlyMidStr.maxBy(_.length).length
  }

  /**
   * list of all possible vowel only substring
   * @param str
   * @param cCount
   * @param cache
   * @return
   */
  def reduce(str: String,
             cCount: Int,
             cache: mutable.Map[String, Set[String]]=cache): Set[String] = {
    def process(): Set[String] = {
      if (str == "") Set(str)
      else if (cCount == 0) Set(str)
      else {
        (vowels(str.charAt(0)), vowels(str.charAt(str.length - 1))) match {
          case (true, true) => reduce(str.tail, cCount) | reduce(str.dropRight(1), cCount)
          case (false, false) => reduce(str.tail, cCount - 1) | reduce(str.dropRight(1), cCount - 1)
          case (true, false) => reduce(str.tail, cCount) | reduce(str.dropRight(1), cCount - 1)
          case (false, true) => reduce(str.tail, cCount - 1) | reduce(str.dropRight(1), cCount)
        }
      }
    }
    cache.getOrElseUpdate(str, process())
  }

}
