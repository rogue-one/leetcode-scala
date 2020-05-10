package org.rogue1.leetcode.strings

object ConsecutiveString {

  def consecutiveChars(a: Int, b: Int, c: Int): String = {
    def process(a: Int, b: Int, c: Int, prefix: String): List[String] = {
      val resA = if (a > 0 && !(prefix.takeRight(2) == "aa")) {
        process(a - 1, b, c, s"${prefix}a")
      } else prefix :: Nil
      val resB = if (b > 0 && !(prefix.takeRight(2) == "bb")) {
        process(a, b - 1, c, s"${prefix}b")
      } else prefix :: Nil
      val resC = if (c > 0 && !(prefix.takeRight(2) == "cc")) {
        process(a, b, c - 1, s"${prefix}c")
      } else prefix :: Nil
      resA ++ resB ++ resC
    }
    val res = process(a,b,c, "")
    res.maxBy(_.length)
  }

}
