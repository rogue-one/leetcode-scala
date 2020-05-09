package org.rogue1.leetcode.strings

/**
 * https://leetcode.com/problems/generate-parentheses/
 */
object GenerateParentheses {

  object BruteForce {
    def generateParenthesis(n: Int): List[String] = {
      val elem = List("(", ")")
      (0 to n-2)
        .foldLeft(elem)({ case (acc, _) => elem ++ acc  })
        .permutations.toList
        .filter(validate)
        .map(_.mkString(""))
    }
  }

  object Recursive {

    def generateParenthesis(n: Int): List[String] = {
      generate(n*2, "", 0)
    }

    def generate(n: Int, prefix: String, computed: Int): List[String] = {
      if (prefix.length == n) {
        if (computed == 0) prefix :: Nil else Nil
      } else if (computed < 0 || Math.abs(computed) > (n - prefix.length)) {
        Nil
      } else {
        generate(n, s"$prefix(", computed+1) ++ generate(n, s"$prefix)", computed-1)
      }
    }

  }




}
