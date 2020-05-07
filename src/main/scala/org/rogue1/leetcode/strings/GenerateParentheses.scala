package org.rogue1.leetcode.strings

object GenerateParentheses {

  def generateParenthesis(n: Int): List[String] = {
    val elem = List("(", ")")
    (0 to n-2)
      .foldLeft(elem)({ case (acc, _) => elem ++ acc  })
      .permutations.toList
      .filter(validate)
      .map(_.mkString(""))
  }

  def validate(li: List[String]): Boolean = {
    var ctr = 0
    li.foreach({ case "(" => ctr += 1 case ")" => ctr -=  1; if (ctr < 0) return false })
    true
 }

}
