package org.rogue1.leetcode.logic

object Logic1 {

  def createCombinations(): Unit = {
    val l1 = (1 to 10).toArray
    val comb1 = for { i <- l1.indices; j <- (i+1) until l1.length } yield (l1(i), l1(j))
    assert(l1.combinations(2).map({ case Array(x,y) => x -> y}).toList == comb1, "combination logic incorrect")
  }


}
