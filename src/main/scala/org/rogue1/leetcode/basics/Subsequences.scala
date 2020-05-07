package org.rogue1.leetcode.basics

object Subsequences {

  def allPossibleSubsequencesSize3(array: Array[String]): List[List[String]] = {
    for {
      i <- array.indices.toList
      j <- ((i + 1) until (array.length - 1))
      k <- ((j+1) until (array.length))
    } yield {
      array(i) :: array(j) :: array(k) :: Nil
    }
  }

}
