package org.rogue1.leetcode.basics

import scala.collection.mutable

object Subsequences {

  def allPossibleSubsequencesSize3(test: Array[String]): Seq[List[String]] = {
    for {
      i <- test.indices
      j <- ((i + 1) until (test.length - 1))
      k <- ((j+1) until (test.length))
    } yield {
      test(i) :: test(j) :: test(k) :: Nil
    }
  }


}
