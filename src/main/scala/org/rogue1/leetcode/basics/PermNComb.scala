package org.rogue1.leetcode.basics

object PermNComb {

  def combinations2(input: Array[String]): List[List[String]] = {
    for {
      i <- input.indices.toList
      j <- i + 1 until input.length
    } yield { input(i) :: input(j) :: Nil }
  }


  def permutations(input: List[String]): List[List[String]] = {
    for {
      i <- input.indices.toList
      j <- input.indices if j != i
      k <- input.indices if k != j && k != i
      l <- input.indices if l != i && l != j && l != k
    } yield input(i) :: input(j) :: input(k) :: input(l) :: Nil
  }

}
