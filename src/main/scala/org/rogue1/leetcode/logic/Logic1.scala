package org.rogue1.leetcode.logic

object Logic1 {

  def createCombinations(): Unit = {
    val l1 = (1 to 10).toArray
    val comb1 = for { i <- l1.indices; j <- (i+1) until l1.length } yield (l1(i), l1(j))
    assert(l1.combinations(2).map({ case Array(x,y) => x -> y}).toList == comb1, "combination logic incorrect")
  }


  /**
   * transpose is common operation on matrix.
   * the swap block must only be called for unique combinations of indexes.
   *
   *
   * @param matrix
   * @return
   */
  def inlineTranspose(matrix: Array[Array[Int]]): Unit = {
    for {
      i <- matrix.indices
      j <- i+1 until matrix.length // it should not be matrix.indices because the swap operation will be called for the
      // any index  (for eg: (1,2)) will be called twice. and this will result in reversing the swap.
    } {
      val tmp = matrix(i)(j)
      matrix(i)(j) = matrix(j)(i)
      matrix(j)(i) = tmp
    }
  }


  def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val arr = Array.ofDim[Int](matrix.length, matrix(0).length)
    for {
      i <- matrix.indices
      j <- matrix.indices
    } {
      arr(j)(i) = matrix(i)(j)
    }
    arr
  }


}
