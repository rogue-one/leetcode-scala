package org.rogue1.leetcode.arrays

object WordSearchMatrix {


  def exist(board: Array[Array[Char]],
            word: String): Boolean = {
    def dfs(x: Int, y: Int, prefix: String, visited: Set[(Int, Int)]): Boolean = {
      if(x < 0 || x >= board.length || y < 0 || y >= board(x).length || visited((x,y))) {
        false
      } else {
        val newPrefix = s"$prefix${board(x)(y)}"
        if (!word.startsWith(newPrefix)) {
          false
        } else if (word == newPrefix) { return true }
        else {
          val newVisited = visited | Set((x,y))
          (dfs(x-1,y, newPrefix, newVisited) || dfs(x+1,y,newPrefix, newVisited) ||
            dfs(x,y-1,newPrefix, newVisited) || dfs(x, y+1, newPrefix, newVisited))
        }
      }
    }
    val res = for {
      x <- board.indices
      y <- board(x).indices
      res = dfs(x, y, "", Set()) if res
    } yield { () }
    res.nonEmpty
  }



}
