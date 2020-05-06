package org.rogue1.leetcode.graph

/**
 * https://leetcode.com/problems/number-of-islands/submissions/ <Youtube>
 *
 * this is like a graph problem. you will have to basically identify number of connected isolated clusters of nodes
 * in a graph (like  a sub dags).
 *
 * use a DFS (or BFS if you prefer) to visit related connected nodes in the graph.
 * instead of maintaining a separate visited nodes list you could updated the grids '1' to '0' in-place (called sinking a land node).
 *
 *
 */
object NumberOfIslands {

  def numIslands(grid: Array[Array[Char]]): Int = {
    var counter = 0
    if (grid.isEmpty || grid == null) {
      0
    } else {
      for {
        i <- grid.indices
        j <- grid(i).indices
      } {
        counter += dfs(grid, i, j)
      }
      counter
    }
  }


  def dfs(grid: Array[Array[Char]], i: Int, j: Int): Int = {
    if (i < 0 || i >= grid.length || j < 0 || j >= grid(i).length || grid(i)(j) == '0') {
      0
    } else {
      grid(i)(j) = '0'
      dfs(grid, i+1, j)
      dfs(grid, i-1, j)
      dfs(grid, i, j+1)
      dfs(grid, i, j-1)
      1 // the dfs method must not return 1 for each land nodes it sinks but must return 1 for all the connected
      // land nodes it sinks
    }
  }

}
