package org.rogue1.leetcode.graph
import scala.collection.{Set, mutable}

/**
 * https://leetcode.com/problems/critical-connections-in-a-network/submissions/
 */
object CriticalConnections {

  /**
   * the problem is identifying edges that are not part of a cycle in a undirected graph.
   * one simple strategy is to remove each of the egdes in the graph and perform a DFS.
   * if you are able to reach all nodes in the graph in a DFS after removing a edge then that edge is
   * part of a cycle. otherwise then it is critical edge.
   *
   */
  object BruteForce {
    def criticalConnections(n: Int, connections: List[List[Int]]): List[List[Int]] = {
      val graph = connections.foldLeft(mutable.Map[Int, Set[Int]]())({
        case (acc, List(x, y)) =>
          acc(x) = Set(y) | acc.getOrElse(x, Set())
          acc(y) = Set(x) | acc.getOrElse(y, Set())
          acc
      })

      def dfs(node: Int, excludeEdge: (Int, Int), visited: Set[Int]): Set[Int] = {
        val (n1, n2) = excludeEdge
        val next = graph(node)
          .filterNot(x => (node == n1 && x == n2) || (node == n2 && x == n1))
          .filterNot(visited(_))
        val newVisited = Set(node) | visited
        next match {
          case x: Set[Int] if x.isEmpty => newVisited
          case x => x.flatMap(x => dfs(x, excludeEdge, newVisited))
        }
      }

      connections.filterNot({ case List(x, y) => dfs(0, (x, y), Set()).size == n })
    }
  }



}
