package org.rogue1.leetcode.graph

/**
 * https://leetcode.com/problems/course-schedule/solution/
 *
 * In complete solution..
 *
 */
object CourseSchedule {

  case class Node(id: Int, private var _dependents: List[Node]) {
    override def toString: String = s"$id -> ${dependents.map(_.id).mkString(",")}"
    def addDependents(node: Node): Unit = {
      _dependents = node :: _dependents
    }
    def dependents: List[Node] = _dependents
  }

  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val cache = (0 until numCourses).map(x => x -> Node(x, Nil)).toMap

    prerequisites.foreach({
      case Array(c, p) => cache(c).addDependents(cache(p))
    })
    cache
      .values.toList.find(_.dependents.isEmpty) match {
      case None => false
      case Some(head) =>
        val res = topSort(head, cache.values.toList, Nil)
        res.map(_.id).toSet == cache.values.map(_.id).toSet
    }
  }

  def topSort(node: Node,
              nodes: List[Node],
              visited: List[Node]): List[Node] = {
    val newVisited = node :: visited
    nodes
      .filterNot(newVisited.contains)
      .filter(x => x.dependents.forall(newVisited.contains)) match {
      case Nil => newVisited
      case x => x.flatMap(x => topSort(x, nodes, newVisited))
    }
  }

}
