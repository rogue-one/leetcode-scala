package org.rogue1.leetcode.trees


object LevelOrderTraversal {

  class TreeNode(_value: Int = 0,
                 _left: TreeNode = null,
                 _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  import scala.collection.mutable

  def levelOrder(root: TreeNode): List[List[Int]] = {
    var queue = List[(Int, TreeNode)]()
    val res = mutable.Map[Int, List[Int]]()
    if (root == null) return Nil
    queue = (0, root) :: Nil
    while(queue.nonEmpty) {
      println("I am in loop")
      val (lvl, x) = queue.head
      queue = queue.tail
      if (x.right != null) queue = queue ++ List((lvl + 1) -> x.right)
      if (x.left != null) queue = queue ++ List((lvl + 1) -> x.left)
      res(lvl) = x.value :: res.getOrElse(lvl, Nil)
    }
    res.keys.toList.map(res)
  }



}
