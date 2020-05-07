package org.rogue1.leetcode.trees


/**
 * https://leetcode.com/problems/diameter-of-binary-tree/submissions/
 */
object BTreeDiameter {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  import scala.collection.mutable


  def diameterOfBinaryTree(root: TreeNode): Int = {
    val cache = mutable.Map[TreeNode, Int]()
    if (root == null) 0 else {
      recurse(root, 0, cache);
      cache.toList.maxBy({ case (_,y) => y})._2
    }
  }

  def recurse(node: TreeNode, depth: Int, cache:  mutable.Map[TreeNode, Int]): Int = {
    (Option(node.left), Option(node.right)) match {
      case (Some(x), Some(y)) =>
        val (l,r) = (recurse(x, depth, cache)+1, recurse(y, depth+1, cache)+1)
        cache(x) = l + r; Math.max(l, r)
      case (Some(x), None) => val res = recurse(x, depth + 1, cache)+1; cache(node) = res; res
      case (None, Some(x)) => val res = recurse(x, depth + 1, cache)+1; cache(node) = res; res
      case (None, None) => cache(node) = 0; 0
    }
  }

}
