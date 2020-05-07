package org.rogue1.leetcode.trees

/**
 * https://leetcode.com/problems/lowest-common-ancestor-of-a-binary-tree/submissions/
 *
 * Things learned:
 * 1. don't shy away from using uncoventional technique's some time. for instance the traversal method does a
 * side-effect operation to update the result. this is quite unorthodox and a normally not used technique.
 *
 */
class LowestCommonAncestor {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  private var result: TreeNode = _

  def lowestCommonAncestor(root: TreeNode,
                           p: TreeNode,
                           q: TreeNode): TreeNode = {
    traversal(root, p.value, q.value)
    result
  }

  def traversal(node: TreeNode, p: Int, q: Int): Boolean = {
    def process(mid: Boolean, left: Boolean, right: Boolean): Boolean = {
      if((mid && left) || (mid && right) || (left && right)) result = node
      mid || left || right
    }
    val res = node.value == p || node.value == q
    (Option(node.left), Option(node.right)) match {
      case (Some(x), Some(y)) => process(res, traversal(x,p, q), traversal(y, p, q))
      case (None, Some(x)) => process(res, left = false, right = traversal(x, p, q))
      case (Some(x), None) => process(res, right = false, left = traversal(x, p, q))
      case (None, None) => false
    }
  }

}
