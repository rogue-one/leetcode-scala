package org.rogue1.leetcode.trees

/**
 * https://www.youtube.com/watch?v=K7LyJTWr2yA
 */
object SymmetricTree {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  def isSymmetric(root: TreeNode): Boolean = {
    if (root == null) true else check(root, root)
  }

  def check(t1: TreeNode, t2: TreeNode): Boolean = {
    (Option(t1), Option(t2)) match {
      case (None, None) => true
      case (Some(TreeNode(x1, Some(ll), Some(lr))), Some(TreeNode(x2, Some(rl), Some(rr)))) =>
        x1 == x2 && check(ll, rr) && check(lr, rl)
      case (Some(TreeNode(x1, None, Some(lr))), Some(TreeNode(x2, Some(rl), None))) => x1 == x2 && check(lr, rl)
      case (Some(TreeNode(x1, Some(ll), None)), Some(TreeNode(x2, None, Some(rr)))) => x1 == x2 && check(ll, rr)
      case (Some(TreeNode(x1, None, None)), Some(TreeNode(x2, None, None))) => x1 == x2
      case (x,y) => false
    }
  }

  def check_another(t1: TreeNode, t2: TreeNode): Boolean = {
    if (t1 == null && t2 == null) {
       true
    } else if (t1 == null || t2 == null) {
      false
    } else {
      t1.value == t2.value && check_another(t1.left, t2.right) && check_another(t1.right , t2.left)
    }
  }

  object TreeNode {
    def unapply(t: TreeNode): Option[(Int, Option[TreeNode], Option[TreeNode])] = {
      Some(t.value, Option(t.left), Option(t.right))
    }
  }

}
