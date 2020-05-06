package org.rogue1.leetcode.trees

/**
 * https://www.youtube.com/watch?v=MILxfAbIhrE&list=PLrmLmBdmIlpv_jNDXtJGYTPNQ2L1gdHxu&index=8.
 */
object ValidateBST {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def isValidBST(root: TreeNode): Boolean = {
    if(root == null) true else validate(root, Long.MinValue, Long.MaxValue)
  }

  def validate(node: TreeNode, low: Long, high: Long): Boolean = {
    val res = (low < node.value && node.value < high)
    res && {
      (Option(node.left), Option(node.right)) match {
        case (Some(x), Some(y)) => validate(x, low ,node.value) && validate(y, node.value, high)
        case (Some(x), None) => validate(x, low ,node.value)
        case (None, Some(x)) => validate(x, node.value, high)
        case (None, None) => true
      }
    }
  }

}
