package org.rogue1.leetcode.trees

object InPreTreeConstructor {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }


  object Solution {

    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
      val postMap = postorder.zipWithIndex.toMap
      val inMap = inorder.zipWithIndex.toMap

      /**
       * find new post-order index range using new in-order index range and build subtree
       * @param newInStart
       * @param newInEnd
       * @return
       */
      def buildSubTree(newInStart: Int, newInEnd: Int): TreeNode = {
        val seq = (newInStart to newInEnd).map(x => postMap(inorder(x))).sorted // subtree indexes in postorder array
        if (seq.isEmpty) {
          null
        } else {
          val (newPostStart, newPostEnd) = (seq.head, seq.last)
          build(newInStart, newInEnd, newPostStart, newPostEnd)
        }
      }

      def build(inStart: Int, inEnd: Int,
                postStart: Int, postEnd: Int): TreeNode = {
        if (postEnd - postStart <= 0) {
          new TreeNode(postorder(postStart))
        } else {
          val root = postorder(postEnd)
          val rootIdx = inMap(root)
          val rtNode = new TreeNode(root)
          val leftNode = buildSubTree(inStart, rootIdx - 1)
          val rightNode = buildSubTree(rootIdx+1, inEnd)
          rtNode.left = leftNode
          rtNode.right = rightNode
          rtNode
        }
      }

      if (inorder.length == 0) null else
        build( 0, inorder.length - 1, 0, postorder.length - 1)
    }


  }



}
