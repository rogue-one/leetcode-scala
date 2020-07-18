package org.rogue1.leetcode.trees

import scala.collection.mutable

object BTreePreOrderSerde {

  case class TreeNode(value: Int,
                      var left: Option[TreeNode]=None,
                      var right: Option[TreeNode]=None) {
    def nl(node: TreeNode): Unit = left = Some(node)
    def nr(node: TreeNode): Unit = right = Some(node)
  }

  def serialize(node: TreeNode): String = {
    node match {
      case TreeNode(x, Some(l), Some(r)) => s"$x,${serialize(l)},${serialize(r)}"
      case TreeNode(x, None, None) => s"$x,X,X"
      case TreeNode(x, Some(l), None) => s"$x,${serialize(l)},X"
      case TreeNode(x, None, Some(r)) => s"$x,X,${serialize(r)}"
    }
  }

  /**
   * THIS IS VERY IMPORTANT. the process method must take mutable buffer and all the recursion call will
   * end up removing nodes the mutable buffer. It might seem reasonable
   * @param node
   * @return
   */
  def deserialize(node: String): Option[TreeNode] = {
    val arr = node.split(",").toBuffer
    def process(list: mutable.Buffer[String]): Option[TreeNode] = {
      list.headOption match {
        case Some("X") =>  list.remove(0); None
        case None => None
        case Some(x) =>
          val root = TreeNode(x.toInt)
          list.remove(0)
          list match {
            case mutable.Buffer() => ()
            case mutable.Buffer(_) => root.left = process(list)
            case _ => val (left, right) = (process(list), process(list))
              root.left = left; root.right = right
          }
          Some(root)
      }
    }
    process(arr)
  }



  def test(): Unit = {
    val n20 = TreeNode(20)
    val n8 = TreeNode(8)
    val n4 = TreeNode(4)
    val n12 = TreeNode(12)
    val n10 = TreeNode(10)
    val n14 = TreeNode(14)
    val n22 = TreeNode(22)
    n20.nl(n8)
    n20.nr(n22)
    n8.nl(n4)
    n8.nr(n12)
    n12.nl(n10)
    n12.nr(n14)
    val Some(root) = deserialize(serialize(n20))
    assert(root == n20)
  }

}
