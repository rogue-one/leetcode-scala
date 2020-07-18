package org.rogue1.leetcode.trees

import scala.collection.mutable

//noinspection DuplicatedCode
object BTreePostOrderSerde {

  case class TreeNode(value: Int,
                      var left: Option[TreeNode]=None,
                      var right: Option[TreeNode]=None) {
    println(this)
    def nl(node: TreeNode): Unit = left = Some(node)
    def nr(node: TreeNode): Unit = right = Some(node)
  }

  def serialize(node: TreeNode): String = {
    node match {
      case TreeNode(x, Some(l), Some(r)) => s"${serialize(l)},${serialize(r)},$x"
      case TreeNode(x, None, None) => s"X,X,$x"
      case TreeNode(x, Some(l), None) => s"${serialize(l)},X,$x"
      case TreeNode(x, None, Some(r)) => s"X,${serialize(r)},$x"
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
      var res: Option[TreeNode] = None
      list match {
        case mutable.Buffer(l,r,rt) =>
          val left = if (l == "X") None else Some(TreeNode(l.toInt))
          val right = if (l == "X") None else Some(TreeNode(r.toInt))
          val root = TreeNode(rt.toInt)
          root.left = left; root.right = right
          Some(root)
        case _ =>
          list.remove(0)
          val left = if ( == "X") None else Some(TreeNode(l.toInt))
      }
      list.headOption match {
        case None => None
        case Some(x) =>
          val left = if (x == "X") None else Some(TreeNode(x.toInt))
          list.remove(0)
          list match {
            case mutable.Buffer() => None
            case mutable.Buffer(x) => x
            case _ => val (right, root) = (process(list), process(list))
              for { x <- root; y <- right } { x.right = Some(y) }
              root.foreach(_.left=left)
              root
          }
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

