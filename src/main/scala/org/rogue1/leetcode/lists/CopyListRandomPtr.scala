package org.rogue1.leetcode.lists

/**
 * A linked list is given such that each node contains an additional random pointer which could point to any node in the list or null.
 *
 * Return a deep copy of the list.
 *
 * The Linked List is represented in the input/output as a list of n nodes. Each node is represented as a pair of [val, random_index] where:
 *
 * val: an integer representing Node.val
 * random_index: the index of the node (range from 0 to n-1) where random pointer points to, or null if it does not point to any node.
 * ------------------------------------------------------------------------------------------
 * https://leetcode.com/problems/copy-list-with-random-pointer/
 * ------------------------------------------------------------------------------------------
 */
object CopyListRandomPtr {


  import scala.collection.mutable

  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  /**
   *
   * @param head
   * @return
   */
  def copyRandomList(head: Node): Node = {
    if (head == null) {
      return null
    }
    walk(head, mutable.Map[Node, Node]())
  }

  /**
   * the visited check should always happen in the first step of the function.
   * initially I had the check at the nested match pattern like below
   * case (None, Some(x)) if !visited.contains(x) => newNode.random = walk(x, visited).
   * But this will be problematic because the in the (Some(x), Some(y)) you call walk twice one for the next
   * and another for random. the first next call will update the visited state and this will not reflect in the
   * second random walk call unless you return the new visited state from the walk method.
   * the simpler solution is to do the visited check at the top of the walk function and this simplifies the nested
   * pattern match clause too..
   * @param node
   * @param visited
   * @return
   */
  def walk(node: Node, visited: mutable.Map[Node, Node]): Node = {
    visited.get(node) match{
      case Some(x) => x
      case None =>
        val newNode = new Node(node.value)
        visited(node) = newNode
        (Option(node.next), Option(node.random)) match {
          case (Some(x), None) => newNode.next = walk(x, visited)
          case (None, Some(x)) => newNode.random = walk(x, visited)
          case (Some(x), Some(y)) =>
            newNode.next = walk(x, visited)
            newNode.random = walk(y, visited)
          case (None,None) => ()
        }
        newNode
    }
  }

}
