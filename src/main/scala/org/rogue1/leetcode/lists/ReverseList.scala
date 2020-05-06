package org.rogue1.leetcode.lists

/**
 * https://leetcode.com/problems/reverse-linked-list/submissions/
 */
object ReverseList {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def reverseList(head: ListNode): ListNode = {
    if (head == null)
      null
    else
      walk(head, None)
  }

  /**
   * the main logic here is that you will have to look base case i.e the last element of the last.
   * for the base case when you are visiting the last node in the list you definitely need the previous node.
   * some start with the logic that in-addition to current node the function must also take the previous node.
   * @param node
   * @param prev
   * @return
   */
  @scala.annotation.tailrec
  def walk(node: ListNode, prev: Option[ListNode]): ListNode = {
    (Option(node.next), prev) match {
      case (Some(x), Some(y)) => node.next = y; walk(x, Some(node))
      case (Some(x), None) => node.next = null; walk(x, Some(node))
      case (None, Some(x)) => node.next = x; node
      case (None, None) => node
    }
  }

}
