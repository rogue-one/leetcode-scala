package org.rogue1.leetcode.lists

object MergeKSortedLists {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    lists.headOption match {
      case Some(x) => lists.tail.foldLeft(x)({ case (acc, x) => merge(acc, x) })
      case None => null
    }
  }

  def merge(l1: ListNode, l2: ListNode) : ListNode = {
    (Option(l1), Option(l2)) match {
      case (None, None) => null
      case (Some(x), None) => x
      case (None, Some(x)) => x
      case (Some(x), Some(y)) if y.x < x.x => y.next = merge(y.next, x); y
      case (Some(x), Some(y)) => x.next = merge(x.next, y); x
    }
  }

}
