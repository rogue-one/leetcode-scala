package org.rogue1.leetcode.lists

import org.rogue1.leetcode.lists.Add2Numbers.ListNode

import scala.collection.mutable

object MergeTwoLists {

  class ListNode(var _x: Int = 0) {
    val visited = mutable.Map[String, String]()
    var next: ListNode = null
    var x: Int = _x
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    merge(iterator(l1).toList, iterator(l2).toList)
  }

  def iterator(head: ListNode): Iterator[ListNode] = new Iterator[ListNode] {
    var node: ListNode = head
    override def hasNext: Boolean = node.next != null
    override def next(): ListNode = {
      val tmp = node
      node = node.next
      tmp
    }
  }

  def merge(l1: List[ListNode], l2: List[ListNode]): ListNode = {
    (l1, l2) match {
      case (head :: Nil, Nil) => head
      case (Nil, head :: Nil) => head
      case (head :: tail, Nil) => head.next = merge(tail, Nil); head
      case (Nil, head :: tail) => head.next = merge(Nil, tail); head
      case (h1 :: t1, h2 :: _)  if h1.x < h2.x =>  h1.next = merge(t1, l2); h1
      case (h1 :: _, h2 :: t2)  if h1.x >= h2.x =>  h2.next = merge(l1, t2); h2
    }
  }




}
