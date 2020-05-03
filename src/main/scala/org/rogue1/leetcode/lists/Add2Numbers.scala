package org.rogue1.leetcode.lists

object Add2Numbers {

  class ListNode(var _x: Int = 0) {
     var next: ListNode = _
     var x: Int = _x
  }

  /**
   * You are given two non-empty linked lists representing two non-negative integers.
   * The digits are stored in reverse order and each of their nodes contain a single digit.
   * Add the two numbers and return it as a linked list.
   *
   * You may assume the two numbers do not contain any leading zero, except the number 0 itself.
   *
   * @param l1
   * @param l2
   * @return
   */
  def addTwoNumbers(l1: ListNode,
                    l2: ListNode): ListNode = {
    val list1 = iterator(l1).toList
    val list2 = iterator(l2).toList
    new Adder(list1, list2).add
  }


  def iterator(head: ListNode): Iterator[Int] = new Iterator[Int] {
    private var ptr: ListNode = head
    override def hasNext: Boolean = { ptr != null }
    override def next(): Int = {
      val x = ptr
      ptr = ptr.next
      x.x
    }
  }

  class Adder(l1: List[Int], l2: List[Int]) {

    private var carry = 0

    def add: ListNode = {
      val res = l1.zipAll(l2, 0, 0).map({ case (x,y) => sum(x, y) })
      val res1 = if (carry > 0) {  res :+ carry } else res
      val res2 = res1.mkString("").map(x => new ListNode(s"$x".toInt))
      link(res2.toList)
    }

    private def link(l1: List[ListNode]): ListNode = {
      l1 match {
        case head :: Nil => head
        case head :: tail =>  head.next = link(tail); head
        case Nil => ???
      }
    }

    private def sum(x: Int, y: Int): Int = {
      val res = x + y + carry
      carry = res / 10
      res % 10
    }

  }

}
