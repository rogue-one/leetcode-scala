package org.rogue1.leetcode.sorting

object MeetingRooms {

  object Part1 {

    def canAttendMeetings(intervals: Array[Array[Int]]): Boolean = {
      if (intervals.isEmpty) return true
      intervals.sortInPlaceBy(x => x.head)
      intervals.zip(intervals.tail).foreach({
        case (arr1, arr2) => if (isOverlap(arr1, arr2)) return false
      })
      return true
    }

  }

  object Part2 {

    def minMeetingRooms(intervals: Array[Array[Int]]): Int = {
      intervals.sortInPlaceBy(x => x.head)
      val scheduler = new Scheduler()
      intervals.foreach(scheduler.start)
      scheduler.maxCapacity
    }

    class Scheduler() {
      private var current: List[Array[Int]] = Nil
      private var _maxCapacity = 0
      def start(meeting: Array[Int]): Unit = {
        current = current.filterNot({ case Array(_,e) => e <= meeting.head })
        current = meeting :: current
        _maxCapacity = Math.max(_maxCapacity, current.length)
      }
      def maxCapacity: Int = _maxCapacity
    }

  }

  def isOverlap(meet1: Array[Int], meet2: Array[Int]): Boolean = {
    (meet1, meet2) match {
      case (Array(x1,x2), Array(y1,y2)) => ((x1 to x2).toSet & (y1 to y2).toSet).size > 1
      case _ => ???
    }
  }

}
