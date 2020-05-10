package org.rogue1.leetcode.scratchpad

import java.awt.im.InputMethodRequests

import scala.collection.mutable

object ScratchPad {

  def mostVisitedPattern(username: Array[String], timestamp: Array[Int], website: Array[String]): List[String] = {
    val arr = username.zip(timestamp).zip(website).map({ case ((x,y),z) => (x,y,z) })
    arr.sortInPlaceBy(x => x._2)
    val map1 = arr.foldLeft(Map[String, List[String]]())({
      case (acc, (u, _, w)) => acc + (u -> (acc.getOrElse(u, Nil) :+ w ))
    })
    val map2: Map[String, List[List[String]]] = map1.map({ case (x,v) => x -> v.combinations(3).toList.sorted })
    val map3 = scala.collection.mutable.Map[List[String], Int]()
    map2.foreach({
      case (user, listTriplet) =>
        listTriplet.foreach(triplet =>  map3(triplet) = map3.getOrElse(triplet, 0) + 1 )
    })
    val ordering = Ordering.by[(List[String], Int), Int](x => x._2).reverse.
      orElse(Ordering.by[(List[String], Int), String](x => x._1.mkString("")))
    val heap = new mutable.PriorityQueue[(List[String], Int)]()(ordering)
    map3.foreach({ case (x,y) => heap.addOne(x -> y) })
    heap.dequeue()._1
  }


}
