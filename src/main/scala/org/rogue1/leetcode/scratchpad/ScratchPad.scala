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

  def battleShip(size: Int,
                 ships: String,
                 hits: String): String = {
    val map = ('a' to 'z').zip(1 to 26).map({ case (x,y) => x -> y}).toMap
    val shipLoc = ships
      .split(",")
      .map(_.split("\\s").map(_.trim.toList).collect({ case List(x,y) => x.toInt -> map(y) }))
    val hitLoc = hits.split("\\s").map(_.trim.toList).collect({ case List(x,y) => x.toInt -> map(y) })
    val sea = Array.ofDim[Int](size, size)
    val hit = 1
    hitLoc.foreach({ case (x,y) => sea(x)(y) = 1 })
    var hitShips, sunkShips = 0
    shipLoc.foreach(x => {
      val res = x.count({ case (y, z) => sea(y)(z) == 1 })
      if (x.length == res) { sunkShips += 1 }
      else if (res > 0) { hitShips += 1 }
      }
    )
    s"$sunkShips , $hitShips"
  }


  def consecutiveChars(a: Int, b: Int, c: Int): String = {
    def process(a: Int, b: Int, c: Int, prefix: String): List[String] = {
      println(prefix)
      val resA = if (a > 0 && !(prefix.takeRight(2) == "aa")) {
        process(a - 1, b, c, s"${prefix}a")
      } else prefix :: Nil
      val resB = if (b > 0 && !(prefix.takeRight(2) == "bb")) {
        process(a, b - 1, c, s"${prefix}b")
      } else prefix :: Nil
      val resC = if (c > 0 && !(prefix.takeRight(2) == "cc")) {
        process(a, b, c - 1, s"${prefix}c")
      } else prefix :: Nil
      resA ++ resB ++ resC
    }
    val res = process(a,b,c, "")
    println(res)
    res.maxBy(_.length)
  }



  def test() = {

    def topKFrequent(words: Array[String], count: Int): List[String] = {
      val map = words.foldLeft(Map[String, Int]())({ case (acc,x) => acc + (x -> (acc.getOrElse(x,0)+1)) })
      val heap = new scala.collection.mutable.PriorityQueue[(String, Int)]()(
        Ordering.by[(String,Int), Int](x => x._2).orElse(Ordering.by[(String,Int), String](x => x._1).reverse)
      )
      map.foreach({case (k,v) => heap.enqueue(k -> v)})
      for (_ <- (1 to count).toList) yield { heap.dequeue._1 }
    }

  }

}
