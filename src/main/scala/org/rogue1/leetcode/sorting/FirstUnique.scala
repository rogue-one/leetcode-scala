package org.rogue1.leetcode.sorting

import scala.collection.mutable

class FirstUnique(_nums: Array[Int]) {


  private var set = mutable.LinkedHashSet[Int]()
  private val map = mutable.Map[Int, Boolean]()
  _nums.foreach(updateMap)
  _nums.foreach(x => if(map(x)) set.add(x))

  def showFirstUnique(): Int = {
    set.headOption match {
      case Some(x) => x
      case None => -1
    }
  }

  def add(value: Int): Unit = {
    if(updateMap(value)) {
      set.add(value)
    } else {
      set.remove(value)
    }
  }

  def updateMap(x: Int): Boolean = {
    map.get(x) match {
      case Some(true) => map(x) = false; false
      case Some(false) => false
      case None => map(x) = true; true
    }
  }



}

