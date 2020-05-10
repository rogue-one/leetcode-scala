package org.rogue1.leetcode.arrays

object BattleShipHits {

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

}
