package org.rogue1.leetcode.arrays

object UserWebsitePattern {

  def mostVisitedPattern(username: Array[String], timestamp: Array[Int], website: Array[String]): List[String] = {
    val cache = map(username, timestamp, website)
    val res = cache.keys
      .flatMap(x => subsequences(cache(x)).map(x -> _))
      .filter(_._2.length == 3)
      .groupBy(_._2)
      .map(x => x._1 -> x._2.map(_._1))
    println(res)
    res
      .toList.sortBy({ case (x, y) => y.size -> x.mkString(",")  })(Ordering.Tuple2(Ordering.Int.reverse, Ordering.String))
      .take(1)
      .headOption match {
      case Some((x, _)) => x
      case None => Nil
    }
  }

  def subsequences(array: List[String]): List[List[String]] = {
    if (array.length <= 3) array :: Nil else {
      for {
        i <- array.indices.toList
        j <- ((i + 1) until (array.length - 1))
        k <- ((j + 1) until (array.length))
      } yield {
        array(i) :: array(j) :: array(k) :: Nil
      }
    }
  }

  def map(username: Array[String],
          timestamp: Array[Int],
          website: Array[String]): Map[String, List[String]] = {
    val data = for { i <- username.indices.toList  } yield { (username(i), timestamp(i), website(i)) }

    val res = for { (u,_,w) <- data.sortBy(_._2) } yield { u -> w }
    res.foldLeft(Map[String, List[String]]())({
      case (acc, (k,v)) => acc + (k -> (acc.getOrElse(k, Nil) :+ v))
    })
  }

}
