package org.rogue1.leetcode.sorting

/**
 * https://leetcode.com/problems/reorder-data-in-log-files/
 */
object ReorderLogFiles {

  def reorderLogFiles(logs: Array[String]): Array[String] = {
    val intRgx = "([a-zA-Z0-9]+)[\\s]+(([0-9]|\\s)+)+".r
    val chrRgx = "([a-zA-Z0-9]+)[\\s]+(([a-z]|\\s)+)".r
    val ordering = Ordering
      .by[(Int, String, String, String), Int](x => x._1)
      .orElse(Ordering.by[(Int, String, String, String), String](x => x._2))
      .orElse(Ordering.by[(Int, String, String, String), String](x => x._3))
    val res = logs.map({
      case x if intRgx.matches(x) => (2,"","",x)
      case x if chrRgx.matches(x)  =>
        val Some(mtch) = chrRgx.findFirstMatchIn(x)
        (1,mtch.group(2), mtch.group(1) ,x)
    }).sorted(ordering)
    res.map(_._4)
  }

}
