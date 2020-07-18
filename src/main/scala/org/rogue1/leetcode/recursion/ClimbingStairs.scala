package org.rogue1.leetcode.recursion

import scala.collection.mutable


/**
 * https://leetcode.com/problems/climbing-stairs/
 */
object ClimbingStairs {

  /**
   *
   * this method uses recursion and caching of computation.
   * But this programming strategy is called Recursion with Memoization and Dynamic programming.
   * this because this program takes Top down approach. for dynamic programming it has to be strictly bottom up approach.
   *
   */
  object Memoization {
      def climbStairs(destination: Int): Int = {
        val cache = mutable.Map[Int, Int]()
        def step(num: Int): Int = {
          num match {
            case _ if num > destination => 0
            case _ if destination == num => 1
            case _ => cache.getOrElseUpdate(num, step(num+1) + step(num+2))
          }
        }
        step(0)
      }
  }



  object DynamicProgramming {
    def climbStairs(n: Int): Int = {
      steps(1, mutable.Map[Int, Int]())
    }

    def steps(current: Int,
              cache: mutable.Map[Int, Int]): Int = {
        current match {
          case x if x <= 0 => 0
          case 1 => 1
          case _ => cache.getOrElseUpdate(current, steps(current-1, cache) + steps(current-2, cache))
      }
    }
  }

}
