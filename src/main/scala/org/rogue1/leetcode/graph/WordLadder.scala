package org.rogue1.leetcode.graph

import scala.collection.mutable

object WordLadder {

  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val cache = wordList.map(x => x -> GraphNode(x, Nil)).toMap
    wordList.map(x => x -> wordList.filter(isNext(_, x))).foreach({ case (x,y) => cache(x).children = y.map(cache)})
    search(cache(beginWord), cache(endWord), Nil)
      .minBy(_.length)
      .length
  }


  def build(startWord: String, )


  def search(startNode: GraphNode,
             endNode: GraphNode,
             visitedNode: List[GraphNode]): List[List[GraphNode]] = {
    if(startNode.children.contains(endNode)) {
      List(startNode :: visitedNode)
    } else {
      startNode.children.flatMap(x => search(x, endNode, startNode :: visitedNode))
    }
  }

  def search1(word: String,
              endWord: String,
              wordList: List[String],
              visited: List[String]): List[List[String]] = {
    val next = wordList.filterNot(visited.contains).filter(isNext(_, word))
    next match {
      case Nil => List(word :: visited)
      case x => x.flatMap(search1(_, endWord, wordList, word :: visited))
    }
  }

  def bfs(startWord: String,
          endWord: String,
          wordList: List[String]): List[String] = {
    val queue = new Queue()
    queue.enqueue(startWord)
    while(queue.nonEmpty) {
      val node = queue.dequeue()
      if (node == endWord) {
        ""
      }
      val words = wordList.filterNot(queue.visited.contains).filter(isNext(_, node))
      queue.enqueueAll(words : _*)

    }
  }


  def isNext(s1: String, s2: String): Boolean = {
    s1
      .zip(s2)
      .map({ case (x,y) if x != y => 1 case _ => 0  })
      .sum == 1
  }


  class Queue() {
    private val _visited = mutable.ListBuffer[String]()
    private val queue = mutable.Queue[String]()

    def enqueue(word: String): Unit = {
      queue.addOne(word)
      _visited.addOne(word)
    }

    def enqueueAll(words: String*): Unit = {
      queue.addAll(words)
      _visited.addAll(words)
    }

    def dequeue(): String = {
      queue.dequeue()
    }

    def nonEmpty: Boolean = {
      queue.nonEmpty
    }

    def visited: mutable.Seq[String] = _visited

  }

}
