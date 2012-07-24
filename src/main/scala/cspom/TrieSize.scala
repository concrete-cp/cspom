package cspom

import scala.util.Random
import cspom.extension.HashTrie
import scala.annotation.tailrec

object TrieSize extends App {
  val d = 2
  //val k = 20
  //val lambda = 100000
  val t = .001

  val r = new Random()

  for (k <- 10 to 100) {
    val lambda = (math.pow(d, k) * t).toInt

    val trie = fill(lambda, k, d)

    println(k + " " + trie.nodes)
  }

  def rTuple(k: Int, d: Int) = List.fill(k)(r.nextInt(d))

  @tailrec
  def fill(lambda: Int, k: Int, d: Int, t: HashTrie = HashTrie.empty): HashTrie = {
    if (t.size >= lambda) t
    else fill(lambda, k, d, t + (rTuple(k, d): _*))
  }

}