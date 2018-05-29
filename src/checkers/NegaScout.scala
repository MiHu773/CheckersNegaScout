package checkers

;

import Type._

import scala.annotation.tailrec

/**
  * Class implementing NegaScout algorithm
  */
class NegaScout {
  /**
    * Getting best move for computer player
    * @param board board before computer's move
    * @return best move
    */
  def getBestMove(board: Board): String = {
    val tree = buildTree(board);
    val movesBest = tree.head.children.map(ns(_, 3, -40, 40,1))
    //println(tree.head.children(tree.head.children.indices.maxBy(movesBest)).boardMove._2)
    return tree.head.children(tree.head.children.indices.maxBy(movesBest)).boardMove._2;
  }

  /**
    * Building tree for NegaScout search
    * @param board board before the move
    * @return whole tree
    */
  def buildTree(board: Board): Tree = {
    new Tree(new Node((board, ""), null, 0, black))
  }

  /**
    * NegaScout algorithm returning best move
    * @param node node of the NegaScout tree
    * @param depth depth of search
    * @param a alfa
    * @param b beta
    * @param color type of player
    * @return heuristic value of the best move
    */
  def ns(node: Node, depth: Int, a: Int, b: Int, color: Int): Int = {
    if (node.children.isEmpty || depth == 0) return color * node.boardMove._1.heuristic();
    countScoreForEach(node.children, 0,depth, a,b,color)
  }

  /**
    * Apply NegaScout algorithm for each children
    * @param nodes children
    * @param index index of children
    * @param depth depth in NegaScout tree
    * @param a alfa
    * @param b beta
    * @param color type of the tile
    * @return heuristic value
    */
  @tailrec
  final def countScoreForEach(nodes: Seq[Node], index: Int, depth: Int, a: Int, b: Int, color: Int): Int = {
    val score = {
      if (index == 0) -ns(nodes(0), depth - 1, -b, -a, -color)
      else {
        val scoreTemp = -ns(nodes(index), depth - 1, -a - 1, -a, -color)
        if (a < scoreTemp && scoreTemp < b) -ns(nodes(index), depth - 1, -b, -scoreTemp, -color)
        else scoreTemp
      }
    }
    val anext = math.max(a,score);
    if (anext>b || index==nodes.length-1) return anext;
    countScoreForEach(nodes, index+1, depth, anext, b, color)
  }
}
