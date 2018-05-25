package checkers

;

import Type._

import scala.annotation.tailrec

class NegaScout {
  def getBestMove(board: Board): String = {
    val tree = buildTree(board);
    val movesBest = tree.head.children.map(ns(_, 4, -40, 40,1))
    println(tree.head.children(tree.head.children.indices.maxBy(movesBest)).boardMove._2)
    return tree.head.children(tree.head.children.indices.maxBy(movesBest)).boardMove._2;//TODO wez najlepszy ruch ale chyba trzeba to zmienic na odpalanie ns z heada
    //TODO sprawdzic czy robi dobre ruchy
  }

  def buildTree(board: Board): Tree = {
    new Tree(new Node((board, ""), null, 0, black))
  }

  def ns(node: Node, depth: Int, a: Int, b: Int, color: Int): Int = {
    if (node.children.isEmpty || depth == 0) return color * node.boardMove._1.heuristic();
    countScoreForEach(node.children, 0,depth, a,b,color)
  }

  @tailrec
  final def countScoreForEach(nodes: Seq[Node], index: Int, depth: Int, a: Int, b: Int, color: Int): Int = {
    val score = {
      if (index == 0) -ns(nodes(0), depth - 1, -b, -a, -color)
      else {
        val scoreTemp = -ns(nodes(index), depth - 1, -a - 1, -a, -color)
        if (a < scoreTemp && scoreTemp < b) -ns(nodes(0), depth - 1, -b, -scoreTemp, -color)
        else scoreTemp
      }
    }
    val anext = math.max(a,score);
    if (anext>b || index==nodes.length-1) return anext;
    countScoreForEach(nodes, index+1, depth, anext, b, color)
  }
}
