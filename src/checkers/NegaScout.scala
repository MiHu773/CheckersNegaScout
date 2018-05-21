package checkers

import Type._;
class NegaScout {
  def getBestMove(board: Board) : String = {
      val random = scala.util.Random;
      val allMoves = board.getAllPossibleMoves(black);
      val r = random.nextInt(allMoves.length);
      val tree = buildTree(board);
      return allMoves(r);
  }

  def buildTree(board:Board) :Tree={
    new Tree(new Node(board, null, 0, black))
  }
}
