package checkers;

import Type._

class NegaScout {
 def getBestMove(board: Board) : String = {
      val random = scala.util.Random;
      val allMoves = board.getAllPossibleMoves(white);
      val r = random.nextInt(allMoves.length);
      return allMoves(r);
  }

}
