package checkers

import scala.util.Random;

class NegaScout {
  def getBestMove(board: Board) : String = {
      val random = scala.util.Random;
      val allMoves = board.getAllPossibleMoves();
      val r = random.nextInt(allMoves.length);
      return allMoves(r);
  }

}
