package checkers

import checkers.findingNextMove.MoveSet


object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    var negaScout = new NegaScout();
    board.setUpBoard();
    board.printBoard();

    while (!board.isFinished()) {
      print("Enter move: "); // input form ex. 60 51
      val x = board.getAllMoveSetsForColor(Type.white)
      println(board.getAllPossibleMoves(Type.white));
      var input = scala.io.StdIn.readLine();
      if (!board.checkIfMoveCorrect(input, Type.white)){
        println("wrong move!");
      }
      else {
        board.makeMoveSequence(input);
        board.printBoard();
        println("enemy making move")
        var enemysMove = negaScout.getBestMove(board);
        board.makeMoveSequence(enemysMove);
        board.printBoard();
      }
    }

    print("End of the game!");
  }
}
