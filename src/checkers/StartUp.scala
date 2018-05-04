package checkers


object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    var negaScout = new NegaScout();
    board.setUpBoard();
    board.printBoard();

    while (!board.isFinished()) {
      print("Enter move: "); // input form ex. 60 51
      var input = scala.io.StdIn.readLine();
      if (!board.checkIfMoveCorrect(input)){
        println("wrong move!");
      }
      else {
        board.makeMoveSequence(input);
        var enemysMove = negaScout.getBestMove(board);
        board.makeMoveSequence(enemysMove);
        board.printBoard();
      }
    }

    print("End of the game!");
  }
}
