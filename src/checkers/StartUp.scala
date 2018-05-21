package checkers


object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    var negaScout = new NegaScout();
//    board.setUpBoard();
    board.setUpBoard6()
    board.printBoard()
    val y = board.getAllPossibleMovesForColor(Type.white);
    val x = board getAllMoveSetsForColor(Type.white)
    val z = board.getAllPossibleMoves(Type.white);
    val b = 1;
//    val x = new MoveSet(board.getElement(1, 0), board)
//    println(x.possibleMoves())

//    x.printPossibleMoves()
    /*while (!board.isFinished()) {
      print("Enter move: "); // input form ex. 60 51
      var input = scala.io.StdIn.readLine();
      if (!board.checkIfMoveCorrect(input)) {
        println("wrong move!");
      }
      else {
        board.makeMoveSequence(input);
        var enemysMove = negaScout.getBestMove(board);
        //board.makeMoveSequence(enemysMove);
        board.printBoard();
      }
      while (true) {
        //print(board.possibleMoves("4", "2"))
        print("\nEnter move: "); // input form ex. 60 51
        var input = readf2("{0} {1}").asInstanceOf[(String, String)];
        board.makeMove(input);
        board.printBoard();
      }

      print("End of the game!");
    }*/
  }
}
