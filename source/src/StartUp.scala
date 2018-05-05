package checkers

object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    board.setUpBoard3();
    board.printBoard();

    while (true) {
      print(board.possibleMoves("4", "2"))
      print("\nEnter move: "); // input form ex. 60 51
      var input = readf2("{0} {1}").asInstanceOf[(String, String)];
      board.makeMove(input);
      board.printBoard();
    }
  }
}
