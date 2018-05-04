package checkers

object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    board.setUpBoard();
    board.printBoard();

    while (true) {
      print("Enter move: "); // input form ex. 60 51
      // var input = readf2("{0} {1}").asInstanceOf[(String, String)];
      var input = scala.io.StdIn.readLine();
      board.makeMoveSequence(input);
      board.printBoard();
    }
  }
}
