package checkers

object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board();
    board.setUpBoard();
    board.printBoard();
  }
}
