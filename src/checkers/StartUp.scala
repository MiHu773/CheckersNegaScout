package checkers

import Type._
import util.control.Breaks._

object StartUp {
  def main(args: Array[String]): Unit = {
    var board = new Board()
    val negaScout = new NegaScout()
    board.setUpBoard()
    board.printBoard()

    breakable {
      while (!board.isFinished(white)) {
        println("Enter move: "); // input form ex. 60 51
        println(board.getAllPossibleMoves(white))
        val input = scala.io.StdIn.readLine()
        if (!board.checkIfMoveCorrect(input, white)) {
          println("wrong move!")
        }
        else {
          board.makeMoveSequence(input)
          board.printBoard()
          if (board.isFinished(black)) break;
          println("enemy thinking...")
          val enemysMove = negaScout.getBestMove(board)
          println("enemy making move: " + enemysMove)
          board.makeMoveSequence(enemysMove);
          board.printBoard()
        }
      }
    }

    print("End of the game!")
  }
}
