package checkers
import Type._;

class Board {
  var board = Array.ofDim[Element](8,8);
  var whites, blacks, whitesQ, blacksQ=0;

  def setUpBoard() : Unit = {
    whites = 12;
    blacks = 12;
    whitesQ = 0;
    blacksQ = 0;
    for (i <- 0 to (board.length-1)){
      for (j <- 0 to (board.length-1)) {
        board(i)(j) = new Element();
      }
    }
    for (i <- 0 to 2){
      for (j <- 0 to (board.length-1)) {
        if ((i%2==0 && j%2==0) || (i%2==1 && j%2==1))
          board(i)(j).setType(black);
      }
    }
    for (i <- 5 to 7){
      for (j <- 0 to (board.length-1)) {
        if ((i%2==0 && j%2==0) || (i%2==1 && j%2==1))
          board(i)(j).setType(white);
      }
    }

  }


  def printBoard() : Unit = {
    for (i <- 0 to (board.length-1)){
      for (j <- 0 to (board.length-1)) {
        print(" "+ board(i)(j).printElement());
      }
      println();
    }
  }
}
