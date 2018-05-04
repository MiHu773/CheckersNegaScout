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

  def checkIfMoveCorrect(moves:String): Boolean = {
        return true // TODO checking if move is correct
  }

  def makeMoveSequence(moves: String): Unit ={
    if (!checkIfMoveCorrect(moves)){
      return; //TODO error checking (throwing exception?)
    }
    var positionsList = moves.split(" ");
    for (i<-1 to (positionsList.length-1)){
      makeMove(positionsList(i-1), positionsList(i))
    }
  }
  def makeMove(moves:(String, String)) : Unit ={

    var i1 = moves._1.charAt(0).asDigit;
    var j1 = moves._1.charAt(1).asDigit;
    var i2 = moves._2.charAt(0).asDigit;
    var j2 = moves._2.charAt(1).asDigit;
    var movedType = board(i1)(j1).elementType;
    board(i1)(j1).setType(null);
    board(i2)(j2).setType(movedType);
    if (math.abs(i1-i2)>1){ //jump
      if ((i2 > i1) && (j2>j1)){
        board(i1+1)(j1+1).setType(null);
      } else if ((i2 > i1) && (j2<j1)){
        board(i1+1)(j1-1).setType(null);
      } else if ((i2 < i1) && (j2>j1)){
        board(i1-1)(j1+1).setType(null);
      } else {
        board(i1-1)(j1-1).setType(null);
      }
    }

  }
  def printBoard() : Unit = {
    println("  0 1 2 3 4 5 6 7");
    for (i <- 0 to (board.length-1)){
      print (i);
      for (j <- 0 to (board.length-1)) {
        print(" "+ board(i)(j).printElement());
      }
      println();
    }
  }
}
