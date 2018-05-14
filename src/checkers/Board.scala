package checkers
import Type._;

class Board {
  val board = Array.ofDim[Element](8,8);

  def setUpBoard() : Unit = {
    for (i <- board.indices; j <- board.indices)
        board(i)(j) = new Element(null);

    for (i <- 0 to 2; j<-board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
          board(i)(j)=new Element(black);

    for (i <- 5 to 7; j <- board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
          board(i)(j)= new Element(white);;
  }

  def checkIfMoveCorrect(moves:String): Boolean = {
        return true; // TODO checking if move is correct
  }

  def makeMoveSequence(moves: String): Unit ={
    val positionsList = moves.split(" ");
    for (i<-1 until positionsList.length){
      makeMove(positionsList(i-1), positionsList(i))
    }
  }

  def makeMove(moves:(String, String)) : Unit ={
    val i1 = moves._1.charAt(0).asDigit;
    val j1 = moves._1.charAt(1).asDigit;
    val i2 = moves._2.charAt(0).asDigit;
    val j2 = moves._2.charAt(1).asDigit;
    val movedType = checkIfChangeToQueen(i2, board(i1)(j1).elementType);
    board(i2)(j2)= new Element(movedType);
    if (math.abs(i1-i2)>1){ //jump
      if ((i2 > i1) && (j2>j1)){
        var j = j1;
        for (i <- i1 to (i2-1)){
           removeTile(i,j);
           j+=1;
        }
      } else if ((i2 > i1) && (j2<j1)){
        var j = j1;
        for (i <- i1 to (i2-1)){
            removeTile(i,j);
            j-=1;
        }
      } else if ((i2 < i1) && (j2>j1)){
        var j = j2-1;
        for (i <- (i2+1) to i1){
            removeTile(i,j);
            j-=1;
        }
      } else {
        var j = j2+1;
        for (i <- (i2+1) to i1){
            removeTile(i,j);
            j+=1;
        }
      }
    } else board(i1)(j1) = new Element(null);

  }

  def checkIfChangeToQueen(i: Int, value: Type.Type): Type ={
    if (i==7 && value == black) {
      return blackQueen;
    }
    else if (i==0 && value == white){
      return whiteQueen;
    }
    return value;
  }


  def getNumberOfElems(mType: Type.Type): Int ={
    val amountsInR = board.map(r => r.count(_.elementType == mType))
    amountsInR.sum;
  }

  def removeTile(i: Int,j: Int): Unit = {
    board(i)(j)=new Element(null);
  }

  def printBoard() : Unit = {
    println("  0 1 2 3 4 5 6 7");
    for (i <- board.indices){
      print (i);
      for (j <- board.indices) {
        print(" "+ board(i)(j).printElement());
      }
      println();
    }
  }

  def isFinished(): Boolean ={
    if ((getNumberOfElems(white) + getNumberOfElems(whiteQueen) == 0) || (getNumberOfElems(black) + getNumberOfElems(blackQueen) == 0))
      return true;
    false;
  }

  def getAllPossibleMoves(): List[String] ={
    List("20 31", "22 33");
  }
}
