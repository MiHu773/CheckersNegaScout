package checkers
import checkers.Type._

import scala.collection.mutable.Queue
import src.{Move, MoveSet}


 /*def setUpBoard1() : Unit = {
    whites = 1;
    blacks = 4;
    whitesQ = 0;
    blacksQ = 0;
    for (i <- 0 to (board.length-1)){
      for (j <- 0 to (board.length-1)) {
        board(i)(j) = new Element();
      }
    }
    board(4)(2).setType(white)

    board(3)(3).setType(black)
    board(3)(1).setType(black)
    board(5)(1).setType(black)
    board(5)(3).setType(black)
  }

  def setUpBoard2() : Unit = {
    whites = 1;
    blacks = 4;
    whitesQ = 0;
    blacksQ = 0;
    for (i <- 0 to (board.length-1)){
      for (j <- 0 to (board.length-1)) {
        board(i)(j) = new Element();
      }
    }
    board(4)(2).setType(black)

    board(3)(3).setType(black)
    board(3)(1).setType(black)
    board(5)(1).setType(black)
    board(5)(3).setType(black)
  }

  def setUpBoard3() : Unit = {
    whites = 1;
    blacks = 4;
    whitesQ = 0;
    blacksQ = 0;
    for (i <- 0 to (board.length-1)){
      for (j <- 0 to (board.length-1)) {
        board(i)(j) = new Element();
      }
    }
    board(4)(2).setType(black)

    board(3)(3).setType(white)
    board(3)(1).setType(white)
    board(5)(1).setType(white)
    board(5)(3).setType(white)
  }*/

case class Board() {
    var board = Array.ofDim[Element](8, 8);

/*   def copy () : Board ={
      var cp = new Board()
      cp.board =  this.board.clone()
      cp
    }*/

    def setUpBoard6() : Unit = {
      for (i <- board.indices; j <- board.indices)
        board(i)(j) = new Element(null, i, j)

      board(1)(0) = new Element(white, 1, 0)

      board(2)(1) = new Element(black, 2, 1)
      board(4)(3) = new Element(black, 4, 3)
      board(6)(5) = new Element(black, 6, 5)
      board(6)(3) = new Element(black, 6, 3)
    }
    def setUpBoard4() : Unit = {
      for (i <- board.indices; j <- board.indices)
        board(i)(j) = new Element(null, i, j)

      board(3)(2) = new Element(white, 3, 2)

      board(4)(3) = new Element(black, 4, 3)
      board(2)(3) = new Element(black, 2, 3)
      board(2)(5) = new Element(black, 2, 5)

    }

    def setUpBoard5() : Unit = {
      for (i <- board.indices; j <- board.indices)
        board(i)(j) = new Element(null, i, j)

      board(2)(2) = new Element(white, 0, 0)

      board(1)(1) = new Element(black, 1, 1)
      board(3)(3) = new Element(black, 3, 3)
      board(1)(3) = new Element(black, 1, 3)
      board(1)(5) = new Element(black, 1, 5)

    }
    def setUpBoard() : Unit = {
      for (i <- board.indices; j <- board.indices)
        board(i)(j) = new Element(null, i, j)

      for (i <- 0 to 2; j<-board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
        board(i)(j)=new Element(black, i, j)

      for (i <- 5 to 7; j <- board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
        board(i)(j)= new Element(white, i, j)
    }
    def setUpBoardCopy(board: Board) : Unit = {
      for (i <- board.board.indices; j <- board.board.indices)
        this.board(i)(j) = new Element(board.board(i)(j).elementType, board.board(i)(j).posX, board.board(i)(j).posY)

      /*for (i <- 0 to 2; j<-board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
        board(i)(j)=new Element(black, i, j)

      for (i <- 5 to 7; j <- board.indices; if i%2==0 && j%2==0 || i%2==1 && j%2==1)
        board(i)(j)= new Element(white, i, j)*/
    }
    def checkIfMoveCorrect(moves:String): Boolean = {
      return true; // TODO checking if move is correct
    }

    def makeMoveSequence(moves: String): Unit ={
      val positionsList = moves.split(" ")
      for (i<-1 until positionsList.length){
        makeMove(positionsList(i-1), positionsList(i))
      }
    }

    def makeMove(moves:(String, String)) : Unit ={
      val i1 = moves._1.charAt(0).asDigit
      val j1 = moves._1.charAt(1).asDigit
      val i2 = moves._2.charAt(0).asDigit
      val j2 = moves._2.charAt(1).asDigit
      val movedType = checkIfChangeToQueen(i2, board(i1)(j1).elementType);
      board(i2)(j2)= new Element(movedType, i2, j2)
      if (math.abs(i1-i2)>1){ //jump
        if ((i2 > i1) && (j2>j1)){
          var j = j1
          for (i <- i1 to (i2-1)){
            removeTile(i,j)
            j+=1
          }
        } else if ((i2 > i1) && (j2<j1)){
          var j = j1
          for (i <- i1 to (i2-1)){
            removeTile(i,j)
            j-=1
          }
        } else if ((i2 < i1) && (j2>j1)){
          var j = j2-1
          for (i <- (i2+1) to i1){
            removeTile(i,j)
            j-=1
          }
        } else {
          var j = j2+1;
          for (i <- (i2+1) to i1){
            removeTile(i,j);
            j+=1;
          }
        }
      } else board(i1)(j1) = new Element(null, i1, j1)

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

    def getElement(x: Int, y: Int) : Element = {
      board(x)(y)
    }

    def removeTile(i: Int,j: Int): Unit = {
      board(i)(j)=new Element(null, i, j)
    }

    def takeElement(i: Int, j: Int): Element = board(i)(j)

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