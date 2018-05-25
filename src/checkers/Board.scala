package checkers

import checkers.Type._
import checkers.findingNextMove.{Move, MoveSet}

class Board() {
  val board = Array.ofDim[Element](8, 8);

  def setUpBoard9(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    board(3)(3) = new Element(whiteQueen, 3, 3)

    board(1)(1) = new Element(black, 1, 1)
  }

  def setUpBoard8(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    board(5)(5) = new Element(black, 5, 5)
    board(3)(3) = new Element(whiteQueen, 3, 3)
    board(6)(0) = new Element(black, 6, 0)
    board(0)(0) = new Element(black, 0, 0)
    board(1)(1) = new Element(black, 1, 1)
    board(5)(1) = new Element(black, 5, 1)
    board(7)(1) = new Element(black, 7, 1)

    board(0)(6) = new Element(black, 0, 6)
    board(2)(6) = new Element(black, 2, 6)
    board(4)(6) = new Element(black, 4, 6)
    board(4)(4) = new Element(white, 4, 4)
  }

  def setUpBoard7(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    for (i <- 0 to 1; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(black, i, j)

    for (i <- 6 to 7; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(white, i, j)

    board(6)(4) = new Element(white, 6, 4)
    board(2)(0) = new Element(white, 2, 0)
    board(3)(1) = new Element(white, 3, 1)

    board(5)(5) = new Element(black, 5, 5)
    board(3)(5) = new Element(black, 3, 5)
    board(3)(3) = new Element(black, 3, 3)
    board(5)(7) = new Element(white, 5, 7)

  }

  def setUpBoard6(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    board(1)(2) = new Element(white, 1, 2)

    board(2)(1) = new Element(black, 2, 1)
    board(4)(3) = new Element(black, 4, 3)
    board(6)(5) = new Element(black, 6, 5)
    board(6)(3) = new Element(black, 6, 3)
    board(2)(3) = new Element(black, 2, 3)
  }

  def setUpBoard4(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    board(3)(2) = new Element(white, 3, 2)

    board(4)(3) = new Element(black, 4, 3)
    board(2)(3) = new Element(black, 2, 3)
    board(2)(5) = new Element(black, 2, 5)

  }

  def setUpBoard5(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    board(2)(2) = new Element(white, 0, 0)

    board(1)(1) = new Element(black, 1, 1)
    board(3)(3) = new Element(black, 3, 3)
    board(1)(3) = new Element(black, 1, 3)
    board(1)(5) = new Element(black, 1, 5)

  }

  def setUpBoard(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    for (i <- 0 to 2; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(black, i, j)

    for (i <- 5 to 7; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(white, i, j)
  }

  def setUpBoardCopy(board: Board): Unit = {
    for (i <- board.board.indices; j <- board.board.indices)
      this.board(i)(j) = new Element(board.board(i)(j).elementType, board.board(i)(j).posX, board.board(i)(j).posY)
  }

  def checkIfMoveCorrect(moves: String, color: Type.Type): Boolean = {
    val posMove = getAllPossibleMoves(color);
    println("possible player moves:" + posMove)
    if (posMove.contains(moves))
      return true;
    false;
  }

  def makeMoveSequence(moves: String): Unit = {
    val positionsList = moves.split(" ")
    for (i <- 1 until positionsList.length) {
     // TODO change back makeMove(positionsList(i - 1), positionsList(i))
      makeMove2(positionsList(i - 1), positionsList(i))
    }
    val finalPositionI = positionsList.last.charAt(0).asDigit;
    val finalPositionJ = positionsList.last.charAt(1).asDigit;

    //checkIfChangeToQueen(finalPositionI, finalPositionJ, board(finalPositionI)(finalPositionJ).elementType); TODO odkomentować jak pojawi sie mozliwy ruch dla krolowych
  }

  def makeMove(moves: (String, String)): Unit = {
    val i1 = moves._1.charAt(0).asDigit
    val j1 = moves._1.charAt(1).asDigit
    val i2 = moves._2.charAt(0).asDigit
    val j2 = moves._2.charAt(1).asDigit
    board(i2)(j2) = new Element(board(i1)(j1).elementType, i2, j2)
    if (math.abs(i1 - i2) > 1) { //jump
      if ((i2 > i1) && (j2 > j1)) {
        var j = j1
        for (i <- i1 to (i2 - 1)) {
          removeTile(i, j)
          j += 1
        }
      } else if ((i2 > i1) && (j2 < j1)) {
        var j = j1
        for (i <- i1 to (i2 - 1)) {
          removeTile(i, j)
          j -= 1
        }
      } else if ((i2 < i1) && (j2 > j1)) {
        var j = j2 - 1
        for (i <- (i2 + 1) to i1) {
          removeTile(i, j)
          j -= 1
        }
      } else {
        var j = j2 + 1;
        for (i <- (i2 + 1) to i1) {
          removeTile(i, j);
          j += 1;
        }
      }
    } else board(i1)(j1) = new Element(null, i1, j1)
  }

  def makeMove2(moves: (String, String)): Unit = {
    val i1 = moves._1.charAt(0).asDigit
    val j1 = moves._1.charAt(1).asDigit
    val i2 = moves._2.charAt(0).asDigit
    val j2 = moves._2.charAt(1).asDigit
    board(i2)(j2) = new Element(board(i1)(j1).elementType, i2, j2)
    if (math.abs(i1 - i2) > 1) { //jump
      if ((i2 > i1) && (j2 > j1)) {
        for (i <- i1 until i2; j<-j1 until j2) {
          removeTile(i, j)
        }
      } else if ((i2 > i1) && (j2 < j1)) {
        for (i <- i1 until i2; j<- j1 until j2 by -1) { //TODO czy można tak do tylu?
          removeTile(i, j)
        }
      } else if ((i2 < i1) && (j2 > j1)) {
        for (i <- i1 until i2 by -1; j <- j1 until j2) {
          removeTile(i, j)
        }
      } else {
        for (i <- i1 until i2 by -1; j<-j1 until j2 by -1) {
          removeTile(i, j);
        }
      }
    } else board(i1)(j1) = new Element(null, i1, j1)
  }



  def checkIfChangeToQueen(i: Int, j: Int, value: Type.Type): Unit = {
    if (i == 7 && value == black) {
      board(i)(j) = new Element(blackQueen, i, j);
    }
    else if (i == 0 && value == white) {
      board(i)(j) = new Element(whiteQueen, i, j)
    }
  }


  def getNumberOfElems(mType: Type.Type): Int = {
    val amountsInR = board.map(r => r.count(_.elementType == mType))
    amountsInR.sum;
  }

  def getElement(x: Int, y: Int): Element = {
    board(x)(y)
  }

  def removeTile(i: Int, j: Int): Unit = {
    board(i)(j) = new Element(null, i, j)
  }


  def printBoard(): Unit = {
    println("  0 1 2 3 4 5 6 7");
    for (i <- board.indices) {
      print(i);
      for (j <- board.indices) {
        print(" " + board(i)(j).printElement());
      }
      println();
    }
  }

  def isFinished(): Boolean = {
    if ((getNumberOfElems(white) + getNumberOfElems(whiteQueen) == 0) || (getNumberOfElems(black) + getNumberOfElems(blackQueen) == 0))
      return true;
    false;
  }

  def printAllMoveSetsForColor(color: Type) = for (x <- getAllMoveSetsForColor(color)) x.printPossibleMoves()

  def getAllMoveSetsForColor(color: Type): List[MoveSet] = {
    //val res: List[MoveSet] = List[MoveSet]()
    val res = for (x <- board.indices; y <- board.indices if board(x)(y).elementType == color) yield new MoveSet(board(x)(y), this);
    val resList = res.toList
    resList;
  }

  def getAllPossibleMovesForColor(color: Type): List[List[List[Move]]] = {
    for (ms <- getAllMoveSetsForColor(color)) yield {
      ms.possibleMoves()
      //.foreach(_.foreach())
    }
  }

  def getAllPossibleMoves(color: Type): List[String] = {
    val moves = getAllPossibleMovesForColor(color)
      .map(_.map(createStringMove));
    moves.flatten


  }

  def getAllPossibleMovesForColor(color: Type): List[List[List[Move]]] = {
    for (ms <- getAllMoveSetsForColor(color)) yield {
      ms.possibleMoves()
    }
  }

  def createStringMove(moves: List[Move]): String = {
    var moveString = "";
    if (moves.isEmpty) return moveString;
    for (move <- moves) {
      moveString = move.end._1.toString + move.end._2.toString + " " + moveString;
    }
    moves.last.start._1.toString + moves.last.start._2.toString + " " + moveString.substring(0, moveString.length - 1)
  }

  def heuristic(): Int = {
    getNumberOfElems(black) + getNumberOfElems(blackQueen) * 3 - getNumberOfElems(white) + getNumberOfElems(whiteQueen) * 3;
  }
}
