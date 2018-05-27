package checkers

import checkers.Type._
import checkers.findingNextMove.{Move, MoveSet}

class Board() {
  val board = Array.ofDim[Element](8, 8);

  def setUpBoardCopy(board: Board): Unit = {
    for (i <- board.board.indices; j <- board.board.indices)
      this.board(i)(j) = new Element(board.board(i)(j).elementType, board.board(i)(j).posX, board.board(i)(j).posY)
  }

  def checkIfMoveCorrect(moves: String, color: Type.Type): Boolean = {
    val posMove = getAllPossibleMoves(color);
    //println("possible player moves:" + posMove)
    if (posMove.contains(moves))
      return true
    false
  }

  def makeMoveSequence(moves: String): Unit = {
    val positionsList = moves.split(" ")
    for (i <- 1 until positionsList.length) {
      makeMove(positionsList(i - 1), positionsList(i))
    }
    val finalPositionI = positionsList.last.charAt(0).asDigit;
    val finalPositionJ = positionsList.last.charAt(1).asDigit;

    checkIfChangeToQueen(finalPositionI, finalPositionJ, board(finalPositionI)(finalPositionJ).elementType);
  }

  def makeMove(moves: (String, String)): Unit = {
    val i1 = moves._1.charAt(0).asDigit
    val j1 = moves._1.charAt(1).asDigit
    val i2 = moves._2.charAt(0).asDigit
    val j2 = moves._2.charAt(1).asDigit
    board(i2)(j2) = new Element(board(i1)(j1).elementType, i2, j2)
    if (math.abs(i1 - i2) > 1) { //jump
      if ((i2 > i1) && (j2 > j1)) {
        //for (i <- i1 until i2; j <- j1 until j2)
        for {
          (i, j) <- (i1 until i2) zip (j1 until j2)
        } {
          removeTile(i, j)
        }
      } else if ((i2 > i1) && (j2 < j1)) {
        //for (i <- i1 until i2; j <- j1 until j2 by -1) {
        for {
          (i, j) <- (i1 until i2) zip (j1 until j2 by -1)
        } {
          removeTile(i, j)
        }
      } else if ((i2 < i1) && (j2 > j1)) {
        //for (i <- i1 until i2 by -1; j <- j1 until j2) {
        for {
          (i, j) <- (i1 until i2 by -1) zip (j1 until j2)
        } {
          removeTile(i, j)
        }
      } else {
        //for (i <- i1 until i2 by -1; j <- j1 until j2 by -1) {
        for {
          (i, j) <- (i1 until i2 by -1) zip (j1 until j2 by -1)
        } {
          removeTile(i, j)
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

  def isFinished(color: Type.Type): Boolean = {
    if ((getNumberOfElems(white) + getNumberOfElems(whiteQueen) == 0) || (getNumberOfElems(black) + getNumberOfElems(blackQueen) == 0))
      return true;
    if (getAllPossibleMoves(color).isEmpty) return true
    false;
  }

  def printAllMoveSetsForColor(color: Type) = for (x <- getAllMoveSetsForColor(color)) x.printPossibleMoves()

  def getAllMoveSetsForColor(color: Type): List[MoveSet] = {
    //val res: List[MoveSet] = List[MoveSet]()
    val res = for (x <- board.indices; y <- board.indices if board(x)(y).elementType == color) yield new MoveSet(board(x)(y), this);
    val resList = res.toList
    resList;
  }

  def getAllPossibleMoves(color: Type.Type): List[String] = {
    if (color == white) return getAllPossibleMovesOfType(white) ++ getAllPossibleMovesOfType(whiteQueen);
    getAllPossibleMovesOfType(black) ++ getAllPossibleMovesOfType(blackQueen);
  }

  def getAllPossibleMovesOfType(color: Type): List[String] = {
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
    if (moves.isEmpty) return "";
    val check:String = moves.map(m => m.end._1.toString + m.end._2.toString + " ").reduce((x, y) => y+x)
    moves.last.start._1.toString + moves.last.start._2.toString + " " + check.substring(0, check.length - 1)
  }

  def heuristic(): Int = {
    getNumberOfElems(black) + getNumberOfElems(blackQueen) * 3 - getNumberOfElems(white) + getNumberOfElems(whiteQueen) * 3;
  }
}
