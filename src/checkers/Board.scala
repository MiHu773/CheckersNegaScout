package checkers

import checkers.Type._
import checkers.findingNextMove.{Move, MoveSet}

/**
  * Class containing board for checkers and functions for applying moves
  */
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

  def setUpBoard10(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    for (i <- 2 to 2; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(black, i, j)

    board(3)(1) = new Element(white, 3, 1)
    board(2)(0) = new Element(null, 3, 1)
  }

  /**
    * Configuring a board before the game
    */
  def setUpBoard(): Unit = {
    for (i <- board.indices; j <- board.indices)
      board(i)(j) = new Element(null, i, j)

    for (i <- 0 to 2; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(black, i, j)

    for (i <- 5 to 7; j <- board.indices; if i % 2 == 0 && j % 2 == 0 || i % 2 == 1 && j % 2 == 1)
      board(i)(j) = new Element(white, i, j)
  }

  /**
    * Copying board configuration
    * @param board board to copy
    */
  def setUpBoardCopy(board: Board): Unit = {
    for (i <- board.board.indices; j <- board.board.indices)
      this.board(i)(j) = new Element(board.board(i)(j).elementType, board.board(i)(j).posX, board.board(i)(j).posY)
  }

  /**
    * Checking if move made by human player is correct
    * @param moves move made by player
    * @param color player's side
    * @return true if move is in the list of possible moves else false
    */
  def checkIfMoveCorrect(moves: String, color: Type.Type): Boolean = {
    val posMove = getAllPossibleMoves(color);
    //println("possible player moves:" + posMove)
    if (posMove.contains(moves))
  }

  /**
    * Making sequence of moves (including jumping)
    * @param moves moves to make
    */
  def makeMoveSequence(moves: String): Unit = {
    val positionsList = moves.split(" ")
    for (i <- 1 until positionsList.length) {
      makeMove(positionsList(i - 1), positionsList(i))
    }
    val finalPositionI = positionsList.last.charAt(0).asDigit;
    val finalPositionJ = positionsList.last.charAt(1).asDigit;

    checkIfChangeToQueen(finalPositionI, finalPositionJ, board(finalPositionI)(finalPositionJ).elementType);
  }

  /**
    * Making single move (one jump or one step)
    * @param moves move to make
    */
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

  /**
    * Changing tile to queen if checker gets to the barier
    * @param i row
    * @param j column
    * @param value player's side
    */
  def checkIfChangeToQueen(i: Int, j: Int, value: Type.Type): Unit = {
    if (i == 7 && value == black) {
      board(i)(j) = new Element(blackQueen, i, j);
    }
    else if (i == 0 && value == white) {
      board(i)(j) = new Element(whiteQueen, i, j)
    }
  }

  /**
    * Getting number of checkers of specific type
    * @param mType type for checking
    * @return number of checkers of specific types
    */
  def getNumberOfElems(mType: Type.Type): Int = {
    val amountsInR = board.map(r => r.count(_.elementType == mType))
    amountsInR.sum;
  }

  /**
    * Getting element at x,y coordinates
    * @param x row
    * @param y column
    * @return element at x,y
    */
  def getElement(x: Int, y: Int): Element = {
    board(x)(y)
  }

  /**
    * Removing element after jump move from the enemy
    * @param i row
    * @param j column
    */
  def removeTile(i: Int, j: Int): Unit = {
    board(i)(j) = new Element(null, i, j)
  }

  /**
    * Printing board
    */
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

  /**
    * Checking if game is finished (0 elements of one of players or player can't make move)
    * @param color player's side
    * @return true if end of the game else false
    */
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

  /**
    * Getting all possible moves for player
    * @param color player's side
    * @return List of all possible moves
    */
  def getAllPossibleMoves(color: Type.Type): List[String] = {
    if (color == white) return getAllPossibleMovesOfType(white) ++ getAllPossibleMovesOfType(whiteQueen);
    getAllPossibleMovesOfType(black) ++ getAllPossibleMovesOfType(blackQueen);
  }

  /**
    * Getting all possible for one type of tiles
    * @param color type of tile
    * @return List of all possible moves for type of tile
    */
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

  /**
    * Translating between MoveSet notation and String notation
    * @param moves Move list
    * @return moves in String list
    */
  def createStringMove(moves: List[Move]): String = {
    if (moves.isEmpty) return "";
    val check:String = moves.map(m => m.end._1.toString + m.end._2.toString + " ").reduce((x, y) => y+x)
    moves.last.start._1.toString + moves.last.start._2.toString + " " + check.substring(0, check.length - 1)
  }

  /**
    * Heuristic function for NegaScout algorithm
    * @return heuristic value of board
    */
  def heuristic(): Int = {
    getNumberOfElems(black) + getNumberOfElems(blackQueen) * 3 - getNumberOfElems(white) + getNumberOfElems(whiteQueen) * 3;
  }
}
