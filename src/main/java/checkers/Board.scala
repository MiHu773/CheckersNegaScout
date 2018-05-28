package checkers

import checkers.Type._
import checkers.findingNextMove.{Move, MoveSet}

/**
  * Class containing board for checkers and functions for applying moves
  */
class Board() {
  val board = Array.ofDim[Element](8, 8);

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
      return true
    false
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

  /**
    * Prints all moves for defined piece type
    * @param color piece Type
    */
  def printAllMoveSetsForColor(color: Type) = {
    for (x <- getAllMoveSetsForColor(color)) x.printPossibleMoves()
  }

  /**
    * Lists MoveSet for each object of Type given in parameter
    * @param color Type of the elements for which we are searching for MoveSets
    * @return List of MoveSets
    */
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
    def getAllPossibleMovesForColor(color: Type): List[List[List[Move]]] = {
      for (ms <- getAllMoveSetsForColor(color)) yield {
        ms.possibleMoves()
      }
    }
    val moves = removeNonJumpsIfNeeded(getAllPossibleMovesForColor(color))
      .map(_.map(createStringMove));
    moves.flatten
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

  /**
    * Function, which checks whether removing unnecessary elements from move list is required.
    * It checks if there are any jumps.
    * @param list list of lists of moves
    * @return if there are any jumps, true, else false
    */
  def checkIfRemovingNeeded(list: List[List[List[Move]]]) : Boolean ={
    for(outer <- list; inner <- outer; move <- inner)
    {
      if (move.jump == true)
        return true
    }
    return false
  }

  /**
    * Method removes moves that are not jumps, from the list, if there is at least one jump in the move list.
    * @param list list of lists of moves
    * @return list of lists of lists of moves without moves, which are not jump
    */
  def removeNonJumpsIfNeeded(list: List[List[List[Move]]]) : List[List[List[Move]]] ={
    if (list == null) return null
    if (checkIfRemovingNeeded(list))
      list.map(_.map(_.filter(_.jump == true)))
          .map(_.filter(_.nonEmpty))
          .filter(_.nonEmpty)
    else list.map(_.filter(_.nonEmpty))
      .filter(_.nonEmpty)
  }
}
