package checkers.findingNextMove

import java.util

import checkers.Type.Type
import checkers.parameterCaseClasses.Settings
import checkers.{Board, Element, Type}

import scala.annotation.tailrec

/**
  * Class which is used for finding possible moves for game pieces. When initialized it automatically
  * creates a tree structure of other MoveSets.
  * @param element element for which the moves will be searched
  * @param board board on which ss the element
  * @param lastMove used internally for the MoveSet class, leave null
  */
class MoveSet(element: Element, board: Board, lastMove: Move = null /*, nextRow : Int, previousRow : Int, nextColumnL : Int,*/)
{
  val settings: Settings = generateSettings()
  val root: List[MoveSet] = findNextMoveSet()

  /**
    * Function displaying possible moves for an element
    */
  def printPossibleMoves(): Unit = {

    for ((pm, i) <- possibleMoves() zip (0 until possibleMoves().length)) {
      print("[" + i + "] :")
      for (mv <- pm.reverse if mv != null) {
        print(mv.end + " ")
      }
      println()
    }
  }


  /**
    * Method removes empty elements from list
    * @param list list to be filtered
    * @return list of lists of moves with removed empty elements
    */
  def removeEmpty(list: List[List[Move]]) : List[List[Move]] = list.filter(_.nonEmpty)


  /**
    * Due to the way the MoveSet class works, it some times adds null elements which have to be remowed
    * @param list list of list of moves
    * @return list of lists of moves without null elements
    */
  def removeNull(list: List[List[Move]]): List[List[Move]] = {
    if (list == null) return null
    for (pm <- list if pm.nonEmpty)
      yield pm.filter(_ != null)
  }

  /**
    * lists all possible moves for an element
    * @return complete list of lists of moves, for the element
    */
  def possibleMoves(): List[List[Move]] = {
    if (element.elementType == Type.whiteQueen || element.elementType == Type.blackQueen)
      removeEmpty(findQueenNextMove())
    else
      removeEmpty(findManNextMove())
  }

  /**
    * finds moves for a simple man element
    * @return a list of lists of moves and jumps, for a single MoveSet class
    */
  def findManNextMove() : List[List[Move]] = {
    val x = removeNull(jmpsToList())
    val y = movesToList()
    if (x.head.nonEmpty) {
      x ++ y
    }
    else y
  }

  /**
    * Due to the way the MoveSet class works, it creates a tree which has to be brought down to a simple list of lists
    * @return list of lists of moves
    */
  def movesToList(): List[List[Move]] = {
    val auxArr = for (m <- findNextMove() if (m.valid)) yield m
    if (auxArr == null) return null
    auxArr.toList.map(List(_))
  }

  /**
    * Due to the way the MoveSet class works, it creates a tree which has to be brought down to a simple list of lists
    * @return list of lists of moves that are jumps
    */
  def jmpsToList(): List[List[Move]] = {

    if (root.length == 0) return List(List(lastMove))

    val x = for (ms <- root) yield {
      for (ml <- ms.jmpsToList()) yield {
        ml :+ lastMove
      }
    }
    x.flatten
  }

  /**
    * Finds moves for the queen, in the the "free move" phase, before taking out the first enemy piece.
    * It checks the moves in 4 directions.
    * @return list of lists of moves for Queen
    */
  def findQueenNextMove(): List[List[Move]] = {
   val aux = (for (x <- -1 to 1 if x != 0; y <- - 1 to 1 if y != 0)
      yield moveQueenCheck((element.posX+x, element.posY+y), (x, y), board)) // (collection.breakOut)

    aux.filter(_ != null).toList.flatten
  }

  /**
    * Checks if the current position can be taken by Queen
    * @param position position to check
    * @param direction direction in which the Queen is moving
    * @param board board on which the Queen is moving
    * @return List of lists of moves, in which is the found move
    */
  private def moveQueenCheck(position: (Int, Int), direction: (Int, Int), board: Board): List[List[Move]] = {
  val nextPos = ((position._1 + direction._1), (position._2 + direction._2))
  if (check(position) == settings.other || check(position) == settings.otherQ) {

      if (check(nextPos) == Type.empty)
        {
          val auxBoard: Board = new Board()
          auxBoard.setUpBoardCopy(board)
          val move = new Move((element.posX, element.posY), nextPos, true, position, true)
          commitMove(move, auxBoard)

          auxBoard.board(move.end._1)(move.end._2) = new Element(settings.self, move.end._1, move.end._2)

          val moveSet : MoveSet = new MoveSet(auxBoard.board(move.end._1)(move.end._2), auxBoard, move)
          val aux = moveSet.jmpsToList()

          return moveSet.jmpsToList()
        }
      else return null
    }
    if (check(position) == Type.error || check(position) == settings.self || check(position) == settings.selfQ ) return null
    else{
      val aux = moveQueenCheck(nextPos, direction, board)
      if (aux != null) return List(List(new Move((element.posX, element.posY), position))) ++ aux
      else return List(List(new Move((element.posX, element.posY), position)))
    }

  }

  /**
    * Checks for jumps. In four directions.
    * @return Array of jumps, if they are possible.
    */
  def findNextJump(): Array[Move] = {
    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku przeciwnika
    //    ================================================================================================================================================================================================

    val JmpNLFunc = if (settings.pozJmpNL._2 <= 7 && settings.pozJmpNL._2 >= 0
      && (settings.checkedMNL == settings.other || settings.checkedMNL == settings.otherQ)
      && settings.checkedJmpNL == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      Move((element.posX, element.posY), (settings.pozJmpNL._1, settings.pozJmpNL._2), true, (settings.pozMNL._1, settings.pozMNL._2))
    else Move(null, null, false, null, false)

    val JmpNRFunc = if (settings.pozJmpNR._2 <= 7 && settings.pozJmpNR._2 >= 0
      && (settings.checkedMNR == settings.other || settings.checkedMNR == settings.otherQ)
      && settings.checkedJmpNR == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      Move((element.posX, element.posY), (settings.pozJmpNR._1, settings.pozJmpNR._2), true, (settings.pozMNR._1, settings.pozMNR._2))
    else Move(null, null, false, null, false)

    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku swoim
    //    ================================================================================================================================================================================================

    val JmpPLFunc = if (settings.pozJmpPL._2 <= 7 && settings.pozJmpPL._2 >= 0
      && (settings.checkedMPL == settings.other || settings.checkedMPL == settings.otherQ)
      && settings.checkedJmpPL == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      Move((element.posX, element.posY), (settings.pozJmpPL._1, settings.pozJmpPL._2), true, (settings.pozMPL._1, settings.pozMPL._2))
    else Move(null, null, false, null, false)

    val JmpPRFunc = if (settings.pozJmpPR._2 <= 7 && settings.pozJmpPR._2 >= 0
      && (settings.checkedMPR == settings.other || settings.checkedMPR == settings.otherQ)
      && settings.checkedJmpPR == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      Move((element.posX, element.posY), (settings.pozJmpPR._1, settings.pozJmpPR._2), true, (settings.pozMPR._1, settings.pozMPR._2))
    else Move(null, null, false, null, false)

    Array(JmpNLFunc, JmpNRFunc, JmpPLFunc, JmpPRFunc)
  }

  /**
    * Checks for moves. In two directions.
    * @return Array of moves, if they are possible.
    */
  def findNextMove(): Array[Move] = {

    //  ================================================================================================================================================================================================
    //  przesuwanie po skosie na wolne pole
    //  ================================================================================================================================================================================================
    val MNLFunc = if (settings.pozMNL._2 <= 7 && settings.pozMNL._2 >= 0 && settings.checkedMNL == Type.empty) //jeśli pole po skosie jest wolne
      Move((element.posX, element.posY), (settings.pozMNL._1, settings.pozMNL._2))
    else Move(null, null, false, null, false)

    val MNRFunc = if (settings.pozMNR._2 <= 7 && settings.pozMNR._2 >= 0 && settings.checkedMNR == Type.empty) //jeśli pole po skosie jest wolne
      Move((element.posX, element.posY), (settings.pozMNR._1, settings.pozMNR._2))
    else Move(null, null, false, null, false)

    Array(MNLFunc, MNRFunc)
  }


  /**
    * Creates a new MoveSet, in the new position determined by move.
    * @return List of MoveSets
    */

  def findNextMoveSet(): List[MoveSet] = {
    if (settings.element.elementType == Type.blackQueen || settings.element.elementType == Type.whiteQueen)
      return null

    var auxSet = Set[MoveSet]()

    for (mv <- findNextJump() if mv.valid) {
      val auxBoard: Board = new Board()
      auxBoard.setUpBoardCopy(board)
      val committedMove: (Element, Board) = commitMove(mv, auxBoard)

      val ms: MoveSet = new MoveSet(committedMove._1, committedMove._2, mv)
      auxSet += ms
    }
    List(auxSet.toList).flatten
  }

  /**
    * Function creating a simulation of the board after move.
    *
    * @param mv move to be done
    * @param board board on which the move will be carried out
    * @return tuple(Element, Board), where element is the element after move, board is the state of the board after move
    */
  def commitMove(mv: Move, board: Board): (Element, Board) = {
    board.makeMove(translateMoveToString(mv))
    if (mv.jump) {
      board.removeTile(mv.jumpOver._1, mv.jumpOver._2)
    }
    (board.getElement(mv.end._1, mv.end._2), board)

  }

  /**
    * Simple translation function for translating the move class into string
    *
    * @param mv move to be committed
    * @return the string code for the move
    */
  def translateMoveToString(mv: Move): (String, String) = {
    (mv.start._1.toString + mv.start._2.toString, mv.end._1.toString + mv.end._2.toString)
  }

  def translateTupleToString(mv: Move): (String, String) = {
    (mv.start._1.toString + mv.start._2.toString, mv.end._1.toString + mv.end._2.toString)
  }

  /**
    * Function checking what type of element is on the specified position.
    *
    * @param position position to be checked
    * @return type of the element on position (white, whiteQueen, black, blackQueen, error - if the position is not on the board, empty - if the given position is empty.
    */
  def check(position: (Int, Int), testBoard: Board = board /*, other:String*/): Type = {
    if (position._1 < 0 || position._2 > 7 || position._1 > 7 || position._2 < 0) {
      return Type.error
    }
    val toCheck: Element = testBoard.getElement(position._1.toInt, position._2.toInt)

    if (toCheck.elementType == null) {
      Type.empty
    }

    else if (toCheck.elementType == Type.white) {
      Type.white
    }

    else if (toCheck.elementType == Type.black) {
      Type.black
    }

    else if (toCheck.elementType == Type.whiteQueen) {
      Type.whiteQueen
    }

    else Type.blackQueen
  }

  /**
    * Generate settings for the element, for which the moves are searched.
    * @return Generated settings.
    */
  def generateSettings(): Settings = { // tworzy ustawienia wykorzystywane do obliczania następnego ruchu

    element.elementType match {
      case Type.white =>
        Settings(
          this,
          element,
          Type.black,
          Type.blackQueen,
          -1)

      case Type.whiteQueen =>
        Settings(this,
          element,
          Type.black,
          Type.blackQueen,
          0,
          Type.white,
          Type.whiteQueen)

      case Type.black =>
        Settings(
          this,
          element,
          Type.white,
          Type.whiteQueen,
          1)

      case Type.blackQueen =>
        Settings(this,
          element,
          Type.white,
          Type.whiteQueen,
          0,
          Type.black,
          Type.blackQueen
        )

    }
  }
}
