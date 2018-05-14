package src

import checkers.{Board, Element, Type}
import checkers.Type.Type

import scala.annotation.tailrec

class MoveSet(element: Element, board: Board, lastMove: Move = null /*, nextRow : Int, previousRow : Int, nextColumnL : Int,*/) {
  val root: Array[MoveSet] = findNextMoveSet()
  val settings = generateSettings()

  def findNextMove(): Array[Move] = {

    //  ================================================================================================================================================================================================
    //  przesuwanie po skosie na wolne pole
    //  ================================================================================================================================================================================================
    val MNLFunc = if (settings.pozMNL._2 <= 7 && settings.pozMNL._2 >= 0 && settings.checkedMNL == "-") //jeśli pole po skosie jest wolne
      new Move((element.posX, element.posY), (settings.pozMNL._1, settings.pozMNL._2))
    else new Move(null, null, false, null, false)

    val MNRFunc = if (settings.pozMNR._2 <= 7 && settings.pozMNR._2 >= 0 && settings.checkedMNR == "-") //jeśli pole po skosie jest wolne
      new Move((element.posX, element.posY), (settings.pozMNR._1, settings.pozMNR._2))
    else new Move(null, null, false, null, false)

    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku przeciwnika
    //    ================================================================================================================================================================================================

    val JmpNLFunc = if (settings.pozJmpNL._2 <= 7 && settings.pozJmpNL._2 >= 0 && (settings.checkedMNL == settings.other || settings.checkedMNL == settings.otherQ) && settings.checkedJmpNL == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpNL._1, settings.pozJmpNL._2), true, (settings.pozMNL._1, settings.pozMNL._2))
    else new Move(null, null, false, null, false)

    val JmpNRFunc = if (settings.pozJmpNR._2 <= 7 && settings.pozJmpNR._2 >= 0 && (settings.checkedMNR == settings.other || settings.checkedMNR == settings.otherQ) && settings.checkedJmpNR == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpNR._1, settings.pozJmpNR._2), true, (settings.pozMNR._1, settings.pozMNR._2))
    else new Move(null, null, false, null, false)

    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku swoim
    //    ================================================================================================================================================================================================

    val JmpPLFunc = if (settings.pozJmpPL._2 <= 7 && settings.pozJmpPL._2 >= 0 && (settings.checkedMPL == settings.other || settings.checkedMPL == settings.otherQ) && settings.checkedJmpPL == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpNR._1, settings.pozJmpNR._2), true, (settings.pozMNR._1, settings.pozMNR._2))
    else new Move(null, null, false, null, false)

    val JmpPRFunc = if (settings.pozJmpPR._2 <= 7 && settings.pozJmpPR._2 >= 0 && (settings.checkedMPR == settings.other || settings.checkedMPR == settings.otherQ) && settings.checkedJmpPR == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpPR._1, settings.pozJmpPR._2), true, (settings.pozMPR._1, settings.pozMPR._2))
    else new Move(null, null, false, null, false)

    Array(MNLFunc, MNRFunc, JmpNLFunc, JmpNRFunc, JmpPLFunc, JmpPRFunc)
  }

  @tailrec
  def findNextMoveSet(): Array[MoveSet] = {

    for (mv <- findNextMove())
      {
        findNextMoveSet()
      }


  }

  def commitMove(mv : Move, board: Board): Board =
  {
    board.makeMove()
  }

  def check(position: (Int, Int) /*, other:String*/): String = {

    val toCheck: Element = board(position._1.toInt)(position._2.toInt);
    if (position._1 < 0 || position._2 > 7 || position._2 > 7 || position._2 < 0) {
      return "?"
    }
    else if (toCheck.elementType == null) {
      return "-"
    }
    else if (toCheck.elementType == Type.white) {
      return "x"
    }
    else if (toCheck.elementType == Type.black) {
      return "o"
    }
    else if (toCheck.elementType == Type.whiteQueen) {
      return "X"
    }
    else return "O"
  }

  def generateSettings(): Settings = { // tworzy ustawienia wykorzystywane do obliczania następnego ruchu
    element.elementType match {
      case Type.white =>
        new Settings(
          this,
          element,
          element.posX - 1,
          element.posY + 1,
          -1,
          Type.black,
          Type.blackQueen)
      //TODO God save the queen
      /*case Type.whiteQueen =>
        new Settings(element.posX - 1,
          element.posY + 1,
          element.posY + 1,
          element.posY - 1,
          -1,
          Type.black,
          Type.blackQueen)*/

      case Type.black =>
        new Settings(
          this,
          element,
          element.posX + 1,
          element.posY - 1,
          1,
          Type.white,
          Type.whiteQueen)

      /*case Type.blackQueen =>
        new Settings(element.posX - 1,
          element.posY + 1,
          element.posY + 1,
          element.posY - 1,
          -1,
          Type.black,
          Type.blackQueen)*/
      //      case null => "-"
    }
  }
}
