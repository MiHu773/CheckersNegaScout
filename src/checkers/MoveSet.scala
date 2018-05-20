package checkers

import checkers.Type.Type

class MoveSet(element: Element, val board: Board, lastMove: Move = null /*, nextRow : Int, previousRow : Int, nextColumnL : Int,*/) {

  val settings: Settings = generateSettings()
  val root: List[MoveSet] = findNextMoveSet()

 def printPossibleMoves() : Unit ={

    for ((pm, i)<- possibleMoves() zip (0 until possibleMoves().length))
      {

        print("[" + i + "] :")
        for (mv <- pm.reverse if mv != null)
          {
            print(mv.end + " ")
          }
        println()
      }
  }
  def removeNull(list: List[List[Move]]) : List[List[Move]] ={
    for ( pm <- list )
    yield {
      pm.filter(_ != null)
    }
  }

  def possibleMoves(): List[List[Move]] = {
//    println("jmpsToList: " + jmpsToList())
    val x = removeNull(jmpsToList())
    val y = movesToList()
    if (x.head.nonEmpty)
       y ++ x
    else y
  }

  def movesToList(): List[List[Move]] = {
    //val aux: List[List[Move]] = jmpsToList()
    var auxSet = Set[Move]()
//    val auxFind: Array[Move] =
    for (m <- findNextMove()) {
//      println("m: " + m)
      if (m.valid)
        auxSet += m
    }
//    for (mv <- auxSet) yield {
//      List(mv)
//    }
    List(auxSet.toList)
  }

  def jmpsToList(string: String = "   "): List[List[Move]] = {
    //    if (lastMove != null) return null
    //    var set = Set[()
    if (root.length == 0) return List(List(lastMove))

    val x = for (ms <- root) yield {
      //      val aux : List[List[Move]] = ms.movesToList(pastList);
//      println("ms: " + ms)
      for (ml <- ms.jmpsToList(string + "   ") ) yield {
//        println(string + "ml: " + ml)

            ml :+ lastMove
      }
    }
//      println("x: " + x)

      x.flatten
  }

  def findNextJump(): Array[Move] = {

    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku przeciwnika
    //    ================================================================================================================================================================================================

    val JmpNLFunc = if (settings.pozJmpNL._2 <= 7 && settings.pozJmpNL._2 >= 0
      && (settings.checkedMNL == settings.other || settings.checkedMNL == settings.otherQ)
      && settings.checkedJmpNL == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpNL._1, settings.pozJmpNL._2), true, (settings.pozMNL._1, settings.pozMNL._2))
    else new Move(null, null, false, null, false)

    val JmpNRFunc = if (settings.pozJmpNR._2 <= 7 && settings.pozJmpNR._2 >= 0
      && (settings.checkedMNR == settings.other || settings.checkedMNR == settings.otherQ)
      && settings.checkedJmpNR == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpNR._1, settings.pozJmpNR._2), true, (settings.pozMNR._1, settings.pozMNR._2))
    else new Move(null, null, false, null, false)

    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku swoim
    //    ================================================================================================================================================================================================

    val JmpPLFunc = if (settings.pozJmpPL._2 <= 7 && settings.pozJmpPL._2 >= 0
      && (settings.checkedMPL == settings.other || settings.checkedMPL == settings.otherQ)
      && settings.checkedJmpPL == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpPL._1, settings.pozJmpPL._2), true, (settings.pozMPL._1, settings.pozMPL._2))
    else new Move(null, null, false, null, false)

    val JmpPRFunc = if (settings.pozJmpPR._2 <= 7 && settings.pozJmpPR._2 >= 0
      && (settings.checkedMPR == settings.other || settings.checkedMPR == settings.otherQ)
      && settings.checkedJmpPR == Type.empty) //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
      new Move((element.posX, element.posY), (settings.pozJmpPR._1, settings.pozJmpPR._2), true, (settings.pozMPR._1, settings.pozMPR._2))
    else new Move(null, null, false, null, false)

    Array(JmpNLFunc, JmpNRFunc, JmpPLFunc, JmpPRFunc)
  }

  def findNextMove(): Array[Move] = {

    //  ================================================================================================================================================================================================
    //  przesuwanie po skosie na wolne pole
    //  ================================================================================================================================================================================================
    val MNLFunc = if (settings.pozMNL._2 <= 7 && settings.pozMNL._2 >= 0 && settings.checkedMNL == Type.empty) //jeśli pole po skosie jest wolne
      new Move((element.posX, element.posY), (settings.pozMNL._1, settings.pozMNL._2))
    else new Move(null, null, false, null, false)

    val MNRFunc = if (settings.pozMNR._2 <= 7 && settings.pozMNR._2 >= 0 && settings.checkedMNR == Type.empty) //jeśli pole po skosie jest wolne
      new Move((element.posX, element.posY), (settings.pozMNR._1, settings.pozMNR._2))
    else new Move(null, null, false, null, false)

    Array(MNLFunc, MNRFunc)
  }

  def findAllMoveSets(): List[MoveSet] = {
    print(findNextMoveSet())
    findNextMoveSet()
  }

  def findNextMoveSet(): List[MoveSet] = {
    //    val numbers = 0 to 3
    //val auxList: List[MoveSet] = List()
    var auxSet = Set[MoveSet]()

    for (mv <- findNextJump() if mv.valid) {
        val auxBoard : Board= board.copy()
        auxBoard.setUpBoardCopy(board)
//        println("checking for" + mv.start)
//        println("checking for board: ")

        val committedMove: (Element, Board) = commitMove(mv, auxBoard)
//        println("after committing")

        val ms: MoveSet = new MoveSet(committedMove._1, committedMove._2, mv)
        //        ms.findNextMoveSet()
        auxSet += ms
        //        set += ms


      //        println("auxList: " + auxList + ", ms: " + ms + ", set: " + set)
    }
    List(auxSet.toList).flatten
  }

  /* def findNextMoveSet(): Array[MoveSet] = {
     val numbers = 0 to 3
     val auxArray = Array.ofDim[MoveSet](4)
     for ((mv, i) <- findNextJump() zip numbers ) {
       println(mv, i)
       if (mv.valid) {
         val committedMove: (Element, Board) = commitMove(mv, board)
         committedMove._2.printBoard()
         val ms: MoveSet = new MoveSet(committedMove._1, committedMove._2, mv)
         ms.findNextMoveSet()
         auxArray(i) = ms
       }
     }
     auxArray
   }*/

  /**
    * Funkcja wykonuje symulacje wyglądu planszy po wykonaniu ruchu
    *
    * @param mv    - ruch do wykonania
    * @param board - plansza na której ma być wykonane przesunięcie
    * @return - tuple(Element, Board), gdzie element to figura po przesunięciu, a boardz planaz po wykonaniu przesunięcia
    */
  def commitMove(mv: Move, board: Board): (Element, Board) = {
    board.makeMove(translateMoveToString(mv))
    if (mv.jump) {
      board.removeTile(mv.jumpOver._1, mv.jumpOver._2)
    }
    (board.getElement(mv.end._1, mv.end._2), board)

  }

  /**
    * funkcja tłumaczy współrzędne zawarte w parametrze klasy Move na String
    *
    * @param mv - ruch do wykonania
    * @return -
    */
  def translateMoveToString(mv: Move): (String, String) = {
    (mv.start._1.toString + mv.start._2.toString, mv.end._1.toString + mv.end._2.toString)
  }

  /**
    * funkcja sprawdzająca jaki jest element na danej pozycji
    *
    * @param position - pozycja do sprawdzenia
    * @return - typ elementu zwrócony jako Type
    */
  def check(position: (Int, Int) /*, other:String*/): Type = {
    if (position._1 < 0 || position._2 > 7 || position._1 > 7 || position._2 < 0) {
      return Type.error
    }
    val toCheck: Element = board.getElement(position._1.toInt, position._2.toInt);

    if (toCheck.elementType == null) {
      return Type.empty
    }

    else if (toCheck.elementType == Type.white) {
      return Type.white
    }

    else if (toCheck.elementType == Type.black) {
      return Type.black
    }

    else if (toCheck.elementType == Type.whiteQueen) {
      return Type.whiteQueen
    }

    else return Type.blackQueen
  }

  def generateSettings(): Settings = { // tworzy ustawienia wykorzystywane do obliczania następnego ruchu
    element.elementType match {
      case Type.white =>
        new Settings(
          this,
          element,
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
