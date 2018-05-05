package checkers
import checkers.Type._

import scala.collection.mutable.Queue
import src.Move
class Board {
  var board = Array.ofDim[Element](8,8);
  var whites, blacks, whitesQ, blacksQ=0;
  //Queue[String];

  def setUpBoard1() : Unit = {
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
  }
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
    board(4)(2).setType(black)
  }

  def checkIfMoveCorrect(moves:(String, String)): Boolean = {
        return true // TODO checking if move is correct
  }

  def makeMove(moves:(String, String)) : Unit ={
    if (!checkIfMoveCorrect(moves)){
      return; //TODO error checking (throwing exception?)
    }
    var i1 = moves._1.charAt(0).asDigit;
    var j1 = moves._1.charAt(1).asDigit;
    var i2 = moves._2.charAt(0).asDigit;
    var j2 = moves._2.charAt(1).asDigit;
    var movedType = board(i1)(j1).elementType;
    board(i1)(j1).setType(null);
    board(i2)(j2).setType(movedType);
    if (math.abs(i1-i2)>1){ //jump
      if ((i2 > i1) && (j2>j1)){
        board(i1+1)(j1+1).setType(null)
      } else if ((i2 > i1) && (j2<j1)){
        board(i1+1)(j1-1).setType(null)
      } else if ((i2 < i1) && (j2>j1)){
        board(i1-1)(j1+1).setType(null)
      } else {
        board(i1-1)(j1-1).setType(null)
      }
    }

  }
  def possibleMoves( position:(String, String)/*, moves:Queue[Queue [(String, String)]]*/) : Queue[(String, String)] = {
    var queue = Queue[Move]()
//    val aux = new (String, String)()
    val el : Element = board(position._1.toInt) (position._2.toInt)
    var nextRow : Int = 0
    var previousRow : Int = 0
    var nextColumnL = position._2.toInt - 1
    var nextColumnR = position._2.toInt + 1

    var other : String =""
    var otherQ : String =""

    var pozJmpNL:(Int, Int) = (0, 0)
    var pozJmpNR:(Int, Int) = (0, 0)
    var pozJmpPL:(Int, Int) = (0, 0)
    var pozJmpPR:(Int, Int) = (0, 0)

    var pozMNL:(Int, Int) = (0, 0)
    var pozMNR:(Int, Int) = (0, 0)
    var pozMPL:(Int, Int) = (0, 0)
    var pozMPR:(Int, Int) = (0, 0)

    var direction:Int = 0

    if (el.elementType== `white`)
    {
      nextRow = (position._1.toInt - 1)
      previousRow = (position._1.toInt + 1)
      other = "o"
      otherQ = "O"
      direction = -1
    }
    else if (el.elementType== `black`)
    {
      nextRow = (position._1.toInt + 1)
      previousRow = (position._1.toInt - 1)
      other = "x"
      otherQ = "X"
      direction = 1
    }
      //case _ => queue
//    nextRow = position._1.toInt-1
    nextColumnL = position._2.toInt-1
    nextColumnR = position._2.toInt+1

    pozMNL = (nextRow, nextColumnL)
    pozMNR = (nextRow, nextColumnR)
    pozMPL = (previousRow, nextColumnL)
    pozMPR = (previousRow, nextColumnR)
    
    pozJmpNL = (nextRow+direction, nextColumnL-1)
    pozJmpNR = (nextRow+direction, nextColumnR+1)
    pozJmpPL = (previousRow-direction, nextColumnL-1)
    pozJmpPR = (previousRow-direction, nextColumnR+1)

    val checkedMNL =  check(pozMNL)
    val checkedMNR =  check(pozMNR)
    val checkedMPL =  check(pozMPL)
    val checkedMPR =  check(pozMPR)

    val checkedJmpNL =  check(pozJmpNL)
    val checkedJmpNR =  check(pozJmpNL)
    val checkedJmpPL =  check(pozJmpPL)
    val checkedJmpPR =  check(pozJmpPR)

//  ================================================================================================================================================================================================
//  przesuwanie po skosie na wolne pole
//  ================================================================================================================================================================================================
    if (pozMNL._2<=7 && pozMNL._2>= 0 && checkedMNL == "-" ) //jeśli pole po skosie jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozMNL._1, pozMNL._2))
      queue.enqueue(aux)
    }

    if (pozMNR._2<=7 && pozMNR._2>= 0 && checkedMNR == "-" ) //jeśli pole po skosie jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozMNR._1, pozMNR._2))
      queue.enqueue(aux)
    }
//    ================================================================================================================================================================================================
//    bicie po skosie w kierunku przeciwnika
//    ================================================================================================================================================================================================

    if (pozJmpNL._2<=7 && pozJmpNL._2>= 0 && (checkedMNL == other ||  checkedMNL == otherQ) &&  checkedJmpNL == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozJmpNL._1, pozJmpNL._2), true, (pozMNL._1, pozMNL._2))
      queue.enqueue(aux)
    }

    if (pozJmpNR._2<=7 && pozJmpNR._2>= 0 && (checkedMNR == other ||  checkedMNR == otherQ) &&  checkedJmpNR == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozJmpNR._1, pozJmpNR._2), true, (pozMNR._1, pozMNR._2))
      queue.enqueue(aux)
    }
    //    ================================================================================================================================================================================================
    //    bicie po skosie w kierunku swoim
    //    ================================================================================================================================================================================================
    
    if (pozJmpPL._2<=7 && pozJmpPL._2>= 0 && (checkedMPL == other ||  checkedMPL == otherQ) &&  checkedJmpPL == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozJmpPL._1, pozJmpPL._2), true, (pozMPL._1, pozMPL._2))
      queue.enqueue(aux)
    }

    if (pozJmpPR._2<=7 && pozJmpPR._2>= 0 && (checkedMPR == other ||  checkedMPR == otherQ) &&  checkedJmpPR == "-") //jeśli pole po skosie jest zajęte przez przeciwny kolor i pole za nim jest wolne
    {
      val aux = new Move((position._1.toInt, position._2.toInt), (pozJmpPR._1, pozJmpPR._2), true, (pozMPR._1, pozMPR._2))
      queue.enqueue(aux)
    }



  /*  for (groupOfMoves <- queue)
      {
        aux =
        moves.enqueue(groupOfMoves)
        possibleMoves(groupOfMoves, moves)

      }*/
    return queue
    }
      /*case `whiteQueen` => "X"
      case `black` => "o"
      case `blackQueen` => "O"*/



  def check(position:(Int, Int)/*, other:String*/) : String = {
    val toCheck : Element= board(position._1.toInt) (position._2.toInt);
    if (position._1<0 || position._2>7 || position._2>7 || position._2<0)
      {
        return "?"
      }
    else if (toCheck.elementType == null)
      {
        return "-"
      }
    else if(toCheck.elementType == white )
    {
      return "x"
    }
    else if(toCheck.elementType == black )
    {
      return "o"
    }
    else if(toCheck.elementType == whitesQ )
    {
      return "X"
    }
    else return "O"
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
