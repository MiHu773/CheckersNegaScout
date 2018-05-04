package checkers
import checkers.Type._

import scala.collection.mutable.Queue
class Board {
  var board = Array.ofDim[Element](8,8);
  var whites, blacks, whitesQ, blacksQ=0;
  //Queue[String];

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
  def possibleMoves( position:(String, String)/*, moves:(String, String)*/) : Queue[(String, String)] = {
    var queue = Queue[(String, String)]()
//    val aux = new (String, String)()
    val el : Element = board(position._1.toInt) (position._2.toInt)
    var nextRow : Int = 0
    var previousRow : Int = 0
    var nextColumn0 = position._2.toInt - 1
    var nextColumn1 = position._2.toInt + 1

    if (el.elementType== `white`)
    {
      nextRow = (position._1.toInt - 1)
      previousRow = (position._1.toInt + 1)
    }
    else if (el.elementType== `black`)
    {
      nextRow = (position._1.toInt + 1)
      previousRow = (position._1.toInt - 1)
    }
      //case _ => queue
      nextRow = position._1.toInt-1
      nextColumn0 = position._2.toInt-1
      nextColumn1 = position._2.toInt+1


        if (nextRow>=0 || nextRow <= 7)
        {

          if (nextColumn0<=7 && nextColumn0>= 0 && checkIfFree((nextRow.toString, nextColumn0.toString))) //jeśli pola po skosie są wolne
          {
            val aux = (nextRow.toString, nextColumn0.toString)
            queue.enqueue(aux)
          }

          if (nextColumn1<=7 && nextColumn1>= 0 && checkIfFree((nextRow.toString, nextColumn1.toString)))
          {
            val aux = (nextRow.toString, nextColumn1.toString)
            queue.enqueue((nextRow.toString, nextColumn1.toString))
          }

          if (nextColumn0-1<=7 && nextColumn0-1>=0 && checkIfFree(((nextRow+1).toString, (nextColumn0-1).toString))) //jeśli pola po skosie są wolne
          {
            val aux = ((nextRow+1).toString, (nextColumn0-1).toString)
            queue.enqueue(aux)
          }

          if (nextColumn1+1<=7 && nextColumn1+1>=0 && checkIfFree(((nextRow-1).toString, (nextColumn1+1).toString))) //jeśli pola po skosie są wolne
          {
            val aux = ((nextRow-1).toString, (nextColumn1+1).toString)
            queue.enqueue(aux)
          }

          if (nextColumn0-1<=7 && nextColumn0-1>=0  && checkIfFree(((previousRow+1).toString, (nextColumn0-1).toString))) //jeśli pola po skosie są wolne
          {
            val aux = ((previousRow+1).toString, (nextColumn0-1).toString)
            queue.enqueue(aux)
          }

          if (nextColumn1+1<=7 && nextColumn1+1>=0 && checkIfFree(((previousRow+1).toString, (nextColumn1+1).toString))) //jeśli pola po skosie są wolne
          {
            val aux = ((previousRow+1).toString, (nextColumn1+1).toString)
            queue.enqueue(aux)
          }
        }


        return queue
    }
      /*case `whiteQueen` => "X"
      case `black` => "o"
      case `blackQueen` => "O"*/



  def checkIfFree(position:(String, String)) : Boolean = {
    val toCheck : Element= board(position._1.toInt) (position._2.toInt);
    if (toCheck.elementType == null)
      true;
    else false;

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
