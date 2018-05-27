package checkers

import checkers.Type._

/**
  * Class representing checker
  * @param elementType type of element
  * @param posX row
  * @param posY column
  */
case class Element(elementType: Type, posX: Int, posY: Int) {

  /**
    * Printing element on console
    * @return element in "visual" notation
    */
  def printElement() : String = {
      elementType match {
        case `white` => "x"
        case `whiteQueen` => "X"
        case `black` => "o"
        case `blackQueen` => "O"
        case null => "-"
      }
  }
}
