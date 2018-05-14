package checkers

import checkers.Type._

case class Element(elementType: Type) {

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
