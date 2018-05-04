package checkers

import checkers.Type._

class Element() {

    var elementType : Type = null;

    def setType(mType: Type) : Unit = {
      elementType = mType;
    }

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
