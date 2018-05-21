package checkers

case class Move (
             start : (Int, Int) = null , var end : (Int, Int) = null,
             jump : Boolean = false,
             jumpOver : (Int, Int) = (-1, -1),
             valid : Boolean = true
              ){
  /* var sX : Int = sX
   var sY : Int = sY*/
}

