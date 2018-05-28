package checkers.findingNextMove

/**
  * Class describing a move
  * @param start starting position
  * @param end ending position
  * @param jump is the move, a jump over? a takeover?
  * @param jumpOver the position of jump
  * @param valid is the move valid, is it possible
  */
case class Move (
             start : (Int, Int) = null , var end : (Int, Int) = null,
             jump : Boolean = false,
             jumpOver : (Int, Int) = (-1, -1),
             valid : Boolean = true
              ){

}

