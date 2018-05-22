package checkers

import Type._;

class Node (board: Board, parent: Node, level:Int, colour: Type.Type){
  val maxLevel = 5;
  val next = {
    if (colour == white) black;
    else white;
  }
  val children: Seq[Node] ={
    if (level > maxLevel) Seq.empty;
    else board.getAllPossibleMoves(colour)
      .view
      .map(b => {
        val mBoard = board;
        mBoard.makeMoveSequence(b);
        mBoard
      })
      .sortBy(-_.heuristic()) //descending
      .map(b => new Node(b, this, level + 1, next))
      .force;
  }


}
