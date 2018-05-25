package checkers

import Type._;

case class Node(boardMove: (Board, String), parent: Node, level: Int, colour: Type.Type) {
  val maxLevel = 5;
  val next = {
    if (colour == white) black;
    else white;
  }
  val children: Seq[Node] = {
    if (level > maxLevel) Seq.empty;
    else boardMove._1.getAllPossibleMoves(colour)
      .view
      .map(move => {
        val mBoard: Board = new Board();
        mBoard.setUpBoardCopy(boardMove._1);
        mBoard.makeMoveSequence(move);
        (mBoard, move)
      })
      .sortBy(-_._1.heuristic()) //descending
      .map(b => new Node(b, this, level + 1, next))
      .force;
  }


}
