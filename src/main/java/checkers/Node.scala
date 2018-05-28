package checkers

import Type._;

/**
  *
  * @param boardMove tuple containing board and move by which board was created
  * @param parent parent board
  * @param level level in the tree
  * @param colour type of checker that has made the move
  */
case class Node(boardMove: (Board, String), parent: Node, level: Int, colour: Type.Type) {
  val maxLevel = 4;
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
