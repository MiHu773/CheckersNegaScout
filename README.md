# Checkers with NegaScout algorithm
## Description of the program:

At the beginning, the program displays the board and a set of possible movements. Where:

`` ``
"X" - white men
"O" - black men
"X" - white king
"O" - black king
"-" - empty space
`` ``

After displaying the board, the program displays a list of movements in the form of a List for example: (44 33, 46 37, 51 42, 53 42, 71 60, 73 64, 75 64, 75 66, 77 66) where the first coordinates represent the checker on the starting field. The coordinates are: first number row rank, second number: column rank. Next coordinates are the next move locations. For example, traffic associated with one of a string of numbersTo move, you have to select and input one of the possible listed moves. In the event of an error, the program will notify you of an incorrect entry of data. The program displays only the results of moves, according to the principle of forced beating. After moving, the opponent's makes his move.

## Game rules:
The goal is to capture all opponent's checkers, or bring him to a position in which he can't  make any move. Checkers can only move diagonally. Beatings are mandatory, and to take out an opponents checker it is needed to jump over it to an empty field behind it. When a player jumps over their opponent's (the other player's) piece, you take that piece from the board. The beatings are combined into sets of moves. A man becomes a king when it finishes it's move in the back rank. It is possible that a clear and steady breakup with others. Kings can move to squares diagonally at any distance. However after starting to takeout opponent's pieces they behave as a normal man, until they end their turn.


## How NegaScout works:
 
 NegaScout algorithm assumes that first explored move is the best one. That is why we are sorting each child of node by it's heuristic value. If we make the assumption we can search other nodes with null window, because it is faster than normal alpha-beta. However, if this search fails we have to decide wether to search it again with the proper window or not. After that searches we have to make the search over the "best" move. 

````

ns(node, depth, a, b, color){
    if (node is terminal  || depth == 0) return color * node's heuristic value;
    countScoreForEach(node.children, 0,depth, a,b,color) //execute ns for each child
}

```` 

To avoid using for loop countScoreForEach function needs to be implemented as tail recursion. 

```` 

  final def countScoreForEach(nodes, index, depth, a, b, color){
    val score = {
      if (index == 0) -ns(nodes(0), depth - 1, -b, -a, -color) //search best child
      else {
        scoreTemp = -ns(nodes(index), depth - 1, -a - 1, -a, -color)  //search with null window
        if (a < scoreTemp and scoreTemp < b) -ns(nodes(index), depth - 1, -b, -scoreTemp, -color) //if search with null window failed make full search
        else scoreTemp
      }
    }
    val anext = math.max(a,score);
    if (anext>b or index==nodes.length-1) return anext;  //alpha-beta pruning
    countScoreForEach(nodes, index+1, depth, anext, b, color) //run recursion
  }
````


