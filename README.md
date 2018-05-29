# Checkers with NegaScout algorithm


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