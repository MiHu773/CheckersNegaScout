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


# Opis działania programu:
Na początku program wyświetla plansze i zestaw możliwych ruchów. Gdzie:

````
„x” – białe piony
„o” – czarne piony
„X” – biała damka
„O” – czarna damka
„-” – puste pole
````

Po wyświetleniu planszy wyświetlona zostaje lista ruchów w formie List(44 33, 46 37, 51 42, 53 42, 71 60, 73 64, 75 64, 75 66, 77 66) – gdzie pierwsza współrzędna reprezentuje położenie pionka, którym możemy wykonać ruch, a następne pola, na które można wykonać ruchy. Aby wykonać ruch należy wprowadzić jeden z ciągów cyfr. W przypadku błędu, program powiadomi o niepoprawnym wprowadzeniu danych. Program wyświetla tylko możliwe ruchy, w zgodzie z zasadą przymusowego bicia. Po wykonaniu ruchu, przeciwnik wykonuje ruch.

# Zasady Gry:
Celem jest zbicie wszystkich pionów przeciwnika, lub doprowadzenie go do sytuacji kiedy nie może wykonać ruchu. Bicia są obowiązkowe i wymagają przeskoczenia pionka przeciwnika. Bicia można łączyć w sekwencje. Pionek staje się damką gdy skończy turę na ostatnim polu (pierwszym polu przeciwnika). Damka może wykonywać ruch o dowolną odległość, w dowolnym kierunku po skosie, po wykonaniu pierwszego bicia zachowuje się jak pionek(może łączyć bicia), aż do zakończenia tury.
