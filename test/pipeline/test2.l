data Tree a 
    = Node of (Tree a, Tree a) 
    | Leaf of a

let max = fun (x,y) -> if x > y then x else y
in Leaf (max (1,2))