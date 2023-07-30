data Tree a 
    = Node of (Tree a, Tree a) 
    | Leaf of a

data Pair k v
    = Pair of (k,v)

let max = fun (x,y) -> if x > y then x else y in
let withKey1 = fun value -> Pair (1,value) in
withKey1 (Leaf 5)