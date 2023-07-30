
let id = fun x -> x 
in let getRight = fun (a,b) -> b 
in getRight (id false,id 1)
