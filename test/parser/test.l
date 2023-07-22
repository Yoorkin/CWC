fib : int -> int = 
    fun x -> if x <= 2 
             then x 
             else let r = fib (x - 1) + fib (x - 2) in r

