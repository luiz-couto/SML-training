(* 1 *)
fun cube (x:real) = x*x*x
cube(3.0)

(* 2 *)
fun pow (n, 0) = 1 
  | pow (n, e) = n * pow(n, e-1)  
pow(5,2)

(* 3 *)
fun sumLists ([], []) = []
  | sumLists (h1::t1, h2::t2) = (h1+h2)::sumLists(t1, t2)
sumLists([2, 5, 10], [1, 15, 4])

(* 4 *)
fun max ([]) = 0 (* retorna 0 caso a lista dada seja uma lista vazia *)
  | max (x::[]) = x
  | max (x::y::[]) = if x >= y then x else y
  | max (x::y::t) = if x >= y then max(x::t) else max(y::t)
max([2, 1, 7, 3])

(* 5 *)
fun cumSum (x::y::xs) = x :: cumSum(x+y::xs)
  | cumSum xs = xs
cumSum([6, 10, 3, 11])

(* 6 *)
fun greet "" = "Hello nobody"
  | greet str = "Hello " ^ str
greet("Janis")