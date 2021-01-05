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
