(* 7 *)
signature MATHLIB =
sig
    exception NegativeNumber
    val fact: int -> int
    val halfPi: real
    val pow: int * int -> int
    val double: int -> int
end;

structure MyMathLib :> MATHLIB =
struct
    exception NegativeNumber
    fun fact n = if n = 0 then 1 else if n < 0 then raise NegativeNumber else n * fact(n-1)
    val halfPi = 3.14/2.0
    fun pow (n, e) = if e = 0 then 1 else if n < 0 then raise NegativeNumber else n * pow (n, e-1)
    fun double n = if n < 0 then raise NegativeNumber else 2*n
end;

fun useMyMathLib(n, "pow") = (print (Int.toString(MyMathLib.pow(n,n)) ^ " ")
    handle MyMathLib.NegativeNumber => print "Nao posso lidar com numeros negativos! ")
  | useMyMathLib(n,"fact") = (print (Int.toString(MyMathLib.fact(n)) ^ " ")
    handle MyMathLib.NegativeNumber => print "Nao posso lidar com numeros negativos! ")
  | useMyMathLib(n, "double") = (print (Int.toString(MyMathLib.double(n)) ^ " ")
    handle MyMathLib.NegativeNumber => print "Nao posso lidar com numeros negativos! ")
  | useMyMathLib _ =  print "";


(* useMyMathLib(2, "pow");
useMyMathLib(~3, "fact");
useMyMathLib(2, "double"); *)