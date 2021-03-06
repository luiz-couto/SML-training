(* 1 *)
signature MATHLIB =
sig
    val fact: int -> int
    val halfPi: real
    val pow: int * int -> int
    val double: int -> int
end;

structure MyMathLib :> MATHLIB =
struct
    fun fact n = if n = 0 then 1 else n * fact(n-1)
    val halfPi = 3.14/2.0
    fun pow (n, e) = if e = 0 then 1 else n * pow (n, e-1)
    fun double n = 2*n
end;

(* MyMathLib.pow(2,3);
MyMathLib.double(6);
MyMathLib.fact(10);
MyMathLib.halfPi; *)