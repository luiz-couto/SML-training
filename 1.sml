(* 1 *)
fun cube (x:real) = x*x*x;
cube(3.0);

(* 2 *)
fun pow (n, 0) = 1 
  | pow (n, e) = n * pow(n, e-1);
pow(5,2);


(* 3 *)
fun sumLists ([], []) = []
  | sumLists (h1::t1, h2::t2) = (h1+h2)::sumLists(t1, t2);
sumLists([2, 5, 10], [1, 15, 4]);


(* 4 *)
fun max ([]) = 0 (* retorna 0 caso a lista dada seja uma lista vazia *)
  | max (x::[]) = x
  | max (x::y::[]) = if x >= y then x else y
  | max (x::y::t) = if x >= y then max(x::t) else max(y::t);
max([2, 1, 7, 3]);


(* 5 *)
fun cumSum (x::y::xs) = x :: cumSum(x+y::xs)
  | cumSum xs = xs;
cumSum([6, 10, 3, 11]);


(* 6 *)
fun greet "" = "Hello nobody"
  | greet str = "Hello " ^ str;
greet("Janis");


(* 7 *)
fun isSeparator c = List.exists (fn x => x = c) (explode(" ,.-"));

fun splitAux ([]) aux s = s::aux
  | splitAux (h::t) aux s = if isSeparator(h) then splitAux t (s::aux) "" else splitAux t aux (s^str(h));

fun split s = rev (splitAux (explode s) [] "");
split("Bom dia,pra-voce");


(* 8 *)
fun allTrue (x::[]) = x
  | allTrue (x::xs) = if x = false then false else allTrue xs;
allTrue([true, true, false, true]);
allTrue([true, true, true]);


(* 9 *)
datatype dinheiro = Centavos of int | Real of real | Pessoa_Dinheiro of (string * real);

fun amount (Centavos din) = din
  | amount (Real din) = round (din * 100.0)
  | amount (Pessoa_Dinheiro din) = round ((#2 din) * 100.0);

amount(Real(2.0));
amount(Centavos(2));
amount(Pessoa_Dinheiro("Gene", 2.5));


(* 10 *)
datatype Planeta = Mercurio | Venus | Terra | Marte | Jupiter | Saturno | Urano | Netuno;
fun calc age orbit = (round (orbit / 12.0)) * age;

fun planetAge age Mercurio = calc age 88.0
  | planetAge age Venus = calc age 225.0
  | planetAge age Terra = calc age 365.0
  | planetAge age Marte = calc age 687.0
  | planetAge age Jupiter = calc age 4332.0
  | planetAge age Saturno = calc age 10760.0
  | planetAge age Urano = calc age 30681.0
  | planetAge age Netuno = calc age 60190.0;

planetAge 24 Jupiter;


(* 11 *)
