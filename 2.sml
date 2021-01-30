(* 1 *)
datatype expr = IConst of int
  | Plus of expr * expr 
  | Minus of expr * expr 
  | Multi of expr * expr
  | Div of expr * expr
  | Max of expr * expr
  | Min of expr * expr
  | Eq of expr * expr
  | Gt of expr * expr;

fun eval (IConst i) = i
  | eval (Plus(e1, e2)) = (eval e1) + (eval e2)
  | eval (Minus(e1, e2)) = (eval e1) - (eval e2)
  | eval (Multi(e1, e2)) = (eval e1) * (eval e2)
  | eval (Div(e1, e2)) = if (eval e2) = 0 then 0 else (eval e1) div (eval e2)
  | eval (Max(e1, e2)) = if (eval e1) > (eval e2) then (eval e1) else (eval e2)
  | eval (Min(e1, e2)) = if (eval e1) < (eval e2) then (eval e1) else (eval e2)
  | eval (Eq(e1, e2)) = if (eval e1) = (eval e2) then 1 else 0
  | eval (Gt(e1, e2)) = if (eval e1) > (eval e2) then 1 else 0;


val e1 = (Max(IConst 3, Plus(IConst 2, IConst 3)));
eval e1;

val e2 = Div(Multi(IConst 5, IConst 4), Minus(IConst 4, IConst 4));
eval e2;


(* 2 *)
datatype area = RConst of real | AQuadrado of area | ACirculo of area | ARetangulo of area * area;

fun eval (RConst r) = r
  | eval (AQuadrado(a)) = (eval a) * (eval a) 
  | eval (ARetangulo(a1, a2)) = (eval a1) * (eval a2)
  | eval (ACirculo(a)) = 3.14 * ((eval a) * (eval a));

val e = ACirculo(RConst 2.0);
eval e;


(* 3 *)

