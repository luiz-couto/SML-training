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
datatype perimetro = PConst of real 
  | PQuadrado of perimetro 
  | PCirculo of perimetro 
  | PRetangulo of perimetro * perimetro 
  | PTriangulo of perimetro * perimetro * perimetro;

fun eval (PConst p) = p
  | eval (PQuadrado(p)) = (eval p) * 4.0
  | eval (PRetangulo(p1, p2)) = (2.0 * (eval p1)) + (2.0 * (eval p2))
  | eval (PCirculo(p)) = 2.0 * 3.14 * (eval p)
  | eval (PTriangulo(p1, p2, p3)) = (eval p1) + (eval p2) + (eval p3);

val p = PQuadrado(PConst 4.0);
eval p;


(* 4 *)
datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = IConst of int | Op1 of UnOp * Sexpr | Op2 of BinOp * Sexpr * Sexpr;

fun simplify (Op2(Add, ex1, ex2)) = 
    if simplify(ex1) = IConst 0 
    then simplify(ex2)
    else if simplify(ex2) = IConst 0
    then simplify(ex1)
    else Op2(Add, simplify(ex1), simplify(ex2))
  
  | simplify (Op2(Sub, ex1, ex2)) = 
    if simplify(ex2) = IConst 0 
    then simplify(ex1)
    else if simplify(ex1) = simplify(ex2)
    then IConst 0
    else Op2(Sub, simplify(ex1), simplify(ex2))
  
  | simplify (Op2(Mul, ex1, ex2)) = 
    if simplify(ex1) = IConst 1 
    then 
    simplify(ex2) 
    else if simplify(ex2) = IConst 1 
    then simplify(ex1)
    else if simplify(ex1) = IConst 0 orelse simplify(ex2) = IConst 0 
    then IConst 0
    else Op2(Mul, simplify(ex1), simplify(ex2))

  | simplify (Op2(Or, ex1, ex2)) = if simplify(ex1) = simplify(ex2) then simplify(ex1) else Op2(Or, simplify(ex1), simplify(ex2))
  | simplify (Op1(Not, Op1(Not, ex))) = simplify(ex)
  | simplify ex = ex;

val t = Op2 (Mul, Op2 (Add, IConst 1, IConst 0), Op2 (Add, Op2 (Or, IConst 10, IConst 12), IConst 0));
simplify t;

val r = Op1(Not, Op1(Not, Op1(Not, IConst 9)));
simplify r;

(* 6 *)
(* a *)

type Num = int ;
type Var = string ;

datatype Aexpr = N of Num | V of Var | Plus of Aexpr * Aexpr | Mult of Aexpr * Aexpr | Minus of Aexpr * Aexpr;
datatype Bexpr = True | False | Eq of Aexpr * Aexpr | Leq of Aexpr * Aexpr | Not of Bexpr | And of Bexpr * Bexpr;

datatype Stm = Assign of Var * Aexpr | Skip | Comp of Stm * Stm | If of Bexpr * Stm * Stm | While of Bexpr * Stm | Repeat of Bexpr * Stm;

fun evalN n : Num = n;

exception FreeVar;
fun lookup [] id = raise FreeVar
  | lookup (( k : string , v ) :: l ) id = if id = k then v else lookup l id;

fun evalA ( N n ) _ = evalN n
  | evalA ( V x ) s = lookup s x
  | evalA ( Plus ( e1 , e2 ) ) s = ( evalA e1 s ) + ( evalA e2 s )
  | evalA ( Mult ( e1 , e2 ) ) s = ( evalA e1 s ) * ( evalA e2 s )
  | evalA ( Minus ( e1 , e2 ) ) s = ( evalA e1 s ) - ( evalA e2 s );

fun evalB True _ = true
  | evalB False _ = false
  | evalB ( Eq ( a1 , a2 ) ) s = ( evalA a1 s ) = ( evalA a2 s )
  | evalB ( Leq ( a1 , a2 ) ) s = ( evalA a1 s ) <= ( evalA a2 s )
  | evalB ( Not b ) s = not ( evalB b s )
  | evalB ( And ( b1 , b2 ) ) s = ( evalB b1 s ) andalso ( evalB b2 s );

fun evalStm ( stm : Stm ) ( s : ( string * int ) list ) : ( string * int ) list =
  case stm of (Assign (x, a)) => (x , evalA a s ) :: s
  | Skip => s
  | (Comp (stm1, stm2)) => evalStm stm2 ( evalStm stm1 s )
  | (If (b , stm1 , stm2 ) ) => if ( evalB b s ) then evalStm stm1 s else evalStm stm2 s
  | (While (b, stm)) => if (evalB b s) then evalStm (While(b, stm)) (evalStm stm s) else s
  | (Repeat (b, stm)) => if (evalB b s) then s else evalStm (Repeat(b, stm)) (evalStm stm s)
  | _ => raise Match;

(* b *)
(*

[repeat tt]: <S, s> -> s' <repeat S until b, s'> -> s''
             ----------------------------------------- if B[[b]] = tt
                     <repeat S until b, s> -> s''

[repeat ff]:                <S, s> -> s' 
             ----------------------------------------- if B[[b]] = ff
                     <repeat S until b, s> -> s'

*)

(* d *)
(*

Sabemos pela definição dada na letra b que o stm S sempre é executado,
não importando se b avalia para falso ou true. A repetição de S, porém,
ocoore apenas se b é igual a falso. Desse modo, as proposiçoes da questão
são equivalentes, já que, quando b é true, a chamada de SKIP faz com que o 
estado seja o mesmo. De modo similar, quando b é false, repeat é chamado no-
vamente.

*)

