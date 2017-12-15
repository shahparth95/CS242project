module Term = struct
  type binop = ADD | SUB | MUL | DIV
  type direction = LEFT | RIGHT
  type ('a, 'b) sum = Left of 'a | Right of 'b
  type ('a, 'b) tuple = Pair of 'a * 'b

  type _ expr =
    | GBool : bool -> bool expr
    | GIf : bool expr * 'a expr * 'a expr -> 'a expr
    | GEq : 'a expr * 'a expr -> bool expr
    | GLt : int expr * int expr -> bool expr
    | GInt : int -> int expr
    | GBinop : binop * int expr * int expr -> int expr
    | GApp: ('a -> 'b expr) * 'a expr -> 'b expr
    | GTuple: 'a expr * 'b expr -> ('a, 'b) tuple expr
    | GProjectLeft: ('a, 'b) tuple expr -> 'a expr
    | GProjectRight: ('a, 'b) tuple expr -> 'b expr
    | GInjectLeft: 'a expr -> ('a, 'b) sum expr
    | GInjectRight: 'b expr -> ('a, 'b) sum expr
    | GCase: ('a, 'b) sum expr * ('a -> 'c expr) * ('b -> 'c expr) -> 'c expr
end

let rec eval : type a. a Term.expr -> a = fun x ->
  match x with
  | Term.GBool (b) -> b
  | Term.GInt (i) -> i
  | Term.GIf (b, l, r) -> if eval b then eval l else eval r
  | Term.GEq (a, b) -> (eval a) = (eval b)
  | Term.GLt (a,b) -> eval a < eval b
  | Term.GBinop(binop, l, r) ->
    (match binop with
      | Term.ADD -> eval l + eval r
      | Term.SUB -> eval l - eval r
      | Term.MUL -> eval l * eval r
      | Term.DIV -> eval l / eval r
    )
  | Term.GApp(a, b) -> eval(a(eval(b)))
  | Term.GProjectLeft(t) -> 
  (match t with 
    | Term.GTuple(l, _) -> eval(l))
  | Term.GProjectRight(t) ->
  (match t with
    | Term.GTuple(_, r) -> eval(r))
  | Term.GCase(a, lf, rf) ->
  (match a with
    | Term.GInjectLeft(l) -> eval(Term.GApp(lf, l))
    | Term.GInjectRight(r) -> eval(Term.GApp(rf, r))
  )
;;

(* eval (Term.GBinop(Term.DIV, Term.GInt(5), Term.GInt(2)));; *)

(* Term.GApp(Term.GLam("x", Type.GBool true, Term.GVar "x"), Term.GBool(true));; *)

(* Term.GBool(true);;

fun (x: int) -> x;;
assert(eval (Term.GApp((fun (x: int) -> Term.GBinop(Term.ADD, Term.GInt(x), Term.GInt(2))), Term.GInt(1))) = 3);;
assert(eval (Term.GProjectLeft(Term.GTuple(Term.GBinop(Term.ADD, Term.GInt(1), Term.GInt(2)), Term.GInt(0)))) = 3);;

let lf = (fun (x: int) -> Term.GBinop(Term.ADD, Term.GInt(x), Term.GInt(2))) in
let rf = (fun (x: bool) -> if x then Term.GInt(1) else Term.GInt(0)) in
let sumt = Term.GInjectLeft(Term.GInt(1)) in
let case = Term.GCase(sumt, lf, rf) in
assert(eval(case) = 3);;

let lf = (fun (x: int) -> Term.GBinop(Term.ADD, Term.GInt(x), Term.GInt(2))) in
let rf = (fun (x: bool) -> if x then Term.GInt(1) else Term.GInt(0)) in
let sumt = Term.GInjectRight(Term.GBool(true)) in
let case = Term.GCase(sumt, lf, rf) in
assert(eval(case) = 1)

let a = Term.Left 1 *)

let rec ilist i n = let x = i+1 in if i <= n then i::(ilist x n) else [];;

let start_t = Unix.gettimeofday();;
print_string (string_of_float(Unix.gettimeofday()));;
print_string "\n";;
 
for i = 1 to 100000000 do
  assert(eval(Term.GBinop(Term.ADD, Term.GInt(i), Term.GInt(i))) = 2*i);
done;;

(* let end_t = Unix.gettimeofday() in  *)
(* print_float int_of_float(start_t - end_t);; *)

print_string (string_of_float(Unix.gettimeofday()))

