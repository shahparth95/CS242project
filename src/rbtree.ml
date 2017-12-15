open Core
open Result.Monad_infix

exception Unimplemented

type z = Z : z
type 'n s = S: 'n -> 'n s

module Node = struct
  type black = Black : black
  type red = Red : red

  type (_, _, _) node =
    | Null : (black, z, 'a) node  
    | BlackN : (_, 'n, 'a) node * 'a * (_, 'n, 'a) node -> (black, 'n s, 'a) node
    | RedN: (black, 'n, 'a) node * 'a * (black, 'n, 'a) node -> (red, 'n, 'a) node

  let rec black_height: type c l a.(c, l, a) node -> int =
  fun nd ->
  match nd with 
  | Null -> 0
  | RedN(l, _, _) -> black_height(l)
  | BlackN(l, _, _) -> 1 + black_height(l)

  let rec find: type c l a.(c, l, a) node -> a -> bool =
    fun nd x ->
    match nd with
    | Null -> false
    | RedN(l, v, r) -> if v = x then true else (if v > x then find l x else find r x)
    | BlackN(l, v, r) -> if v = x then true else (if v > x then find l x else find r x) ;;
end

    
type 'a type_node = 
  T : ('c, 'n, 'a) Node.node -> 'a type_node;;

type ('n, 'a) ltype_node = 
  |T : ('c, 'n, 'a) Node.node -> ('n, 'a) ltype_node
  |H : (Node.black, 'n s, 'a) Node.node ->  ('n, 'a) ltype_node;;


let rec find_node: type c l a.(c, l, a)Node.node -> a -> (a type_node, string) Result.t =
  fun nd x ->
  match nd with
  | Node.Null -> Error "Value not found"
  | Node.RedN(l, v, r) -> if v = x then Ok(T(Node.RedN(l, v, r))) else (if v > x then find_node l x else find_node r x)
  | Node.BlackN(l, v, r) -> if v = x then Ok(T(Node.BlackN(l, v, r))) else (if v > x then find_node l x else find_node r x) ;;

let rec insert_val: type c l a.(c, l, a)Node.node -> a -> (l, a) ltype_node =
  fun nd x ->
  match nd with
  | Node.Null -> T(Node.RedN(Node.Null, x, Node.Null))  (*inserting the red node always*)
  | Node.RedN(l, v, r) ->
    if v > x then (
      let child_insert = insert_val l x in
      match child_insert with
      | T(Node.BlackN(lc, vc, rc)) -> T(Node.RedN(Node.BlackN(lc, vc, rc), v, r))
      | T(Node.RedN(lc, vc, rc)) -> H(Node.BlackN(Node.RedN(lc, vc, rc), v, r)) 
    ) else (
      let child_insert = insert_val r x in 
      match child_insert with
      | T(Node.BlackN(lc, vc, rc)) -> T(Node.RedN(l, v, Node.BlackN(lc, vc, rc)))
      | T(Node.RedN(lc, vc, rc)) -> H(Node.BlackN(l, v, Node.RedN(lc, vc, rc)))
    )
  | Node.BlackN(l, v, r) -> 
    if v > x then (
      let child_insert = insert_val l x in
      match child_insert with
      | T(lc) -> T(Node.BlackN(lc, v, r))
      | H(lc) -> 
      ( match r with
        | Node.RedN(rlc, rv, rrc) ->
          T(Node.RedN(lc, v, Node.BlackN(rlc, rv, rrc)))
        | Node.BlackN(rlc, rv, rrc) -> (
          match lc with
          | Node.BlackN(Node.RedN(lllc, llv, llrc), lv, Node.BlackN(lrlc, lrv, lrrc)) -> (* Node.BlackN(lrlc, lrv, lrrc) = lrc *)
            T(Node.BlackN(
                Node.RedN(lllc, llv, llrc), 
                lv, 
                Node.RedN(Node.BlackN(lrlc, lrv, lrrc), v, Node.BlackN(rlc, rv, rrc))
              )
            ) 
          | Node.BlackN(Node.BlackN(lllc, llv, llrc), lv, Node.RedN(lrlc, lrv, lrrc)) ->  (* Node.BlackN(lllc, llv, llrc) = llc*)
            T(Node.BlackN(
                Node.RedN(Node.BlackN(lllc, llv, llrc), lv, lrlc), 
                lrv, 
                Node.RedN(lrrc, v, Node.BlackN(rlc, rv, rrc))
              )
            )
        )
        | Node.Null -> (
          match lc with
          | Node.BlackN(Node.RedN(Node.Null, llv, Node.Null), lv, Node.Null) -> (* Node.Null = lrc *)
            T(Node.BlackN(
                Node.RedN(Node.Null, llv, Node.Null), 
                lv, 
                Node.RedN(Node.Null, v, Node.Null)
              )
            ) 
          | Node.BlackN(Node.Null, lv, Node.RedN(Node.Null, lrv, Node.Null)) ->  (* Node.Null = llc*)
            T(Node.BlackN(
                Node.RedN(Node.Null, lv, Node.Null), 
                lrv, 
                Node.RedN(Node.Null, v, Node.Null)
              )
            )
        )
      )
    ) else (
      let child_insert = insert_val r x in 
      match child_insert with
      | T(rc) -> T(Node.BlackN(l, v, rc))
      | H(rc) ->   
      ( match l with
        | Node.RedN(llc, lv, lrc) ->
          T(Node.RedN(Node.BlackN(llc, lv, lrc), v, rc))
        | Node.BlackN(llc, lv, lrc) -> ( (*Node.BlackN(llc, lv, lrc) = l*)
          match rc with
          | Node.BlackN(Node.RedN(rllc, rlv, rlrc), rv, Node.BlackN(rrlc, rrv, rrrc)) -> (* Node.BlackN(rrlc, rrv, rrrc) = rrc*)
            T(Node.BlackN(
                Node.RedN(Node.BlackN(llc, lv, lrc), v, rllc),
                rlv,
                Node.RedN(rlrc, rv, Node.BlackN(rrlc, rrv, rrrc))
              )
            ) 
          | Node.BlackN(Node.BlackN(rllc, rlv, rlrc), rv, Node.RedN(rrlc, rrv, rrrc)) -> (*Node.BlackN(rllc, rlv, rlrc) = rlc*)
            T(Node.BlackN( 
                Node.RedN(Node.BlackN(llc, lv, lrc), v, Node.BlackN(rllc, rlv, rlrc)),
                rv,
                Node.RedN(rrlc, rrv, rrrc)
              )
            )
        )
        | Node.Null -> ( (*Node.BlackN(llc, lv, lrc) = l*)
          match rc with
          | Node.BlackN(Node.RedN(Node.Null, rlv, Node.Null), rv, Node.Null) -> (* Node.Null = rrc*)
            T(Node.BlackN(
                Node.RedN(Node.Null, v, Node.Null),
                rlv,
                Node.RedN(Node.Null, rv, Node.Null)
              )
            ) 
          | Node.BlackN(Node.Null, rv, Node.RedN(Node.Null, rrv, Node.Null)) -> (*Node.Null = rlc*)
            T(Node.BlackN( 
                Node.RedN(Node.Null, v, Node.Null),
                rv,
                Node.RedN(Node.Null, rrv, Node.Null)
              )
            )
        ) 
      )
    )

module RBTree = struct
  type _ root =
    | Root : (Node.black, 'l, 'a) Node.node-> 'a root

  let rec black_height_tree: type a.a root -> int =
  fun rn ->
  match rn with
  | Root(Node.Null) -> 0
  | Root(Node.BlackN(l, _, _)) -> 1 + Node.black_height(l)

  let insert: type a. a root -> a -> a root =
  fun rn x ->
  match rn with
  | Root(Node.Null) -> Root(Node.BlackN(Node.Null, x, Node.Null))
  | Root(r) ->
  let insert_tree = (insert_val r x) in
  match insert_tree with 
  |T(Node.BlackN(li, vi, ri)) -> Root(Node.BlackN(li, vi, ri)) 
  |T(Node.RedN(li, vi, ri)) -> Root(Node.BlackN(li, vi, ri))

  let insert_list: type a. a root -> a list -> a root =
  fun rn nlist ->
    List.fold nlist ~init:rn ~f:(
      fun new_root i ->
      insert new_root i
  )

  let find: type a. a root -> a -> bool =
  fun rn x ->
  match rn with
  | Root(Node.Null) -> false
  | Root(r) -> Node.find r x 
end;;

let rbtree = RBTree.Root(Node.Null) in
let insert1 = RBTree.insert rbtree 1 in 
let insert2 = RBTree.insert insert1 2 in 
let insert3 = RBTree.insert insert2 3 in 
RBTree.insert insert3 4;;

let rec ilist i n = let x = i+1 in if i <= n then i::(ilist x n) else [];;

let insertion_list = ilist 1 100000;;

print_string (string_of_float(Unix.gettimeofday()));;
print_string "\n";;

let rbtree = RBTree.Root(Node.Null) in
let final_result = RBTree.insert_list rbtree insertion_list in
for i = 1 to 100000 do
  assert(RBTree.find final_result i)
done;;
print_string "\n";;
print_string (string_of_float(Unix.gettimeofday()));;
print_string "\n";;

let _ = List.fold insertion_list ~init:String.Set.empty ~f:(
  fun c_set i ->
  String.Set.add c_set (string_of_int(i)) 
) in
print_string (string_of_float(Unix.gettimeofday()));;
print_string "\n";;

for i = 1 to 10000 do
  ignore(ilist 0 i)
done;;
print_string (string_of_float(Unix.gettimeofday()));;
print_string "\n";;





