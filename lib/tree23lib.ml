open Sexplib.Sexp
module IntSet = Set.Make(Int)
type tree23 = 
    Leaf of string
  | Node2 of tree23 * tree23 
  | Node3 of tree23 * tree23 * tree23 

type tree23h = 
    LeafH of string * int
  | Node2H of (tree23h * tree23h) * int
  | Node3H of (tree23h * tree23h * tree23h) * int

type metavar = int

type 'a tree23c = 
    Hole of 'a 
  | LeafC of string
  | Node2C of 'a tree23c * 'a tree23c 
  | Node3C of 'a tree23c * 'a tree23c * 'a tree23c
type 'a change23 = 'a tree23c * 'a tree23c
type patch23 = (metavar change23) tree23c

(* soure_vars * destination_vars * is_closed *)
type change_vars = IntSet.t * IntSet.t * bool
type patch23v = 
    HoleV of metavar change23 * change_vars
  | LeafV of string
  | Node2V of (patch23v * patch23v) * change_vars
  | Node3V of (patch23v * patch23v * patch23v) * change_vars

(* --------------------------------------------------------- *)
(* Type conversion *)
let rec tree_to_treec = function 
| Leaf l -> LeafC l
| Node2 (a, b) -> Node2C (tree_to_treec a, tree_to_treec b)
| Node3 (a, b, c) -> Node3C (tree_to_treec a, tree_to_treec b, tree_to_treec c)

(* --------------------------------------------------------- *)
(* S-expression *)
let rec sexpr_to_tree23 s = 
  match s with
  | Atom a -> Leaf a
  | List [Atom "Node2"; a; b] -> Node2 (sexpr_to_tree23 a, sexpr_to_tree23 b)
  | List [Atom "Node3"; a; b; c] -> Node3 (sexpr_to_tree23 a, sexpr_to_tree23 b, sexpr_to_tree23 c)
  | x -> failwith (to_string x ^" does not obey the type constraint of tree23")

let rec tree23_sexpr t = 
  match t with
  | Leaf l -> Atom l
  | Node2 (a, b) -> List [Atom "Node2"; tree23_sexpr a; tree23_sexpr b]
  | Node3 (a, b, c) -> List [Atom "Node3"; tree23_sexpr a; tree23_sexpr b; tree23_sexpr c]


let load_tree23s f =
  let sexps = load_sexps f in 
  List.map (function 
      | List [List [Atom "S"; s]; List [Atom "D"; d]] -> 
        sexpr_to_tree23 s, sexpr_to_tree23 d 
      | _ -> failwith "The input in the dataset has a wrong format. Each line must contain two trees"
    ) sexps

let tree23c_sexpr hl_sexpr = 
  let rec aux =
    function
    | Hole h -> hl_sexpr h 
    | LeafC l -> Atom l
    | Node2C (a, b) -> List [Atom "Node2"; aux a; aux b]
    | Node3C (a, b, c) -> List [Atom "Node3"; aux a; aux b; aux c] 
  in aux 

let change23_sexpr (c1, c2) = 
  let hl_sexpr h = List [Atom "Hole"; Atom (Stdlib.string_of_int h)] in
  List[tree23c_sexpr hl_sexpr c1; Atom "==>" ; tree23c_sexpr hl_sexpr c2]

let patch23_sexpr p = tree23c_sexpr change23_sexpr p

