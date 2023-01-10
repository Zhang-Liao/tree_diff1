open Sexplib.Sexp
open Sexplib.Conv

module MetaVarSet = Set.Make(String) 
type tree23 = 
    Leaf of string
  | Node2 of tree23 * tree23 
  | Node3 of tree23 * tree23 * tree23 [@@deriving sexp]

type metavar = string [@@deriving sexp]
type tree23h = 
    LeafH of string * metavar
  | Node2H of (tree23h * tree23h) * metavar
  | Node3H of (tree23h * tree23h * tree23h) * metavar


(* 'b contains additional information *)
type ('a, 'b) tree23c = 
    Hole of 'a * 'b 
  | LeafC of string 
  | Node2C of ('a, 'b) tree23c * ('a, 'b) tree23c *'b 
  | Node3C of ('a, 'b) tree23c * ('a, 'b) tree23c * ('a, 'b) tree23c * 'b [@@deriving sexp]
type 'a change23 = ('a, unit) tree23c * ('a, unit) tree23c [@@deriving sexp]
type patch23 = ((metavar change23), unit) tree23c [@@deriving sexp]

(* soure_vars * destination_vars * is_closed *)
type change_vars = MetaVarSet.t * MetaVarSet.t * bool
type patch23v =  ((metavar change23), change_vars) tree23c 

(* --------------------------------------------------------- *)
(* Type conversion *)
let rec tree_to_treec = function 
| Leaf l -> LeafC l
| Node2 (a, b) -> Node2C (tree_to_treec a, tree_to_treec b, ())
| Node3 (a, b, c) -> Node3C (tree_to_treec a, tree_to_treec b, tree_to_treec c, ())

(* --------------------------------------------------------- *)
(* S-expression *)
let load_tree23s f =
  (* let open Core.Sexp in *)
  let sexps = load_sexps f in 
  List.map (function 
      | List [List [Atom "S"; s]; List [Atom "D"; d]] -> 
        tree23_of_sexp s, tree23_of_sexp d 
      | _ -> failwith "The input in the dataset has a wrong format. Each line must contain two trees"
    ) sexps

