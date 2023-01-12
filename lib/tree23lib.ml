open Sexplib.Sexp
open Sexplib.Conv

module MetaVarSet = Set.Make(String) 
type tree23 =     
    Leaf of string
  | Node2 of tree23 * tree23
  | Node3 of tree23 * tree23 * tree23 [@@deriving sexp]
type 'a tree23_functor = 
    LeafF of string
  | Node2F of 'a * 'a
  | Node3F of 'a * 'a * 'a [@@deriving sexp]
type metavar = string [@@deriving sexp]
type tree23h = {data: tree23h tree23_functor; dig :string} 

type 'a tree23c = 
    Tree of 'a tree23c tree23_functor
  | Hole of 'a  [@@deriving sexp]

type 'a change23 = 'a tree23c * 'a tree23c [@@deriving sexp]
type patch23 = (metavar change23) tree23c [@@deriving sexp]

(* --------------------------------------------------------- *)
(* Type conversion ??? *)
let rec tree_to_treec t: metavar tree23c = 
  match t with 
  | Leaf l -> Tree (LeafF l)
  | Node2 (a, b) ->  Tree(Node2F (tree_to_treec a, tree_to_treec b))
  | Node3 (a, b, c) -> Tree(Node3F (tree_to_treec a, tree_to_treec b, tree_to_treec c))

(* --------------------------------------------------------- *)
(* S-expression *)
let load_tree23s f =
  (* let open Core.Sexp in *)
  let sexps = load_sexps f in 
  List.map (function 
      | List [List [Atom "S"; s]; List [Atom "D"; d]] -> 
        tree23_of_sexp s, tree23_of_sexp d 
        (* s1, d1 *)
      | _ -> failwith "The input in the dataset has a wrong format. Each line must contain two trees"
    ) sexps
