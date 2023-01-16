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
(* Type conversion  *)
let rec tree_to_treec t: metavar tree23c = 
  match t with 
  | Leaf l -> Tree (LeafF l)
  | Node2 (a, b) ->  Tree(Node2F (tree_to_treec a, tree_to_treec b))
  | Node3 (a, b, c) -> Tree(Node3F (tree_to_treec a, tree_to_treec b, tree_to_treec c))

let rec treeh_to_treec t: metavar tree23c = 
  match t.data with 
  | LeafF l -> Tree (LeafF l)
  | Node2F (a, b) ->  Tree(Node2F (treeh_to_treec a, treeh_to_treec b))
  | Node3F (a, b, c) -> Tree(Node3F (treeh_to_treec a, treeh_to_treec b, treeh_to_treec c))

let rec treeh_to_tree t = 
  match t.data with 
  | LeafF l -> Leaf l
  | Node2F (a, b) -> Node2 (treeh_to_tree a, treeh_to_tree b)
  | Node3F (a, b, c) -> Node3 (treeh_to_tree a, treeh_to_tree b, treeh_to_tree c)

(* --------------------------------------------------------- *)
(* S-expression *)
let load_tree23s f =
  let sexps = load_sexps f in 
  List.map (function 
      | List [List [Atom "S"; s]; List [Atom "D"; d]] -> 
        tree23_of_sexp s, tree23_of_sexp d 
      (* s1, d1 *)
      | _ -> failwith "The input in the dataset has a wrong format. Each line must contain two trees"
    ) sexps

(* --------------------------------------------------------- *)
(* Map and Fold *)
let get_holes t = 
  let rec aux acc = function
    | Hole h -> h::acc
    | Tree (LeafF _) -> acc
    | Tree (Node2F (a, b)) -> 
      let acc1 = aux acc a in
      aux acc1 b 
    | Tree (Node3F (a, b, c)) -> 
      let acc1 = aux acc a in
      let acc2 = aux acc1 b in
      aux acc2 c 
  in aux [] t

let map_holes f t = 
  let rec aux t = 
    match t with
    | Hole h -> f h 
    | Tree (LeafF l) -> Tree (LeafF l)
    | Tree (Node2F (a, b)) -> Tree (Node2F (aux a, aux b))
    | Tree (Node3F (a, b, c)) -> Tree (Node3F (aux a, aux b, aux c))
  in aux t

let map_fold_holes f t acc = 
  let rec aux t acc = 
    match t with
    | Hole h -> f acc h 
    | Tree (LeafF l) -> Tree (LeafF l), acc
    | Tree (Node2F (a, b)) -> 
      let a', acc1 = aux a acc in
      let b', acc2 = aux b acc1 in
      Tree (Node2F (a', b')), acc2
    | Tree (Node3F (a, b, c)) -> 
      let a', acc1 = aux a acc in
      let b', acc2 = aux b acc1 in
      let c', acc3 = aux c acc2 in
      Tree (Node3F (a', b', c')), acc3    
  in aux t acc

