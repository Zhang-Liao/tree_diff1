open Sexplib.Sexp
open Sexplib.Conv

module MetaVarSet = Set.Make(String)
type 'a tree23_functor =
    Leaf of string
  | Node2 of 'a * 'a
  | Node3 of 'a * 'a * 'a [@@deriving sexp]

type tree23 = tree23 tree23_functor [@@deriving sexp]
type metavar = string [@@deriving sexp]
type tree23h = {data: tree23h tree23_functor; dig :string}

type 'a tree23c =
    Tree of 'a tree23c tree23_functor
  | Hole of 'a

let rec sexp_of_tree23c sexp_of_a t =
  match t with
  | Hole h -> List[Atom "Hole"; sexp_of_a h]
  | Tree (Leaf l) -> List[Atom "Leaf"; Atom l]
  | Tree(Node2 (a , b)) -> List [Atom "Node2"; sexp_of_tree23c sexp_of_a a ; sexp_of_tree23c sexp_of_a b]
  | Tree(Node3 (a, b, c)) -> List [Atom "Node3"; sexp_of_tree23c sexp_of_a a ; sexp_of_tree23c sexp_of_a b; sexp_of_tree23c sexp_of_a c]

type 'a change23 = 'a tree23c * 'a tree23c [@@deriving sexp_of]
type patch23 = (metavar change23) tree23c [@@deriving sexp_of]

(* --------------------------------------------------------- *)
(* S-expression *)
let load_tree23s f =
  let sexps = load_sexps f in
  Stdlib.fst@@List.fold_left (fun (acc, curr_exp) sexp ->
      if curr_exp == None
      then (acc, Some (tree23_of_sexp sexp))
      else (Option.get curr_exp, tree23_of_sexp sexp)::acc, None
    ) ([], None) sexps

(* --------------------------------------------------------- *)
(* Map and Fold *)
let get_holes t =
  let rec aux acc = function
    | Hole h -> h::acc
    | Tree (Leaf _) -> acc
    | Tree (Node2 (a, b)) ->
      let acc1 = aux acc a in
      aux acc1 b
    | Tree (Node3 (a, b, c)) ->
      let acc1 = aux acc a in
      let acc2 = aux acc1 b in
      aux acc2 c
  in aux [] t

let map_holes f t =
  let rec aux t =
    match t with
    | Hole h -> f h
    | Tree (Leaf l) -> Tree (Leaf l)
    | Tree (Node2 (a, b)) -> Tree (Node2 (aux a, aux b))
    | Tree (Node3 (a, b, c)) -> Tree (Node3 (aux a, aux b, aux c))
  in aux t

let map_tree_functor f t =
  match t with
  | Leaf l -> Leaf l
  | Node2 (a, b) -> Node2 (f a, f b)
  | Node3 (a, b, c) -> Node3 (f a, f b, f c)

let rec map_tree23c f t =
  match t with
  | Hole h -> f h
  | Tree tre -> Tree (map_tree_functor (map_tree23c f) tre)

(* --------------------------------------------------------- *)
(* Type conversion  *)
let rec treeh_to_tree t = map_tree_functor (fun x -> treeh_to_tree x) t.data

let rec treeh_to_treec t = Tree (map_tree_functor (fun x ->  treeh_to_treec x) t.data)

let rec tree_to_treec t = Tree (map_tree_functor (fun x ->  tree_to_treec x) t)
