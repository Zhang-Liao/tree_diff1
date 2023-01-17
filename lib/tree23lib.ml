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
  | Tree (Leaf l) -> Atom l
  | Tree(Node2 (a , b)) -> List [Atom "Node2"; sexp_of_tree23c sexp_of_a a ; sexp_of_tree23c sexp_of_a b]
  | Tree(Node3 (a, b, c)) -> List [Atom "Node3"; sexp_of_tree23c sexp_of_a a ; sexp_of_tree23c sexp_of_a b; sexp_of_tree23c sexp_of_a c]

type 'a change23 = 'a tree23c * 'a tree23c [@@deriving sexp_of]
type patch23 = (metavar change23) tree23c [@@deriving sexp_of]

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

let map_tree_functor f0 f1 t =
  let rec aux t = 
  match f0 t with   
  | Leaf l -> f1 (Leaf l)
  | Node2 (a, b) -> f1 (Node2 (aux a, aux b))
  | Node3 (a, b, c) -> f1 (Node3 (aux a, aux b, aux c)) 
  in aux t

(* --------------------------------------------------------- *)
(* Type conversion  *)
let treeh_to_treec t = map_tree_functor (fun x -> x.data) (fun x -> Tree x) t 

let treeh_to_tree t = map_tree_functor  (fun x -> x.data) (fun x -> x) t

let tree_to_treec t = map_tree_functor  (fun x -> x) (fun x -> Tree x) t 
