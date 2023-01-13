open Sexplib
open Tree_diff_lib.Tree23lib

let context = ref 1
let file = ref "data/tree23.dat"
let args =
  [
    ("-context", Arg.Set_int context, "Set to 1 to contain the context, 2 to remove context. Default to contain the context.");
    ("-file", Arg.Set_string file, "The file of input data.")
  ]
let usage = "Tree23."
let () = Arg.parse
    args (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

module StrMap = Map.Make(String)
module IntMap = Map.Make(Int)
let rec extract o t = 
  match o t with
  | Some i -> Hole i
  | None -> (
      match t.data with
      | LeafF a -> Tree (LeafF a)
      | Node2F (a, b) -> Tree(Node2F (extract o a, extract o b))   
      | Node3F (a, b, c) -> Tree(Node3F (extract o a, extract o b, extract o c)))

let postproc t1 t2 (tc1, tc2) = 
  let vars_of t = 
    List.fold_left (fun acc v -> MetaVarSet.add v acc)  MetaVarSet.empty (get_holes t) in
  let keep_or_drop tc t vars map =
    let rec aux tc t map = 
      match (tc, t) with
      | Hole h, _ -> (
          match StrMap.find_opt h vars with
          | None -> tree_to_treec t, map
          | Some reorder_h -> Hole (Stdlib.string_of_int reorder_h), IntMap.add reorder_h t map )
      | Tree(LeafF _), _ -> tc, map
      | Tree(Node2F (a0, b0)), Node2(a1, b1)->
        let a2, map0 = aux a0 a1 map in
        let b2, map1 = aux b0 b1 map0 in
        Tree(Node2F (a2, b2)), map1
      | Tree(Node3F (a0, b0, c0)), Node3(a1, b1, c1) -> 
        let a2, map0 = aux a0 a1 map in
        let b2, map1 = aux b0 b1 map0 in
        let c2, map2 = aux c0 c1 map1 in
        Tree(Node3F (a2, b2, c2)), map2
      | _ -> failwith "tree23c's node is inconsistent with tree23's node" 
    in aux tc t map
  in
  let vars1 = vars_of tc1 in
  let vars2 = vars_of tc2 in
  let vars = MetaVarSet.inter vars1 vars2 in
  let reorder_vars, _ = 
    MetaVarSet.fold (fun v (acc, i) -> 
        StrMap.add v i acc, i +1) vars (StrMap.empty, 0) in
  let tc1', map0 = keep_or_drop tc1 t1 reorder_vars IntMap.empty in 
  let tc2', map1 = keep_or_drop tc2 t2 reorder_vars map0 in
  tc1', tc2', map1

let rec gcp tc1 tc2 = 
  match tc1, tc2 with
  | Tree(LeafF t), Tree(LeafF t') when t = t' -> Tree (LeafF t)
  | Tree(Node2F (a, b)), Tree(Node2F(a', b')) -> Tree(Node2F (gcp a a', gcp b b'))
  | Tree(Node3F (a, b, c)), Tree(Node3F(a', b', c')) -> Tree(Node3F (gcp a a', gcp b b', gcp c c'))
  | _ -> Hole(tc1, tc2)

let rec decorate t = 
  match t with 
  | Leaf a -> 
    let s = Printf.sprintf "(Leaf %s)" a in
    {data = LeafF a; dig = Digest.string s}
  | Node2 (a, b)-> 
    let a_h = decorate a in
    let b_h = decorate b in
    let s = Printf.sprintf "(Node2 %s %s)" a_h.dig b_h.dig in
    {data = Node2F (a_h, b_h); dig = Digest.string s}
  | Node3 (a, b, c) -> 
    let a_h = decorate a in
    let b_h = decorate b in
    let c_h = decorate c in
    let s = Printf.sprintf "(Node3 %s %s %s)" a_h.dig b_h.dig c_h.dig in
    {data = Node3F (a_h, b_h, c_h); dig =Digest.string s}

let subtrees t = 
  let rec aux acc t =
    let acc1 = MetaVarSet.add t.dig acc in 
    match t.data with
    | LeafF _ -> acc1
    | Node2F (a, b) -> 
      let acc2 = aux acc1 a in
      aux acc2 b 
    | Node3F (a, b, c) -> 
      let acc2 = aux acc1 a in
      let acc3 = aux acc2 b in
      aux acc3 c in
  aux MetaVarSet.empty t

let get_changes p = 
  List.filter_map (function 
      | (a, b) when a = b -> None
      | h -> Some (Hole h)) (get_holes p)

let wcs s d = 
  let trees1 = subtrees s in 
  let trees2 = subtrees d in
  let inters = MetaVarSet.inter trees1 trees2 in
  fun t -> 
    let h =  t.dig in
    MetaVarSet.find_opt h inters 

let change_tree23 s d = 
  let s_h = decorate s in
  let d_h = decorate d in
  let oracle = wcs s_h d_h in
  postproc s d (extract oracle s_h, extract oracle d_h)

let tree23c_holes t =  MetaVarSet.of_list@@get_holes t

let get_source t = map_holes (fun (s, _) -> s) t

let get_dest t = map_holes (fun (_, d) -> d) t

let closure pat = 
  let rec aux p = 
    match p with
    | Hole (s, d) -> tree23c_holes s, tree23c_holes d, Hole (s, d) 
    | Tree(LeafF l) -> MetaVarSet.empty, MetaVarSet.empty, Tree(LeafF l)
    | Tree(Node2F (a, b)) ->
      let s1, d1, a' = aux a in
      let s2, d2, b' = aux b in
      let s = MetaVarSet.union s1 s2 in
      let d = MetaVarSet.union d1 d2 in
      let res = if MetaVarSet.equal s d
        then Tree(Node2F (a', b'))
        else Hole ((Tree (Node2F (get_source a', get_source b')),Tree (Node2F (get_dest a', get_dest b')))) in
      s, d, res
    | Tree(Node3F (a, b, c)) -> 
      let s1, d1, a' = aux a in
      let s2, d2, b' = aux b in
      let s3, d3, c' = aux c in
      let s = MetaVarSet.union (MetaVarSet.union s1 s2) s3 in
      let d = MetaVarSet.union (MetaVarSet.union d1 d2) d3 in
      let res = if MetaVarSet.equal s d
        then Tree(Node3F (a', b', c'))
        else Hole ((Tree (Node3F (get_source a', get_source b', get_source c')),Tree (Node3F (get_dest a', get_dest b', get_dest c')))) in
      s, d, res
  in 
  let _,_, pat' = aux pat in pat'

let diff_tree23 (s, d) = 
  let t0, t1, map = change_tree23 s d in
  gcp t0 t1, map

let _ = 
  let aux ((t1, t2) as t) =
    let _ = 
      Printf.printf "Tree1 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 t1);
      Printf.printf "Tree2 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 t2);
      Stdlib.flush Stdlib.stdout
    in
    let patch, map = diff_tree23 t in
    let patch =  closure patch in
    if !context == 1 then 
      let _ = print_endline@@Sexp.to_string_hum@@sexp_of_patch23 patch in 
      IntMap.iter (fun i t -> Printf.printf "Hole %i\tTerm %s\n" i (Sexp.to_string_hum@@sexp_of_tree23 t)) map; Stdlib.flush Stdlib.stdout
    else  
      let changes = get_changes patch in
      List.iter (fun c -> 
          let cs = sexp_of_patch23 c in
          print_endline@@Sexp.to_string_hum@@cs) changes in 
  let sexps = load_tree23s !file in
  List.iter (fun x -> aux x; print_newline ()) sexps