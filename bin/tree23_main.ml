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
  | Some _ -> Hole t
  | None -> (
      match t.data with
      | Leaf a -> Tree (Leaf a)
      | Node2 (a, b) -> Tree(Node2 (extract o a, extract o b))   
      | Node3 (a, b, c) -> Tree(Node3 (extract o a, extract o b, extract o c)))

(* evaluate twice ?? *)
let vars_of t = 
  List.fold_left (fun acc th -> MetaVarSet.add th.dig acc)  MetaVarSet.empty (get_holes t) 

let inter_vars t1 t2 =  MetaVarSet.inter (vars_of t1) (vars_of t2)

let postproc tc1 tc2 = 
  let vars = inter_vars tc1 tc2 in
  let keep_or_drop hl =
    match MetaVarSet.find_opt hl.dig vars with
    | None -> treeh_to_treec hl
    | Some _ -> Hole hl 
  in
  map_holes keep_or_drop tc1, map_holes keep_or_drop tc2 

let reorder (tc1, tc2) = 
  let var_terms, _ = List.fold_left (fun (acc, i) th ->
      if StrMap.exists (fun key _ -> String.equal key th.dig) acc then
        acc, i     
      else
        StrMap.add th.dig (treeh_to_tree th, i) acc, i+1) 
      (StrMap.empty, 0) (get_holes tc1) in
  let reorder_vars t = map_holes (fun th -> let _, i = StrMap.find th.dig var_terms in Hole (string_of_int i)) t in
  let reordered_vars = 
    StrMap.fold (fun _ (t, i) acc -> IntMap.add i t acc) var_terms IntMap.empty in 
  reorder_vars tc1, reorder_vars tc2, reordered_vars

let rec gcp tc1 tc2 = 
  match tc1, tc2 with
  | Tree(Leaf t), Tree(Leaf t') when t = t' -> Tree (Leaf t)
  | Tree(Node2 (a, b)), Tree(Node2(a', b')) -> Tree(Node2 (gcp a a', gcp b b'))
  | Tree(Node3 (a, b, c)), Tree(Node3(a', b', c')) -> Tree(Node3 (gcp a a', gcp b b', gcp c c'))
  | _ -> Hole(tc1, tc2)

let rec decorate t = 
  match t with 
  | Leaf a -> 
    let s = Printf.sprintf "(Leaf %s)" a in
    {data = Leaf a; dig = Digest.string s}
  | Node2 (a, b)-> 
    let a_h = decorate a in
    let b_h = decorate b in
    let s = Printf.sprintf "(Node2 %s %s)" a_h.dig b_h.dig in
    {data = Node2 (a_h, b_h); dig = Digest.string s}
  | Node3 (a, b, c) -> 
    let a_h = decorate a in
    let b_h = decorate b in
    let c_h = decorate c in
    let s = Printf.sprintf "(Node3 %s %s %s)" a_h.dig b_h.dig c_h.dig in
    {data = Node3 (a_h, b_h, c_h); dig = Digest.string s}

let subtrees t = 
  let rec aux acc t =
    let acc1 = MetaVarSet.add t.dig acc in 
    match t.data with
    | Leaf _ -> acc1
    | Node2 (a, b) -> 
      let acc2 = aux acc1 a in
      aux acc2 b 
    | Node3 (a, b, c) -> 
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
  reorder@@postproc (extract oracle s_h) (extract oracle d_h)

let tree23c_holes t =  MetaVarSet.of_list@@get_holes t

let get_source t = map_holes (fun (s, _) -> s) t 

let get_dest t = map_holes (fun (_, d) -> d) t 

let closure pat = 
  let rec aux p = 
    match p with
    | Hole (s, d) -> tree23c_holes s, tree23c_holes d, Hole (s, d) 
    | Tree(Leaf l) -> MetaVarSet.empty, MetaVarSet.empty, Tree(Leaf l)
    | Tree(Node2 (a, b)) ->
      let s1, d1, a' = aux a in
      let s2, d2, b' = aux b in
      MetaVarSet.union s1 s2,
      MetaVarSet.union d1 d2,
      if MetaVarSet.equal s1 d1 && MetaVarSet.equal s2 d2
      then Tree(Node2 (a', b'))
      else Hole ((Tree (Node2 (get_source a', get_source b')),Tree (Node2 (get_dest a', get_dest b')))) 
    | Tree(Node3 (a, b, c)) -> 
      let s1, d1, a' = aux a in
      let s2, d2, b' = aux b in
      let s3, d3, c' = aux c in
      MetaVarSet.union (MetaVarSet.union s1 s2) s3,
      MetaVarSet.union (MetaVarSet.union d1 d2) d3,
      if MetaVarSet.equal s1 d1 && MetaVarSet.equal s2 d2 && MetaVarSet.equal s3 d3
      then Tree(Node3 (a', b', c'))
      else Hole ((Tree (Node3 (get_source a', get_source b', get_source c')),Tree (Node3 (get_dest a', get_dest b', get_dest c')))) 
  in 
  let _,_, pat' = aux pat in pat'

let diff_tree23 (s, d) = 
  let t0, t1, map = change_tree23 s d in
  gcp t0 t1, map

let _ = 
  let aux ((t1, t2) as t) =
    let _ = 
      Sexp.pp_hum_indent 4 Format.std_formatter (sexp_of_tree23 t1); Format.print_newline (); (* ??? *)
      Sexp.pp_hum_indent 4 Format.std_formatter (sexp_of_tree23 t2); Format.print_newline ()
    in
    let patch, map = diff_tree23 t in
    let patch = closure patch in
    if !context == 1 then 
      let _ = Sexp.pp_hum_indent 4 Format.std_formatter (sexp_of_patch23 patch); Format.print_newline () in 
      IntMap.iter (fun i t -> 
          Format.pp_print_string Format.std_formatter (Printf.sprintf "Hole %i " i) ;
          Sexp.pp_hum_indent 4 Format.std_formatter (sexp_of_tree23 t); Format.print_newline ()
        ) map
    else  
      let changes = get_changes patch in
      List.iter (fun c -> 
          let cs = sexp_of_patch23 c in
          Sexp.pp_hum_indent 4 Format.std_formatter cs; Format.print_newline ()) changes in 
  let sexps = load_tree23s !file in
  List.iter (fun x -> aux x; Format.print_newline ()) sexps