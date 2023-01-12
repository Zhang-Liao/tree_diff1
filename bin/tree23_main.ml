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

let rec extract o t = 
  match o t with
  | Some i -> Hole i
  | None -> (
      match t.data with
      | LeafF a -> Tree (LeafF a)
      | Node2F (a, b) -> Tree(Node2F (extract o a, extract o b))   
      | Node3F (a, b, c) -> Tree(Node3F (extract o a, extract o b, extract o c)))

let postproc t1 t2 tc1 tc2 = 
  let vars_of t = 
    List.fold_left (fun acc v -> MetaVarSet.add v acc)  MetaVarSet.empty (get_holes t) in
  let keep_or_drop tc t vars =
    let rec aux tc t = 
      match (tc, t) with
      | Hole h, _ -> (
          match StrMap.find_opt h vars with
          | None -> tree_to_treec t
          | Some reorder_h -> Hole (Stdlib.string_of_int reorder_h))
      | Tree(LeafF _), _ -> tc
      | Tree(Node2F (a, b)), Node2(a', b')-> Tree(Node2F (aux a a', aux b b'))
      | Tree(Node3F (a, b, c)), Node3(a', b', c') -> Tree(Node3F (aux a a', aux b b', aux c c'))
      | _ -> failwith "tree23c's node is inconsistent with tree23's node" 
    in aux tc t 
  in
  let vars1 = vars_of tc1 in
  let vars2 = vars_of tc2 in
  let vars = MetaVarSet.inter vars1 vars2 in
  let reorder_vars, _ = 
    MetaVarSet.fold (fun v (acc, i) -> 
        StrMap.add v i acc, i +1) vars (StrMap.empty, 0) in
  keep_or_drop tc1 t1 reorder_vars, keep_or_drop tc2 t2 reorder_vars

let rec gcp tc1 tc2 = 
  match tc1, tc2 with
  | Tree(LeafF t), Tree(LeafF t') when t = t' -> Tree (LeafF t)
  | Tree(Node2F (a, b)), Tree(Node2F(a', b')) -> Tree(Node2F (gcp a a', gcp b b'))
  | Tree(Node3F (a, b, c)), Tree(Node3F(a', b', c')) -> Tree(Node3F (gcp a a', gcp b b', gcp c c'))
  | _ -> Hole(tc1, tc2)

let rec decorate t = 
  let dig = Digest.string@@Sexp.to_string_mach@@sexp_of_tree23 t in
  match t with 
  | Leaf a -> {data = LeafF a; dig = dig}
  | Node2 (a, b)-> 
    let a_h = decorate a in
    let b_h = decorate b in
    {data = Node2F (a_h, b_h); dig = dig}
  | Node3 (a, b, c) -> 
    let a_h = decorate a in
    let b_h = decorate b in
    let c_h = decorate c in
    {data = Node3F (a_h, b_h,c_h); dig =dig}

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
  List.map (fun h -> Hole h) (get_holes p)

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
  postproc s d (extract oracle s_h) (extract oracle d_h)

let tree23c_holes t =  MetaVarSet.of_list@@get_holes t

let get_source t = map_holes (fun (s, _) -> s) t

let get_dest t = map_holes (fun (d, _) -> d) t

(* 

let rec patch_to_patchv (p:patch23) = 
  match p with
  | Hole ((s, d), _) -> 
    let s_hls = tree23c_holes s in
    let d_hls = tree23c_holes d in
    Hole ((s, d), (s_hls, d_hls, MetaVarSet.equal s_hls d_hls))
  | LeafC l -> LeafC l 
  | Node2C (a, b, _) -> 
    let av = patch_to_patchv a in
    let bv = patch_to_patchv b in
    let s_hls1, d_hls1, _ = patch23v_hls av in
    let s_hls2, d_hls2, _ = patch23v_hls bv in
    let s_hls = MetaVarSet.union s_hls1 s_hls2 in
    let d_hls = MetaVarSet.union d_hls1 d_hls2 in 
    Node2C (av, bv, (s_hls, d_hls, MetaVarSet.equal s_hls d_hls))
  | Node3C (a, b, c, _) -> 
    let av = patch_to_patchv a in
    let bv = patch_to_patchv b in
    let cv = patch_to_patchv c in
    let s_hls1, d_hls1, _ = patch23v_hls av in
    let s_hls2, d_hls2, _ = patch23v_hls bv in
    let s_hls3, d_hls3, _ = patch23v_hls cv in
    let s_hls = MetaVarSet.union s_hls3 (MetaVarSet.union s_hls1 s_hls2) in
    let d_hls = MetaVarSet.union d_hls3 (MetaVarSet.union d_hls1 d_hls2) in
    Node3C (av, bv, cv, (s_hls, d_hls, MetaVarSet.equal s_hls d_hls))    

let is_closed = function
  | Hole (_, (_,_, c)) | Node2C(_,_, (_,_, c)) | Node3C (_, _, _,(_,_, c)) -> c
  | LeafC _ -> true



let closure patc = 
  let rec aux p = 
    match p with
    | Hole (hl, _) -> Hole (hl, ())  
    | LeafC l -> LeafC l
    | Node2C (a , b, _) ->
      if is_closed p && ((is_closed a && is_closed b) == false) 
      then Hole ((Node2C (get_source a, get_source b, ()), Node2C (get_dest a, get_dest b, ())), ()) 
      else Node2C (aux a, aux b, ())
    | Node3C (a, b, c, _) -> 
      if is_closed p && ((is_closed a && is_closed b && is_closed c) == false) 
      then Hole ((Node3C (get_source a, get_source b, get_source c, ()), Node3C (get_dest a, get_dest b, get_dest c, ())), ())
      else Node3C (aux a, aux b, aux c, ())
  in aux@@patch_to_patchv patc *)


let diff_tree23 (s, d) = (BatPervasives.uncurry gcp)@@change_tree23 s d 

let _ = 
  let aux ((t1, t2) as t) =
    let _ = 
      Printf.printf "Tree1 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 t1);
      Printf.printf "Tree2 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 t2);
      Stdlib.flush Stdlib.stdout
    in
    let patch = (* closure@@ *) diff_tree23 t in
    if !context == 1 then 
      print_endline@@Sexp.to_string_hum@@sexp_of_patch23 patch 
    else  
      let changes = get_changes patch in
      List.iter (fun c -> 
        let cs = sexp_of_patch23 c in
       print_endline@@Sexp.to_string_hum@@cs) changes in 
  let sexps = load_tree23s !file in
  List.iter (fun x -> aux x; print_newline ()) sexps