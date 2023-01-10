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
  | Some i -> Hole (i, ()) 
  | None -> (
      match t with
      | LeafH (a, _) -> LeafC a
      | Node2H ((a, b), _) -> Node2C (extract o a, extract o b, ())   
      | Node3H ((a, b, c), _) -> Node3C (extract o a, extract o b, extract o c, ()))

let postproc t1 t2 tc1 tc2 = 
  let vars_of t = 
    let rec aux acc = function
      | Hole (i, _) -> MetaVarSet.add i acc
      | LeafC _ -> acc
      | Node2C (a, b, _) -> aux (aux acc a) b
      | Node3C (a, b, c, _) -> aux (aux (aux acc a) b) c
    in
    aux MetaVarSet.empty t in
  let keep_or_drop tc t vars =
    let rec aux tc t = 
      match (tc, t) with
      | Hole (h, _), _ -> (
          match StrMap.find_opt h vars with
          | None -> tree_to_treec t
          (* ???? *)
          | Some reorder_h -> Hole (Stdlib.string_of_int reorder_h, ()))
      | LeafC _, _ -> tc
      | Node2C (a, b, _), Node2(a', b')-> Node2C (aux a a', aux b b', ())
      | Node3C (a, b, c, _), Node3(a', b', c') -> Node3C (aux a a', aux b b', aux c c', ())
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
  | LeafC t, LeafC t' when t = t' -> LeafC t
  | Node2C (a, b, _), Node2C(a', b', _) -> Node2C (gcp a a', gcp b b', ())
  | Node3C (a, b, c, _), Node3C(a', b', c', _) -> Node3C (gcp a a', gcp b b', gcp c c', ())
  | _ -> Hole((tc1, tc2), ())

let get_hash = function
  | LeafH (_, h) 
  | Node2H (_ ,h)
  | Node3H (_, h) -> h

let rec decorate t = 
  let dig = Digest.string@@Sexp.to_string_mach@@sexp_of_tree23 t in
  match t with 
  | Leaf a -> LeafH (a, dig)
  | Node2 (a, b)-> 
    let a_h = decorate a in
    let b_h = decorate b in
    Node2H ((a_h, b_h), dig)
  | Node3 (a, b, c) -> 
    let a_h = decorate a in
    let b_h = decorate b in
    let c_h = decorate c in
    Node3H ((a_h, b_h,c_h), dig)

let subtrees t = 
  let rec aux acc = function
    | LeafH (_, h) -> MetaVarSet.add h acc
    | Node2H ((a, b), h) -> 
      let acc1 = aux acc a in
      let acc2 = aux acc1 b in
      MetaVarSet.add h acc2
    | Node3H ((a, b, c), h) -> 
      let acc1 = aux acc a in
      let acc2 = aux acc1 b in
      let acc3 = aux acc2 c in
      MetaVarSet.add h acc3 in
  aux MetaVarSet.empty t

let wcs s d = 
  let trees1 = subtrees s in 
  let trees2 = subtrees d in
  let inters = MetaVarSet.inter trees1 trees2 in
  fun t -> 
    let h = get_hash t in
    MetaVarSet.find_opt h inters 

let change_tree23 s d = 
  let s_h = decorate s in
  let d_h = decorate d in
  let oracle = wcs s_h d_h in
  postproc s d (extract oracle s_h) (extract oracle d_h)

let tree23c_holes t =
  let rec aux s_hls t = 
    match t with
    | LeafC _ -> s_hls 
    | Hole (h, _) -> MetaVarSet.add h s_hls
    | Node2C (a, b, _) -> 
      let s_hls1 = aux s_hls a in
      aux s_hls1 b
    | Node3C (a, b, c, _) -> 
      let s_hls1 = aux s_hls a in
      let s_hls2 = aux s_hls1 b in
      aux s_hls2 c
  in aux MetaVarSet.empty t

let patch23v_hls = function
  | Hole (_, hls) | Node2C (_, _, hls) | Node3C (_,_, _, hls) -> hls
  | LeafC _ -> MetaVarSet.empty, MetaVarSet.empty, true

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

let rec get_source t = 
  match t with
  | Hole ((s, _), _) -> s
  | Node2C (a, b, info) -> Node2C (get_source a, get_source b, info)
  | Node3C (a, b, c, info) -> Node3C (get_source a, get_source b, get_source c, info)
  | LeafC l -> LeafC l

let rec get_dest t = 
  match t with
  | Hole ((_, d), _) -> d
  | Node2C (a, b, info) -> Node2C (get_dest a, get_dest b, info)
  | Node3C (a, b, c, info) -> Node3C (get_dest a, get_dest b, get_dest c, info)
  | LeafC l -> LeafC l

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
  in aux@@patch_to_patchv patc

let get_changes p = 
  let rec aux acc p =
  match p with
  | Hole (a, b) ->  Hole (a, b) :: acc
  | LeafC _ -> acc
  | Node2C (a, b) ->
    let acc1 = aux acc a in 
    aux acc1 b 
  | Node3C (a, b, c) ->
    let acc1 = aux acc a in 
    let acc2 = aux acc1 b in 
    aux acc2 c 
  in aux [] p

let diff_tree23 (s, d) = (BatPervasives.uncurry gcp)@@change_tree23 s d 

let _ = 
  let aux ((t1, t2) as t) =
    let _ = 
      Printf.printf "Tree1 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 (fun _ -> Sexp.unit) t1);
      Printf.printf "Tree2 %s\n" (Sexp.to_string_hum@@sexp_of_tree23 (fun _ -> Sexp.unit) t2);
      Stdlib.flush Stdlib.stdout
    in
    let patch = closure@@diff_tree23 t in
    if !context == 1 then 
      print_endline@@Sexp.to_string_hum@@patch23_sexpr patch 
    else  
      let changes = get_changes patch in
      List.iter (fun c -> 
        let cs = patch23_sexpr c in
       print_endline@@Sexp.to_string_hum@@cs) changes in 
  let sexps = load_tree23s !file in
  List.iter (fun x -> aux x; print_newline ()) sexps