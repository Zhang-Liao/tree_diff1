open Sexplib
open Tree_diff_lib.Tree23lib

let context = ref false
let file = ref "data/tree23.dat"
let args = [("-context", Arg.Unit (fun () -> context := true), "Contain the context or not.");
            ("-file", Arg.Set_string file, "The file of input data.")]
let () = Arg.parse args (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) "Tree23."

module StrMap = Map.Make(String)
module IntMap = Map.Make(Int)

let postproc tc1 tc2 =
  let vars_of t = List.fold_left (fun acc th -> MetaVarSet.add th.dig acc) MetaVarSet.empty (get_holes t) in
  let keep_or_drop hl = if MetaVarSet.mem hl.dig (MetaVarSet.inter (vars_of tc1) (vars_of tc2)) then Hole hl else treeh_to_treec hl in
  map_tree23c keep_or_drop tc1, map_tree23c keep_or_drop tc2

let rec gcp tc1 tc2 = match tc1, tc2 with
  | Tree(Leaf t), Tree(Leaf t') when t = t' -> Tree (Leaf t)
  | Tree(Node2 (a, b)), Tree(Node2(a', b')) -> Tree(Node2 (gcp a a', gcp b b'))
  | Tree(Node3 (a, b, c)), Tree(Node3(a', b', c')) -> Tree(Node3 (gcp a a', gcp b b', gcp c c'))
  | _ -> Hole(tc1, tc2)

let change_tree23 (s, d) =
  let rec extract oracle t = if oracle t then Hole t else Tree (map_tree_functor (extract oracle) t.data) in
  let rec decorate t =
    let decorate_t = map_tree_functor (fun x -> decorate x) t in {data = decorate_t; dig = match   decorate_t with
        | Leaf a -> Printf.sprintf "(Leaf %s)" a
        | Node2 (a, b) -> Printf.sprintf "(Node2 %s %s)" a.dig b.dig
        | Node3(a, b, c) -> Printf.sprintf "(Node3 %s %s %s)" a.dig b.dig c.dig} in
  let rec subtrees t acc = fold_tree_functor (fun acc curr_t -> subtrees curr_t acc) (MetaVarSet.add t.dig acc) t.data in
  let s_h = decorate s in
  let d_h = decorate d in
  let oracle =
    let inters = MetaVarSet.inter (subtrees s_h MetaVarSet.empty) (subtrees d_h MetaVarSet.empty) in
    fun t -> MetaVarSet.mem t.dig inters in
  postproc (extract oracle s_h) (extract oracle d_h)

let get_source t = map_tree23c (fun (s, _) -> s) t

let closure pat =
  let tree23c_holes t = MetaVarSet.of_list (List.map (fun x-> x.dig) (get_holes t)) in
  let comb (s1, d1, eq) ((s2, d2), _) =
    MetaVarSet.union s1 s2, MetaVarSet.union d1 d2, eq && MetaVarSet.equal s2 d2 in
  let rec aux = function
    | Hole (s, d) -> (tree23c_holes s, tree23c_holes d), Hole (s, d)
    | Tree t ->
      let t = map_tree_functor aux t in
      let s, d, eq = fold_tree_functor comb (MetaVarSet.empty, MetaVarSet.empty, true) t in
      let t = Tree (map_tree_functor snd t) in
      (s, d), if eq then t else Hole (map_tree23c fst t, map_tree23c snd t)
  in snd@@aux pat

let subst_ident patc =
  let pat = map_tree23c (function
      | (Hole a, Hole b) when a.dig = b.dig -> treeh_to_treec a
      | h -> Hole h) patc in
  let var_terms, _ = List.fold_left (fun (acc, i) th ->
      if StrMap.exists (fun key _ -> String.equal key th.dig) acc then acc, i
      else StrMap.add th.dig (treeh_to_tree th, i) acc, i+1)
      (StrMap.empty, 0) (get_holes@@get_source pat) in
  let reorder_vars t = map_tree23c (fun th ->
      let _, i = StrMap.find th.dig var_terms in Hole (string_of_int i)) t in
  map_tree23c (fun (a, b) -> Hole (reorder_vars a, reorder_vars b)) pat, StrMap.fold (fun _ (t, i) acc -> IntMap.add i t acc) var_terms IntMap.empty

let _ = List.iter (fun ((t1, t2) as t) ->
    print_endline "Tree1"; print_endline@@Sexp.to_string_hum@@sexp_of_tree23 t1; print_endline "Tree2"; print_endline@@Sexp.to_string_hum@@sexp_of_tree23 t2;
    let patch, map = subst_ident@@closure@@(Batteries.uncurry gcp)@@change_tree23 t in
    if !context then (
      print_endline "Patch"; print_endline@@Sexp.to_string_hum@@sexp_of_patch23 patch;
      IntMap.iter (fun i t -> print_endline@@Printf.sprintf "Hole %i" i;
                    print_endline@@Sexp.to_string_hum@@sexp_of_tree23 t) map)
    else
      List.iter (fun c -> print_endline "Change"; print_endline@@Sexp.to_string_hum@@sexp_of_change23 sexp_of_metavar c) (get_holes patch)
  ; print_newline ()) (load_tree23s !file)