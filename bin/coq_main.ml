(* open Tree_diff_lib *)
(* open Coq_lib *)
open Tree_diff_lib
open Coq_type

module IntSet = Set.Make(Int)

let _train file =
  let rows = Utils.load_data file in 
  List.fold_left (fun last_row (ps, _)  -> 
    match last_row with
    | None -> Some ps
    | Some _last_r -> None
  ) None rows