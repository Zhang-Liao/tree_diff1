(* open Tree_diff_lib *)
(* open Coq_lib *)
open Tree_diff_lib
(* open Coq_type *)

let context = ref 1
let file = ref "data/tree23.dat"
let args =
  [
    ("-context", Arg.Set_int context, "Set to 1 to contain the context, 2 to remove context. Default to contain the context.");
    ("-file", Arg.Set_string file, "The file of input data.")
  ]
let usage = "Coq term."
let () = Arg.parse
    args (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

module IntSet = Set.Make(Int)

let _ =
  let _rows = Utils.read_lines !file in ()