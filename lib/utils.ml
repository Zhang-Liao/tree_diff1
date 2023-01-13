open Sexplib.Sexp
open Coq_type
(* ------------------------------------------------------------ *)
(* List operation *)
let rec removelast = function
  | [] -> assert false
  | [_] -> []
  | x::ls -> x :: removelast ls

let replicate x n =
  let rec aux n ls =
    if n <= 0 then ls else aux (n - 1) (x::ls) in
  aux n []

let append_last l i = List.rev (List.rev_append l [i])

let list_remove l i =
  let rec aux l i acc =
    match l with
    | [] -> failwith "The list does not contain the item to remove"
    | hd::tl -> if i = hd then (List.rev tl)@acc else aux tl i (hd::acc) 
  in
  List.rev@@aux l i [] 

(* ------------------------------------------------------------ *)
(* S-expression *)
let parse_hyps hyps =
  let parse_hyp = 
    function
    | List [Atom id; typ] -> LocalAssum (id, term_of_sexp typ)
    | List [Atom id; typ; trm] -> LocalDef (id, term_of_sexp typ, term_of_sexp trm)
    | _ -> failwith "A hypothesis in the dataset has a wrong format of S-expression."
  in
  List.map parse_hyp hyps

let parse_ps ps =
  match ps with
  | List [Atom "State"; List [Atom "Goal"; goal]; List [Atom "Hypotheses"; List hyps]] ->
    (parse_hyps hyps, term_of_sexp goal)
  | _ -> failwith "A proof state in the dataset has a wrong format of S-expression."

(* ------------------------------------------------------------ *)
(* IO *)
let read_lines file  =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc curr_lemma curr_data = match try_read () with
    | Some s -> 
        let split_s = String.split_on_char '\t' s in
        let s_0 = List.hd split_s  in
        if String.equal s_0 "#lemma" then
          let next_lemma = List.nth split_s 1 in 
          loop ((curr_lemma, curr_data)::acc) next_lemma [] 
        else 
          loop acc curr_lemma (s_0::curr_data) 
    | None -> close_in ic; List.rev acc in
  loop [] "" []
