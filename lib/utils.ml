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
(* IO *)
let read_lines file  =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc curr_lemma = match try_read () with
    | Some s -> 
        let split_s = String.split_on_char '\t' s in
        let s_0 = List.hd split_s  in
        if String.equal s_0 "#lemma" then
          let next_lemma = List.nth split_s 1 in 
          loop (curr_lemma::acc) {name = next_lemma; data =[]} 
        else 
          loop acc {curr_lemma with data = s_0:: curr_lemma.data} 
    | None -> close_in ic; List.rev acc in
  loop [] {name = ""; data =[]}
