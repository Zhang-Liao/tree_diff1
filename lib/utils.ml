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
let read_lines file : string list =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> 
        let split_s = String.split_on_char '\t' s in
        if String.equal (List.hd split_s) "#lemma" then 
        loop acc else loop (s::acc)
    | None -> close_in ic; List.rev acc in
  loop []

let load_data f =
  let load_row r = 
    let r = String.split_on_char '\t' r in
    let ps = List.hd r in
    let tac = List.nth r 1 in
    ps, tac in
  let ls = read_lines f in
  List.map load_row ls
