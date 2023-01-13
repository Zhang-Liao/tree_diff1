(* open Tree_diff_lib *)
(* open Coq_lib *)
open Tree_diff_lib
open Coq_type

module IntSet = Set.Make(Int)

(* let annot term = 
  let rec aux term =
    let hash = hash_term term in
    match term with
    | Rel r -> HRel (r, hash)
    | Evar e -> HEvar (e, hash)
    | Construct (c, u) -> HConstruct ((c, u), hash) 
    | Ind (i, u) -> HInd ((i, u), hash)
    | Var v -> HVar (v, hash) 
    | Const (c, u) -> HConst ((c, u), hash) 
    | Int i -> HInt (i, hash) 
    | Float f -> HFloat (f, hash) 
    | Sort s -> HSort (s, hash)  
    | Meta m -> HMeta (m, hash)
    | LetIn (id, body1, typ, body2) ->
      HLetIn ((id, body1, typ, body2), hash)
    | Case (info, term, typ, cases) ->
      HCase ((info, term, typ, cases), hash)
    | Fix (binder_annots, typs, trms) ->
      HFix ((binder_annots, typs, trms), hash)
    | CoFix (binder_annots, typs, trms) ->
      HCoFix ((binder_annots, typs, trms), hash)
    | Prod (id, typ, body) ->
      let typ' = aux typ in
      let body' = aux body in
      HProd ((id, typ', body'), hash)
    | Lambda (id, typ, body) ->
      HLambda ((id, typ, body), hash)
    | Proj (proj, trm) ->
      let trm = aux trm in
      HProj ((proj, trm), hash)
    | App (head, args) ->
        let head = aux head in
        let args = array_aux args in
        HApp ((head, args), hash)
    | Cast (term, kind, typ) ->
      let term = aux term in
      let typ = aux typ in
      HCast ((term, kind, typ), hash)
  and array_aux trms = 
    Array.map (fun t -> aux t) trms
  in aux term  *)

let _train file =
  let rows = Utils.load_data file in 
  List.fold_left (fun last_row (ps, _)  -> 
    match last_row with
    | None -> Some ps
    | Some _last_r -> None
  ) None rows