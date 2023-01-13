open Sexplib.Conv
type term =
| Rel       of int * string
| Var       of string
| Meta      of int
| Evar      of int * term list
| Sort      of string list
| Cast      of term * string * term
| Prod      of string * string * term * term
| Lambda    of string * string * term * term
| LetIn     of string * string * term * term * term
| App       of term * term array
| Const     of string * string list 
| Ind       of string * string list
| Construct of string list * string list
| Case      of string * term * term * term array
| Fix       of string list * term list * term list
| CoFix     of string list * term list * term list
| Proj      of string * term 
| Int       of int
| Float     of float [@@deriving sexp]

type hyp = 
(* id * type *)
| LocalAssum of string * term
(* id * term * type *)
| LocalDef of string * term * term


let hyp_id = function
  | LocalAssum (id, _) | LocalDef (id, _, _) -> id

(**********************************************************************)
(* Hterm *)
type metavar = int
type hterm =
  | HRel       of int * metavar
  | HEvar      of (int * term list) * metavar
  | HConstruct of ((string * string) * string list) * metavar
  | HInd       of (string * string list) * metavar
  | HVar       of string * metavar
  | HConst     of (string * string list) * metavar 
  | HInt       of int * metavar
  | HFloat     of float * metavar
  | HSort      of string list * metavar
  | HMeta      of int * metavar
  | HCast      of (hterm * string * hterm) * metavar
  | HProd      of ((string * string) * hterm * hterm) * metavar
  | HApp       of (hterm * hterm array) * metavar
  | HProj      of (string * hterm) * metavar
  | HFix       of (string list * term list * term list) * metavar
  | HCoFix     of (string list * term list * term list) * metavar
  | HHole      of metavar
  | HLambda    of ((string * string) * term * term) * metavar
  | HLetIn     of ((string * string) * term * term * term) * metavar
  | HCase      of (string * term * term * term array) * metavar