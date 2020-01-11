type tyvar = string
type ty =
  | TInt
  | TBool
  | TUnit
  | TArrow of ty * ty
  | TVar of tyvar
  | TList of ty
type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

(* 型を文字列化 *)
let rec string_of_type ty =
  match ty with
  | TInt -> Printf.sprintf "TInt"
  | TBool -> Printf.sprintf "TBool"
  | TUnit -> Printf.sprintf "TUnit"
  | TVar(s) -> Printf.sprintf "%s" s
  | TArrow(t1, t2) ->
    let (s1, s2) = (string_of_type t1, string_of_type t2) in
    Printf.sprintf "(%s -> %s)" s1 s2
  | TList(t) ->
    let s = string_of_type t in
    Printf.sprintf "(%s) list" s
