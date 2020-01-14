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

let is_atomic = function
  | TInt | TBool | TUnit -> true
  | _ -> false

(* 型を文字列化 *)
let rec string_of_type ty =
  match ty with
  | TInt -> Printf.sprintf "int"
  | TBool -> Printf.sprintf "bool"
  | TUnit -> Printf.sprintf "unit"
  | TVar(s) -> Printf.sprintf "%s" s
  | TArrow(t1, t2) ->
    let (s1, s2) = (string_of_type t1, string_of_type t2) in
    Printf.sprintf "(%s -> %s)" s1 s2
  | TList(t) ->
    let s = string_of_type t in
    Printf.sprintf "(%s) list" s
