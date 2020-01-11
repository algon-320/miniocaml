open Exp

type value =
  | IntVal  of int
  | BoolVal of bool
  | UnitVal
  | ListVal of value list
  | FunVal of string * exp * (string * value) list
  | RecFunVal of string * string * exp * (string * value) list
  | ContVal of (value -> value)

let rec string_of_value v =
  match v with
  | IntVal(x) -> Printf.sprintf "%d" x
  | BoolVal(true) -> Printf.sprintf "true"
  | BoolVal(false) -> Printf.sprintf "false"
  | ListVal(l) ->
    let rec str_of_list l =
      match l with
      | [] -> ""
      | h::[] -> Printf.sprintf "%s" (string_of_value h)
      | h::t -> Printf.sprintf "%s; %s" (string_of_value h)  (str_of_list t)
    in Printf.sprintf "[%s]" (str_of_list l)
  | FunVal(arg, body, e) ->
    Printf.sprintf "{fun %s -> %s [%s]}" arg (string_of_exp body) (string_of_env e)
  | RecFunVal(f, arg, body, e) ->
    Printf.sprintf "{rec-fun %s: %s -> %s [%s]}" f arg (string_of_exp body) (string_of_env e)
  | ContVal(_) ->
    Printf.sprintf "{continuation}"
  | UnitVal ->
    Printf.sprintf "()"
and string_of_env env =
  match env with
  | [] -> ""
  | h::t -> let (name, value) = h in
    Printf.sprintf "(%s:%s),%s" name (string_of_value value) (string_of_env t)
