open Exp

type value =
  | VInt  of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VClos of string * string * ast_node * (string * value) list
  | VCont of (value -> value)

let rec string_of_value v =
  match v with
  | VInt(x) -> Printf.sprintf "%d" x
  | VBool(true) -> Printf.sprintf "true"
  | VBool(false) -> Printf.sprintf "false"
  | VList(l) ->
    let rec str_of_list l =
      match l with
      | [] -> ""
      | h::[] -> Printf.sprintf "%s" (string_of_value h)
      | h::t -> Printf.sprintf "%s; %s" (string_of_value h)  (str_of_list t)
    in Printf.sprintf "[%s]" (str_of_list l)
  | VClos(name, arg, body, e) ->
    if name = "$" then
      Printf.sprintf "{fun %s -> %s [%s]}" arg (string_of_exp @@ take_exp body) (string_of_env e)
    else
      Printf.sprintf "{rec-fun %s: %s -> %s [%s]}" name arg (string_of_exp @@ take_exp body) (string_of_env e)
  | VCont(_) ->
    Printf.sprintf "{continuation}"
  | VUnit ->
    Printf.sprintf "()"
and string_of_env env =
  match env with
  | [] -> ""
  | h::t -> let (name, value) = h in
    Printf.sprintf "(%s:%s),%s" name (string_of_value value) (string_of_env t)

(* ノードIDを無視した比較 *)
let rec equal value1 value2 =
  match value1, value2 with
  | VList(l1), VList(l2) ->
    (try List.for_all2 equal l1 l2 with Invalid_argument _ -> false)
  | VClos(f1, a1, b1, e1), VClos(f2, a2, b2, e2) ->
    (f1 = f2) && (a1 = a2) && (b1 === b2) &&
    (List.for_all2 (fun (n1, a1) (n2, a2) -> (n1 = n2) && (equal a1 a2)) e1 e2)
  | x, y when x = y -> true
  | _ -> false