open Exp;;

type value =
  | IntVal  of int
  | BoolVal of bool
  | UnitVal
  | ListVal of value list
  | FunVal of string * exp * (string * value) list
  | RecFunVal of string * string * exp * (string * value) list
  | ContVal of (value -> value)
