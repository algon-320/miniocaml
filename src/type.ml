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
