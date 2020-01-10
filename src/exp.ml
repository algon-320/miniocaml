type exp =
  | IntLit      of int                  (* リテラル *)
  | BoolLit     of bool                 (* 真偽値リテラル *)
  | UnitLit

  | Add         of exp * exp            (* 足し算 *)
  | Sub         of exp * exp            (* 引き算 *)
  | Mul         of exp * exp            (* 掛け算 *)
  | Div         of exp * exp            (* 割り算 *)

  | If          of exp * exp * exp      (* if * then * else *)
  | Eq          of exp * exp            (* = *)
  | Ne          of exp * exp            (* <> *)
  | Gt          of exp * exp            (* > *)
  | Lt          of exp * exp            (* < *)

  | Var         of string               (* 変数へのアクセス *)
  | Let         of string * exp * exp   (* Let式 *)
  | LetRec      of string * string * exp * exp   (* Let rec 式 *)
  | Fun         of string * exp         (* クロージャ式 *)
  | App         of exp * exp            (* 関数適用 *)
  | Match       of exp * ((pattern * exp) list)

  | ListEmpty                           (* 空リスト *)
  | ListCons    of exp * exp            (* h::t *)
  | ListHead    of exp                  (* List.hd list *)
  | ListTail    of exp                  (* List.tl list *)

  | Skip        of exp * exp            (* 左辺を評価(捨てる)→右辺を評価 *)
  | Print       of exp

  | CallCC      of exp                  (* call/cc *)

  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEq
  | OpNe
  | OpGt
  | OpLt

and pattern =
  | LiteralPat  of exp
  | WildcardPat of string
  | ListPat     of pattern * pattern
