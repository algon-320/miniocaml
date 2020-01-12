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

let rec string_of_exp e =
  let rec string_of_exp_impl e parent_level =
    match e with
    | IntLit(x) -> Printf.sprintf "%d" x
    | BoolLit(true) -> Printf.sprintf "true"
    | BoolLit(false) -> Printf.sprintf "false"
    | UnitLit -> Printf.sprintf "()"

    | Add(e1, e2) -> with_paren (parent_level > 1) (Printf.sprintf "%s + %s" (string_of_exp_impl e1 1) (string_of_exp_impl e2 1))
    | Sub(e1, e2) -> with_paren (parent_level > 1) (Printf.sprintf "%s - %s" (string_of_exp_impl e1 1) (string_of_exp_impl e2 2))
    | Mul(e1, e2) -> with_paren (parent_level > 2) (Printf.sprintf "%s * %s" (string_of_exp_impl e1 2) (string_of_exp_impl e2 2))
    | Div(e1, e2) -> with_paren (parent_level > 2) (Printf.sprintf "%s / %s" (string_of_exp_impl e1 2) (string_of_exp_impl e2 3))

    | If(cond, e1, e2) -> Printf.sprintf "if %s then %s else %s" (string_of_exp cond) (string_of_exp e1) (string_of_exp e2)
    | Eq(e1, e2) -> Printf.sprintf "%s = %s" (string_of_exp e1) (string_of_exp e2)
    | Ne(e1, e2) -> Printf.sprintf "%s <> %s" (string_of_exp e1) (string_of_exp e2)
    | Gt(e1, e2) -> Printf.sprintf "%s > %s" (string_of_exp e1) (string_of_exp e2)
    | Lt(e1, e2) -> Printf.sprintf "%s < %s" (string_of_exp e1) (string_of_exp e2)

    | Var(var) -> Printf.sprintf "%s" var
    | Let(var, body, stmt) ->  Printf.sprintf "let %s = %s in %s" var (string_of_exp body) (string_of_exp stmt)
    | LetRec(f, var, body, stmt) -> Printf.sprintf "let rec %s %s = %s in %s" f var (string_of_exp body) (string_of_exp stmt)
    | Fun(var, body) -> Printf.sprintf "func %s -> %s" var (string_of_exp body)
    | App(e1, e2) -> Printf.sprintf "(%s) (%s)" (string_of_exp e1) (string_of_exp e2)

    | Match(e, _) -> Printf.sprintf "match %s with (.. omitted)" (string_of_exp e)

    | ListEmpty -> Printf.sprintf "[]"
    | ListCons(h, t) -> Printf.sprintf "(%s::%s)" (string_of_exp h) (string_of_exp t)
    | ListHead(l) -> Printf.sprintf "ListHead %s" (string_of_exp l)
    | ListTail(l) -> Printf.sprintf "ListTail %s" (string_of_exp l)

    | Skip(e1, e2) -> Printf.sprintf "(%s; %s)" (string_of_exp e1) (string_of_exp e2)
    | Print(e) -> Printf.sprintf "Print %s" (string_of_exp e)

    | CallCC(e) -> Printf.sprintf "CallCC(%s)" (string_of_exp e)

    | OpAdd -> Printf.sprintf "(+)"
    | OpSub -> Printf.sprintf "(-)"
    | OpMul -> Printf.sprintf "(*)"
    | OpDiv -> Printf.sprintf "(/)"
    | OpEq  -> Printf.sprintf "(=)"
    | OpNe  -> Printf.sprintf "(<>)"
    | OpGt  -> Printf.sprintf "(>)"
    | OpLt  -> Printf.sprintf "(<)"
  and with_paren p s =
    if p then
      Printf.sprintf "(%s)" s
    else
      s
  in string_of_exp_impl e 0
