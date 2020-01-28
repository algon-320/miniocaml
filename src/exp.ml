type ast_node = Node of exp * int
and exp =
  | Lit         of literal

  | Add         of ast_node * ast_node
  | Sub         of ast_node * ast_node
  | Mul         of ast_node * ast_node
  | Div         of ast_node * ast_node

  | If          of ast_node * ast_node * ast_node
  | Eq          of ast_node * ast_node
  | Ne          of ast_node * ast_node
  | Gt          of ast_node * ast_node
  | Lt          of ast_node * ast_node

  | Var         of string
  | Let         of string * ast_node * ast_node
  | LetRec      of string * string * ast_node * ast_node
  | Fun         of string * ast_node
  | App         of ast_node * ast_node
  | Match       of ast_node * ((pattern * ast_node) list)

  | ListCons    of ast_node * ast_node
  | ListHead    of ast_node
  | ListTail    of ast_node

  | Skip        of ast_node * ast_node
  | Print       of ast_node
  | ReadInt

  | CallCC      of ast_node

  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEq
  | OpNe
  | OpGt
  | OpLt

and literal =
  | CInt of int
  | CBool of bool
  | ListEmpty
  | Unit

and pattern =
  | LiteralPat  of literal
  | WildcardPat of string
  | ListPat     of pattern * pattern

let next_node_id = ref 1
let new_node exp =
  let node = Node(exp, !next_node_id) in
  next_node_id := !next_node_id + 1;
  node

let take_exp node = let Node (e, _) = node in e
let take_id node = let Node (_, id) = node in id

let rec string_of_exp e =
  let string_of_literal l =
    match l with
    | CInt(n) -> Printf.sprintf "%d" n
    | CBool(true) -> Printf.sprintf "true"
    | CBool(false) -> Printf.sprintf "false"
    | Unit -> Printf.sprintf "()"
    | ListEmpty -> Printf.sprintf "[]"
  in
  let rec string_of_exp_impl e parent_level =
    match e with
    | Lit(l) -> string_of_literal l

    | Add(e1, e2) -> with_paren (parent_level > 1) (Printf.sprintf "%s + %s" (string_of_exp_impl (take_exp e1) 1) (string_of_exp_impl (take_exp e2) 1))
    | Sub(e1, e2) -> with_paren (parent_level > 1) (Printf.sprintf "%s - %s" (string_of_exp_impl (take_exp e1) 1) (string_of_exp_impl (take_exp e2) 2))
    | Mul(e1, e2) -> with_paren (parent_level > 2) (Printf.sprintf "%s * %s" (string_of_exp_impl (take_exp e1) 2) (string_of_exp_impl (take_exp e2) 2))
    | Div(e1, e2) -> with_paren (parent_level > 2) (Printf.sprintf "%s / %s" (string_of_exp_impl (take_exp e1) 2) (string_of_exp_impl (take_exp e2) 3))

    | If(cond, e1, e2) -> Printf.sprintf "if %s then %s else %s" (string_of_exp @@ take_exp cond) (string_of_exp (take_exp e1)) (string_of_exp (take_exp e2))
    | Eq(e1, e2) -> Printf.sprintf "%s = %s" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)
    | Ne(e1, e2) -> Printf.sprintf "%s <> %s" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)
    | Gt(e1, e2) -> Printf.sprintf "%s > %s" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)
    | Lt(e1, e2) -> Printf.sprintf "%s < %s" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)

    | Var(var) -> Printf.sprintf "%s" var
    | Let(var, body, stmt) ->  Printf.sprintf "let %s = %s in %s" var (string_of_exp @@ take_exp body) (string_of_exp @@ take_exp stmt)
    | LetRec(f, var, body, stmt) -> Printf.sprintf "let rec %s %s = %s in %s" f var (string_of_exp @@ take_exp body) (string_of_exp @@ take_exp stmt)
    | Fun(var, body) -> Printf.sprintf "func %s -> %s" var (string_of_exp @@ take_exp body)
    | App(e1, e2) -> Printf.sprintf "((%s) (%s))" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)

    | Match(e, _) -> Printf.sprintf "match %s with (.. omitted)" (string_of_exp @@ take_exp e)

    | ListCons(h, t) -> Printf.sprintf "((%s)::%s)" (string_of_exp @@ take_exp h) (string_of_exp @@ take_exp t)
    | ListHead(l) -> Printf.sprintf "ListHead %s" (string_of_exp @@ take_exp l)
    | ListTail(l) -> Printf.sprintf "ListTail %s" (string_of_exp @@ take_exp l)

    | Skip(e1, e2) -> Printf.sprintf "(%s; %s)" (string_of_exp @@ take_exp e1) (string_of_exp @@ take_exp e2)
    | Print(e) -> Printf.sprintf "Print %s" (string_of_exp @@ take_exp e)
    | ReadInt -> Printf.sprintf "ReadInt"

    | CallCC(e) -> Printf.sprintf "CallCC(%s)" (string_of_exp @@ take_exp e)

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

(* ノードIDを無視した比較 *)
let rec (===) tree1 tree2 =
  match (take_exp tree1, take_exp tree2) with
  | Add(a1, b1), Add(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Sub(a1, b1), Sub(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Mul(a1, b1), Mul(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Div(a1, b1), Div(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Eq(a1, b1), Eq(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Ne(a1, b1), Ne(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Gt(a1, b1), Gt(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Lt(a1, b1), Lt(a2, b2) -> (a1 === a2) && (b1 === b2)
  | If(a1, b1, c1), If(a2, b2, c2) -> (a1 === a2) && (b1 === b2) && (c1 === c2)
  | Let(x1, a1, b1), Let(x2, a2, b2) -> (x1 = x2) && (a1 === a2) && (b1 === b2)
  | LetRec(f1, x1, a1, b1), LetRec(f2, x2, a2, b2) ->
    (f1 = f2) && (x1 = x2) && (a1 === a2) && (b1 === b2)
  | Fun(x1, a1), Fun(x2, a2) -> (x1 = x2) && (a1 === a2)
  | App(a1, b1), App(a2, b2) -> (a1 === a2) && (b1 === b2)
  | ListCons(a1, b1), ListCons(a2, b2) -> (a1 === a2) && (b1 === b2)
  | ListHead(a1), ListHead(a2) -> (a1 === a2)
  | ListTail(a1), ListTail(a2) -> (a1 === a2)
  | Skip(a1, b1), Skip(a2, b2) -> (a1 === a2) && (b1 === b2)
  | Print(a1), Print(a2) -> (a1 === a2)
  | CallCC(a1), CallCC(a2) -> (a1 === a2)
  | Match(a1, arms1), Match(a2, arms2) ->
    (a1 === a2) && (
      List.for_all2 (
        fun (a1, e1) (a2, e2) -> (equal_pattern a1 a2) && (e1 === e2)
      ) arms1 arms2
    )
  | (x, y) when (x = y) -> true
  | _ -> false
and equal_literal lit1 lit2 =
  lit1 = lit2
and equal_pattern pat1 pat2 =
  match pat1, pat2 with
  | LiteralPat(lit1), LiteralPat(lit2) -> equal_literal lit1 lit2
  | WildcardPat(n1), WildcardPat(n2) -> n1 = n2
  | ListPat(a1, b1), ListPat(a2, b2) -> (equal_pattern a1 a2) && (equal_pattern b1 b2)
  | _ -> false
