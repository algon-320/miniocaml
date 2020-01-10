open Exp;;
open Value;;

let ident_cont = fun x -> x;;

let option_map f opt =
  match opt with
  | Some(x) -> Some(f x)
  | None -> None
;;

(* 環境 *)
let emptyenv () = [];;
let ext env x v =
  (x, v) :: env
;;
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v)::tl ->
    if x = y then v 
    else lookup x tl 
;;
(* 2つの環境を連結する *)
let concat_env e1 e2 = e1 @ e2
;;

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
    | ListCons(h, t) -> Printf.sprintf "%s::%s" (string_of_exp h) (string_of_exp t)
    | ListHead(l) -> Printf.sprintf "ListHead(%s)" (string_of_exp l)
    | ListTail(l) -> Printf.sprintf "ListTail(%s)" (string_of_exp l)

    | Skip(e1, e2) -> Printf.sprintf "%s; %s" (string_of_exp e1) (string_of_exp e2)
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

and string_of_value v =
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
;;

(* 評価器 *)
let rec eval e env cont =
  let binop_int f name e1 e2 env cont =
    eval e2 env (fun y ->
        eval e1 env (fun x ->
            match (x, y) with
            | IntVal(n1), IntVal(n2) -> cont (IntVal(f n1 n2))
            | _ -> failwith (name ^ ": operands must be integer values")
          ))
  in
  let cond_op f_int f_bool f_list name e1 e2 env cont =
    eval e2 env (fun right ->
        eval e1 env (fun left ->
            match (left, right) with
            | IntVal(n1),  IntVal(n2)  -> cont (BoolVal(f_int n1 n2))
            | BoolVal(b1), BoolVal(n2) -> cont (BoolVal(f_bool b1 n2))
            | ListVal(l1), ListVal(l2) -> cont (BoolVal(f_list l1 l2))
            | (x, y) -> failwith (Printf.sprintf "%s: wrong arguments: left=%s, right=%s" name (string_of_value x) (string_of_value y))
          ))
  in
  match e with
  | IntLit(n)     -> cont (IntVal(n))
  | BoolLit(n)    -> cont (BoolVal(n))
  | UnitLit       -> cont UnitVal
  | Var(x)        -> cont (lookup x env)

  | Add(e1, e2)  -> binop_int ( + ) "(+)"   e1 e2 env cont
  | Sub(e1, e2)   -> binop_int ( - ) "(-)"   e1 e2 env cont
  | Mul(e1, e2) -> binop_int ( * ) "( * )" e1 e2 env cont
  | Div(e1, e2)   -> binop_int ( / ) "(/)"   e1 e2 env cont

  | If(cond, e1, e2) -> eval cond env (function
      | BoolVal(true) -> eval e1 env cont
      | BoolVal(false) -> eval e2 env cont
      | _ -> failwith "if: first argument must be bool value"
    )
  | Eq(e1, e2)      -> cond_op (=) (=) (=) "(=)"     e1 e2 env cont
  | Ne(e1, e2)   -> cond_op (<>) (<>) (<>) "(<>)" e1 e2 env cont
  | Gt(e1, e2) -> cond_op (>) (>) (>) "(>)"     e1 e2 env cont
  | Lt(e1, e2)    -> cond_op (<) (<) (<) "(<)"     e1 e2 env cont

  | Let(x, e1, e2) -> eval e1 env (fun body -> eval e2 (ext env x body) cont)
  | LetRec(f, x, e1, e2) -> eval e2 (ext env f (RecFunVal (f, x, e1, env))) cont
  | Fun(e1, e2) -> cont (FunVal(e1, e2, env))
  | App(e1, e2) ->
    eval e2 env (fun arg ->
        eval e1 env (fun f ->
            apply f arg cont
          ))

  | Match(e, patterns) ->
    eval e env (fun v ->
        (* パターンpと値vの一致を判定し、一致するなら束縛を追加した環境をOptionで包んで返す *)
        let rec check p v =
          match p with
          | LiteralPat(lit) ->
            if (eval lit env ident_cont) = v then
              Some(emptyenv ())
            else
              None
          | WildcardPat(w) ->
            (
              match w with
              | "_" -> Some(emptyenv ())
              | name -> Some(ext (emptyenv ()) name v)
            )
          | ListPat(h_pat, t_pat) ->
            (
              match v with
              | ListVal(l) ->
                (
                  let rec list_match l_pat l_val =
                    match (l_pat, l_val) with
                    | (LiteralPat(x), l) when (eval x env ident_cont) = (ListVal l) -> Some(emptyenv ())
                    | (WildcardPat(name), l) -> Some(ext (emptyenv ()) name (ListVal l))
                    | (ListPat(h_p, t_p), h_v::t_v) -> (
                        match (check h_p h_v, list_match t_p t_v) with
                        | (Some(e1), Some(e2)) -> Some(concat_env e1 e2)
                        | _ -> None
                      )
                    | _ -> None
                  in list_match (ListPat(h_pat, t_pat)) l
                )
              | _ -> None
            )
        in
        (* パターンのリストにおいて一致するものを探す *)
        let rec find = function
          | [] -> failwith "match failed"
          | (p, ex)::next -> (
              match check p v with
              | Some(e) -> eval ex (concat_env e env) cont
              | None -> find next
            )
        in find patterns
      )

  | ListEmpty -> cont (ListVal([]))
  | ListCons(h, t) ->
    eval t env (fun t ->
        eval h env (fun h ->
            match (h, t) with
            | (h, ListVal(l)) -> cont (ListVal (h::l))
            | (_, other) -> failwith (Printf.sprintf "ListCons: second argument must be a list: %s" (string_of_value other))
          ))
  | ListHead(l) ->
    eval l env (function
        | ListVal([]) -> failwith "ListHead: argument must not be empty"
        | ListVal(h::_) -> cont (h)
        | _ -> failwith "ListHead: argument must be a list"
      )
  | ListTail(l) ->
    eval l env (function
        | ListVal([]) -> failwith "ListHead: argument must not be empty"
        | ListVal(_::t) -> cont (ListVal(t))
        | _ -> failwith "ListHead: argument must be a list"
      )

  (* e1を評価して結果を捨て、e2を評価した結果を返す *)
  | Skip(e1, e2) -> eval e1 env (fun _ -> eval e2 env cont )

  (* 式の評価結果を画面に表示する *)
  | Print(e) ->
    eval e env (
      fun v ->
        print_string @@ string_of_value v;
        print_newline ();
        cont UnitVal
    )

  | CallCC(e) ->
    eval e env (fun func_val -> apply func_val (ContVal cont) cont)

  | OpAdd -> cont (FunVal("l", Fun("r", Add(Var("l"), Var("r"))), (emptyenv ())))
  | OpSub -> cont (FunVal("l", Fun("r", Sub(Var("l"), Var("r"))), (emptyenv ())))
  | OpMul -> cont (FunVal("l", Fun("r", Mul(Var("l"), Var("r"))), (emptyenv ())))
  | OpDiv -> cont (FunVal("l", Fun("r", Div(Var("l"), Var("r"))), (emptyenv ())))
  | OpEq  -> cont (FunVal("l", Fun("r", Eq(Var("l"),  Var("r"))), (emptyenv ())))
  | OpNe  -> cont (FunVal("l", Fun("r", Ne(Var("l"),  Var("r"))), (emptyenv ())))
  | OpGt  -> cont (FunVal("l", Fun("r", Gt(Var("l"),  Var("r"))), (emptyenv ())))
  | OpLt  -> cont (FunVal("l", Fun("r", Lt(Var("l"),  Var("r"))), (emptyenv ())))

and apply f arg cont =
  match f with
  | FunVal(var, body, closure_e) ->
    let e = ext closure_e var arg in
    eval body e cont
  | RecFunVal(name, var, body, closure_e) ->
    let e = ext (ext closure_e var arg) name f in
    eval body e cont
  | ContVal(cont) -> cont arg
  | x -> failwith (Printf.sprintf "(Function Apply): first argument must be a function value, but `%s`" (string_of_value x))
;;
