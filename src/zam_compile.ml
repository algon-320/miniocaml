open Exp;;
open Zam;;

(* 定数伝播のためのヴァリアント *)
type const =
  | IntConst of int
  | BoolConst of bool
  | NonConst
;;

let position x venv =
  let rec impl x venv index =
    match venv with
    | [] -> failwith "no matching variable in environment"
    | (y, is_const)::venv2 ->
      if x = y then (index, is_const)
      else impl x venv2 (index + 1)
  in impl x venv 0

(* コンパイル結果が定数かを返す true / false *)
let rec check_const code =
  if (List.length code) = 1 then
    match code with
    | (ZAM_Ldi _)::[] | (ZAM_Ldb _)::[] -> true
    | _ -> false
  else
    false

(* 補助関数 *)
let int_folding op =
  fun x -> fun y -> ZAM_Ldi (op x y)
let bool_folding op =
  fun x -> fun y -> ZAM_Ldb (op x y)

let rec compile node venv =
  let Node(e, _) = node in
  match e with
  | Fun(x, e) ->
    [ZAM_Closure(compile_tail e ((x, NonConst)::("$", NonConst)::venv))]
  | LetRec(f, x, e1, e2) ->
    [ZAM_Closure(compile_tail e1 ((x, NonConst)::(f, NonConst)::venv))]
    @ [ZAM_Let]
    @ (compile e2 ((f, NonConst)::venv))
    @ [ZAM_EndLet]
  | App(_) ->
    let rec impl node c =
      let Node(e, _) = node in
      match e with
      | App(e1, e2) -> impl e1 (c @ (compile e2 venv))
      | _ -> c @ (compile node venv) @ [ZAM_Apply]
    in impl node [ZAM_PushMark]

  | Var(x) ->
    (
      let pos, is_const = position x venv in
      (* 定数の展開 *)
      match is_const with
      | IntConst n -> [ZAM_Ldi n]
      | BoolConst b -> [ZAM_Ldb b]
      | _ -> [ZAM_Access pos]
    )
  | Lit(lit) -> compile_literal lit venv

  | Add(e1, e2) -> comp_binop e1 e2 venv (int_folding (+)) [ZAM_Add] "Plus"
  | Sub(e1, e2) -> comp_binop e1 e2 venv (int_folding (-)) [ZAM_Sub] "Sub"
  | Mul(e1, e2) -> comp_binop e1 e2 venv (int_folding ( * )) [ZAM_Mul] "Mul"
  | Div(e1, e2) -> comp_binop e1 e2 venv (int_folding (/)) [ZAM_Sub] "Div"
  | Eq(e1, e2) -> comp_binop e1 e2 venv (bool_folding (=)) [ZAM_Eq] "Eq"
  | Ne(e1, e2) -> comp_binop e1 e2 venv (bool_folding (<>)) [ZAM_Ne] "Ne"
  | Gt(e1, e2) -> comp_binop e1 e2 venv (bool_folding (>)) [ZAM_Gt] "Gt"
  | Lt(e1, e2) -> comp_binop e1 e2 venv (bool_folding (<)) [ZAM_Lt] "Lt"

  | Let(x, e1, e2) ->
    (
      let c1 = compile e1 venv in
      if check_const c1 then
        match c1 with
        | (ZAM_Ldi n)::[] -> compile e2 ((x, IntConst n)::venv)
        | (ZAM_Ldb b)::[] -> compile e2 ((x, BoolConst b)::venv)
        | _ -> failwith "unexpected"
      else
        c1 @ [ZAM_Let] @ (compile e2 ((x, NonConst)::venv)) @ [ZAM_EndLet]
    )
  | If(e1, e2, e3) ->
    (
      let c = compile e1 venv in
      if check_const c then
        match c with
        | (ZAM_Ldb true)::[] -> compile e2 venv
        | (ZAM_Ldb false)::[] -> compile e3 venv
        | _ -> failwith "If: condition must be bool"
      else
        c @ [ZAM_Test(compile e2 venv, compile e3 venv)]
    )
  | _ -> failwith "unsupported"

and compile_literal lit venv =
  match lit with
  | CInt(n) -> [ZAM_Ldi(n)]
  | CBool(b) -> [ZAM_Ldb(b)]
  | _ -> failwith "compile_literal: unsupported expression"

and comp_binop e1 e2 venv f tail opname =
  let c1 = compile e1 venv in
  let c2 = compile e2 venv in
  if (check_const c1) && (check_const c2) then
    match c1, c2 with
    | (ZAM_Ldi n1)::[], (ZAM_Ldi n2)::[] -> [f n1 n2]
    | _ -> failwith (opname ^ ": operands must be int")
  else c2 @ c1 @ tail

(* 後続の計算がない場合 *)
and compile_tail node venv =
  let Node(e, _) = node in
  match e with
  | Fun(x, e) ->
    [ZAM_Grab] @ (compile_tail e ((x, NonConst)::("$", NonConst)::venv))
  | LetRec(f, x, e1, e2) ->
    [ZAM_Closure(compile_tail e1 ((x, NonConst)::(f, NonConst)::venv))]
    @ [ZAM_Let]
    @ (compile_tail e2 ((f, NonConst)::venv))
  | App(_) ->
    let rec impl node c =
      let Node(e, _) = node in
      match e with
      | App(e1, e2) -> impl e1 (c @ (compile e2 venv))
      | _ -> c @ (compile node venv) @ [ZAM_TailApply]
    in impl node []

  | Let(x, e1, e2) ->
    (
      let c1 = compile e1 venv in
      if check_const c1 then
        match c1 with
        | (ZAM_Ldi n)::[] -> compile_tail e2 ((x, IntConst n)::venv)
        | (ZAM_Ldb b)::[] -> compile_tail e2 ((x, BoolConst b)::venv)
        | _ -> failwith "unexpected"
      else
        c1 @ [ZAM_Let] @ (compile_tail e2 ((x, NonConst)::venv))
    )
  | If(e1, e2, e3) ->
    (
      let c = compile e1 venv in
      if check_const c then
        match c with
        | (ZAM_Ldb true)::[] -> compile_tail e2 venv
        | (ZAM_Ldb false)::[] -> compile_tail e3 venv
        | _ -> failwith "If: condition must be bool"
      else
        c @ [ZAM_Test(compile_tail e2 venv, compile_tail e3 venv)]
    )

  (* `compile`に丸投げ *)
  | Var(_)
  | Lit(_)
  | Add(_) | Sub(_) | Mul(_) | Div(_)
  | Eq(_) | Ne(_) | Gt(_) | Lt(_) ->
    (compile node venv) @ [ZAM_Return]
  | _ -> failwith "unsupported"
;;
