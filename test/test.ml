open Exp
open Value
open Type

let all_passed = ref true

let assert_eq line actual expected eq printer =
  try
    assert (eq expected actual)
  with _ ->
    Printf.printf "Test Failed on file: %s (line: %d)\n" __FILE__ line;
    Printf.printf "expected:\n\t%s\nactual:\n\t%s\n" (printer expected) (printer actual);
    all_passed := false

let assert_exp_eq line actual expected =
  assert_eq line actual expected (===) (fun n -> Exp.string_of_exp @@ Exp.take_exp n)
let assert_value_eq line actual expected =
  assert_eq line actual expected Value.equal Value.string_of_value
let assert_type_eq line actual expected =
  assert_eq line actual expected (=) Type.string_of_type

let unwrap op =
  match op with
  | Some x -> x
  | None -> failwith "unwrap: invalid argument"

let test line str (exp:Exp.ast_node) value ty =
  let parse_str str = Parser_wrapper.parse @@ Lexing.from_string str in
  let tinf str = Tinf.rename_typevar @@ Tinf.get_type @@ unwrap @@ parse_str str in

  let act_exp = unwrap @@ parse_str str in
  let act_val = Eval.eval (unwrap @@ parse_str str) (Eval.emptyenv ()) Eval.ident_cont in
  let act_ty = tinf str in
  assert_exp_eq line act_exp exp;
  assert_value_eq line act_val value;
  assert_type_eq line act_ty ty

let nd = new_node

let int_lit x = Lit(CInt x)
let bool_lit x = Lit(CBool x)
let unit = Lit(Unit)

let () = test __LINE__ "123"
    (nd@@int_lit 123) (VInt 123) (TInt)
let () = test __LINE__ "00123"
    (nd@@int_lit 123) (VInt 123) (TInt)
let () = test __LINE__ "-123"
    (nd@@Sub (nd@@int_lit 0, nd@@int_lit 123)) (VInt (-123)) (TInt)
let () = test __LINE__ "true"
    (nd@@bool_lit true) (VBool true) (TBool)
let () = test __LINE__ "false"
    (nd@@bool_lit false) (VBool false) (TBool)
let () = test __LINE__ "()"
    (nd@@unit) (VUnit) (TUnit)
let () = test __LINE__ "1 + 1"
    (nd@@Add(nd@@int_lit 1, nd@@int_lit 1)) (VInt 2) (TInt)
let () = test __LINE__ "1 + 2 + 3"
    (nd@@Add(nd@@Add(nd@@int_lit 1, nd@@int_lit 2), nd@@int_lit 3)) (VInt 6) (TInt)
let () = test __LINE__ "2 - 1"
    (nd@@Sub(nd@@int_lit 2, nd@@int_lit 1)) (VInt 1) (TInt)
let () = test __LINE__ "1 - 2 - 3"
    (nd@@Sub(nd@@Sub(nd@@int_lit 1, nd@@int_lit 2), nd@@int_lit 3)) (VInt (-4)) (TInt)
let () = test __LINE__ "2 * 1"
    (nd@@Mul(nd@@int_lit 2, nd@@int_lit 1)) (VInt 2) (TInt)
let () = test __LINE__ "2 / 1"
    (nd@@Div(nd@@int_lit 2, nd@@int_lit 1)) (VInt 2) (TInt)

let () = test __LINE__ "let x = 123 in x + x"
    (nd@@Let ("x", nd@@int_lit 123, nd@@Add(nd@@Var "x", nd@@Var "x")))
    (VInt 246) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f 123"
    (nd@@LetRec ("f", "x", nd@@Sub(nd@@Var "x", nd@@int_lit 1), nd@@App(nd@@Var "f", nd@@int_lit 123)))
    (VInt 122) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f"
    (nd@@LetRec ("f", "x", nd@@Sub(nd@@Var "x", nd@@int_lit 1), nd@@Var "f"))
    (VClos("f", "x", nd@@Sub(nd@@Var "x", nd@@int_lit 1), [])) (TArrow(TInt, TInt))
let () = test __LINE__ "let rec f x = x in f"
    (nd@@LetRec ("f", "x", nd@@Var "x", nd@@Var "f"))
    (VClos("f", "x", nd@@Var "x", [])) (TArrow(TVar"'a", TVar "'a"))
let () = test __LINE__ "let rec f x = x in f 123"
    (nd@@LetRec ("f", "x", nd@@Var "x", nd@@App(nd@@Var "f", nd@@int_lit 123)))
    (VInt 123) (TInt)
let () = test __LINE__ "fun x -> x"
    (nd@@Fun("x", nd@@Var "x"))
    (VClos ("$", "x", nd@@Var "x", [])) (TArrow(TVar "'a", TVar "'a"))
let () = test __LINE__ "fun x -> ()"
    (nd@@Fun("x", nd@@unit))
    (VClos ("$", "x", nd@@unit, [])) (TArrow(TVar "'a", TUnit))
let () = test __LINE__ "fun x -> (x + 1; x)"
    (nd@@Fun("x", nd@@Skip(nd@@Add(nd@@Var "x", nd@@int_lit 1), nd@@Var "x")))
    (VClos("$", "x", nd@@Skip(nd@@Add(nd@@Var "x", nd@@int_lit 1), nd@@Var "x"), [])) (TArrow(TInt, TInt))

let () = test __LINE__ "match 123 with | 123 -> true | _ -> false"
    (nd@@Match(nd@@int_lit 123, [(LiteralPat(CInt 123), nd@@bool_lit true); (WildcardPat("_"), nd@@bool_lit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match true with | x -> x | _ -> false"
    (nd@@Match(nd@@bool_lit true, [(WildcardPat("x"), nd@@Var "x"); (WildcardPat("_"), nd@@bool_lit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match false with | x -> x | _ -> false"
    (nd@@Match(nd@@bool_lit false, [(WildcardPat("x"), nd@@Var "x"); (WildcardPat("_"), nd@@bool_lit false)]))
    (VBool false) (TBool)
let () = test __LINE__ "match (123::[]) with | [] -> 0 | x::xs -> x"
    (nd@@Match(nd@@ListCons(nd@@int_lit 123, nd@@Lit(ListEmpty)), [(LiteralPat(ListEmpty), nd@@int_lit 0); (ListPat(WildcardPat("x"), WildcardPat("xs")), nd@@Var "x")]))
    (VInt 123) (TInt)
let () = test __LINE__ "match (123::[]) with [] -> [] | x::xs -> xs"
    (nd@@Match(nd@@ListCons(nd@@int_lit 123, nd@@Lit(ListEmpty)), [(LiteralPat(ListEmpty), nd@@Lit(ListEmpty)); (ListPat(WildcardPat("x"), WildcardPat("xs")), nd@@Var "xs")]))
    (VList []) (TList (TInt))

let () = test __LINE__ "[]"
    (nd@@Lit(ListEmpty))
    (VList []) (TList (TVar "'a"))
let () = test __LINE__ "1::2::3::[]"
    (nd@@ListCons(nd@@int_lit 1, nd@@ListCons(nd@@int_lit 2, nd@@ListCons(nd@@int_lit 3, nd@@Lit(ListEmpty)))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "[1; 2; 3]"
    (nd@@ListCons(nd@@int_lit 1, nd@@ListCons(nd@@int_lit 2, nd@@ListCons(nd@@int_lit 3, nd@@Lit(ListEmpty)))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "1::[2; 3]"
    (nd@@ListCons(nd@@int_lit 1, nd@@ListCons(nd@@int_lit 2, nd@@ListCons(nd@@int_lit 3, nd@@Lit(ListEmpty)))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "ListHead (1::[])"
    (nd@@ListHead (nd@@ListCons(nd@@int_lit 1, nd@@Lit(ListEmpty))))
    (VInt 1) (TInt)
let () = test __LINE__ "ListHead ([1]::[])"
    (nd@@ListHead (nd@@ListCons(nd@@ListCons(nd@@int_lit 1, nd@@Lit(ListEmpty)), nd@@Lit(ListEmpty))))
    (VList [VInt 1]) (TList TInt)
let () = test __LINE__ "ListTail (1::[])"
    (nd@@ListTail (nd@@ListCons(nd@@int_lit 1, nd@@Lit(ListEmpty))))
    (VList []) (TList TInt)

let () = test __LINE__ "Print 123"
    (nd@@Print(nd@@int_lit 123))
    (VUnit) (TUnit)
let () = test __LINE__ "(); 123"
    (nd@@Skip(nd@@unit, nd@@int_lit 123))
    (VInt 123) (TInt)

open Match_completeness
let () = assert (is_exhausted TInt [LiteralPat(CInt 1)] = false)
let () = assert (is_exhausted TInt [LiteralPat(CInt 1); WildcardPat ""] = true)
let () = assert (is_exhausted TInt [WildcardPat ""] = true)

let () = assert (is_exhausted TBool [LiteralPat(CBool true)] = false)
let () = assert (is_exhausted TBool [LiteralPat(CBool true); LiteralPat(CBool false)] = true)
let () = assert (is_exhausted TBool [LiteralPat(CBool true); WildcardPat ""] = true)
let () = assert (is_exhausted TBool [WildcardPat ""] = true)

let () = assert (is_exhausted TUnit [LiteralPat(Unit)] = true)
let () = assert (is_exhausted TUnit [LiteralPat(Unit); WildcardPat ""] = true)
let () = assert (is_exhausted TUnit [WildcardPat ""] = true)

let () = assert (is_exhausted (TList TUnit) [LiteralPat (ListEmpty)] = false)
let () = assert (is_exhausted (TList TUnit) [LiteralPat (ListEmpty); ListPat(WildcardPat "", WildcardPat "")] = true)
let () = assert (is_exhausted (TList TUnit) [LiteralPat (ListEmpty); ListPat(LiteralPat Unit, WildcardPat "")] = true)
let () = assert (is_exhausted (TList TUnit) [LiteralPat (ListEmpty); ListPat(WildcardPat "", LiteralPat(ListEmpty)); ListPat(WildcardPat "", ListPat(LiteralPat Unit, WildcardPat ""))] = true)
let () = assert (is_exhausted (TList TUnit) [WildcardPat ""] = true)

let () = assert (is_exhausted (TList TBool) [ListPat(LiteralPat(CBool true), WildcardPat ""); ListPat(LiteralPat(CBool false), WildcardPat ""); LiteralPat(ListEmpty)] = true)

let () = if !all_passed then print_string "all tests passed !\n"
