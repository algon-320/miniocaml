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

let test line str (exp:Exp.ast_node) value ty =
  let act_exp = Main.parse str in
  let act_val = Main.eval str in
  let act_ty = Main.tinf str in
  assert_exp_eq line act_exp exp;
  assert_value_eq line act_val value;
  assert_type_eq line act_ty ty

let nd = new_node

let () = test __LINE__ "123"
    (nd@@IntLit 123) (VInt 123) (TInt)
let () = test __LINE__ "00123"
    (nd@@IntLit 123) (VInt 123) (TInt)
let () = test __LINE__ "-123"
    (nd@@Sub (nd@@IntLit 0, nd@@IntLit 123)) (VInt (-123)) (TInt)
let () = test __LINE__ "true"
    (nd@@BoolLit true) (VBool true) (TBool)
let () = test __LINE__ "false"
    (nd@@BoolLit false) (VBool false) (TBool)
let () = test __LINE__ "()"
    (nd@@UnitLit) (VUnit) (TUnit)
let () = test __LINE__ "1 + 1"
    (nd@@Add(nd@@IntLit 1, nd@@IntLit 1)) (VInt 2) (TInt)
let () = test __LINE__ "1 + 2 + 3"
    (nd@@Add(nd@@Add(nd@@IntLit 1, nd@@IntLit 2), nd@@IntLit 3)) (VInt 6) (TInt)
let () = test __LINE__ "2 - 1"
    (nd@@Sub(nd@@IntLit 2, nd@@IntLit 1)) (VInt 1) (TInt)
let () = test __LINE__ "1 - 2 - 3"
    (nd@@Sub(nd@@Sub(nd@@IntLit 1, nd@@IntLit 2), nd@@IntLit 3)) (VInt (-4)) (TInt)
let () = test __LINE__ "2 * 1"
    (nd@@Mul(nd@@IntLit 2, nd@@IntLit 1)) (VInt 2) (TInt)
let () = test __LINE__ "2 / 1"
    (nd@@Div(nd@@IntLit 2, nd@@IntLit 1)) (VInt 2) (TInt)

let () = test __LINE__ "let x = 123 in x + x"
    (nd@@Let ("x", nd@@IntLit 123, nd@@Add(nd@@Var "x", nd@@Var "x")))
    (VInt 246) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f 123"
    (nd@@LetRec ("f", "x", nd@@Sub(nd@@Var "x", nd@@IntLit 1), nd@@App(nd@@Var "f", nd@@IntLit 123)))
    (VInt 122) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f"
    (nd@@LetRec ("f", "x", nd@@Sub(nd@@Var "x", nd@@IntLit 1), nd@@Var "f"))
    (VClos("f", "x", nd@@Sub(nd@@Var "x", nd@@IntLit 1), [])) (TArrow(TInt, TInt))
let () = test __LINE__ "let rec f x = x in f"
    (nd@@LetRec ("f", "x", nd@@Var "x", nd@@Var "f"))
    (VClos("f", "x", nd@@Var "x", [])) (TArrow(TVar"'a", TVar "'a"))
let () = test __LINE__ "let rec f x = x in f 123"
    (nd@@LetRec ("f", "x", nd@@Var "x", nd@@App(nd@@Var "f", nd@@IntLit 123)))
    (VInt 123) (TInt)
let () = test __LINE__ "fun x -> x"
    (nd@@Fun("x", nd@@Var "x"))
    (VClos ("$", "x", nd@@Var "x", [])) (TArrow(TVar "'a", TVar "'a"))
let () = test __LINE__ "fun x -> ()"
    (nd@@Fun("x", nd@@UnitLit))
    (VClos ("$", "x", nd@@UnitLit, [])) (TArrow(TVar "'a", TUnit))
let () = test __LINE__ "fun x -> (x + 1; x)"
    (nd@@Fun("x", nd@@Skip(nd@@Add(nd@@Var "x", nd@@IntLit 1), nd@@Var "x")))
    (VClos("$", "x", nd@@Skip(nd@@Add(nd@@Var "x", nd@@IntLit 1), nd@@Var "x"), [])) (TArrow(TInt, TInt))

let () = test __LINE__ "match 123 with | 123 -> true | _ -> false"
    (nd@@Match(nd@@IntLit 123, [(LiteralPat(nd@@IntLit 123), nd@@BoolLit true); (WildcardPat("_"), nd@@BoolLit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match true with | x -> x | _ -> false"
    (nd@@Match(nd@@BoolLit true, [(WildcardPat("x"), nd@@Var "x"); (WildcardPat("_"), nd@@BoolLit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match false with | x -> x | _ -> false"
    (nd@@Match(nd@@BoolLit false, [(WildcardPat("x"), nd@@Var "x"); (WildcardPat("_"), nd@@BoolLit false)]))
    (VBool false) (TBool)
let () = test __LINE__ "match (123::[]) with | [] -> 0 | x::xs -> x"
    (nd@@Match(nd@@ListCons(nd@@IntLit 123, nd@@ListEmpty), [(LiteralPat(nd@@ListEmpty), nd@@IntLit 0); (ListPat(WildcardPat("x"), WildcardPat("xs")), nd@@Var "x")]))
    (VInt 123) (TInt)
let () = test __LINE__ "match (123::[]) with [] -> [] | x::xs -> xs"
    (nd@@Match(nd@@ListCons(nd@@IntLit 123, nd@@ListEmpty), [(LiteralPat(nd@@ListEmpty), nd@@ListEmpty); (ListPat(WildcardPat("x"), WildcardPat("xs")), nd@@Var "xs")]))
    (VList []) (TList (TInt))

let () = test __LINE__ "[]"
    (nd@@ListEmpty)
    (VList []) (TList (TVar "'a"))
let () = test __LINE__ "1::2::3::[]"
    (nd@@ListCons(nd@@IntLit 1, nd@@ListCons(nd@@IntLit 2, nd@@ListCons(nd@@IntLit 3, nd@@ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "[1; 2; 3]"
    (nd@@ListCons(nd@@IntLit 1, nd@@ListCons(nd@@IntLit 2, nd@@ListCons(nd@@IntLit 3, nd@@ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "1::[2; 3]"
    (nd@@ListCons(nd@@IntLit 1, nd@@ListCons(nd@@IntLit 2, nd@@ListCons(nd@@IntLit 3, nd@@ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "ListHead (1::[])"
    (nd@@ListHead (nd@@ListCons(nd@@IntLit 1, nd@@ListEmpty)))
    (VInt 1) (TInt)
let () = test __LINE__ "ListHead ([1]::[])"
    (nd@@ListHead (nd@@ListCons(nd@@ListCons(nd@@IntLit 1, nd@@ListEmpty), nd@@ListEmpty)))
    (VList [VInt 1]) (TList TInt)
let () = test __LINE__ "ListTail (1::[])"
    (nd@@ListTail (nd@@ListCons(nd@@IntLit 1, nd@@ListEmpty)))
    (VList []) (TList TInt)

let () = test __LINE__ "Print 123"
    (nd@@Print(nd@@IntLit 123))
    (VUnit) (TUnit)
let () = test __LINE__ "(); 123"
    (nd@@Skip(nd@@UnitLit, nd@@IntLit 123))
    (VInt 123) (TInt)

let () = if !all_passed then print_string "all tests passed !\n"
