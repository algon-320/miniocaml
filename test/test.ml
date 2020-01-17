open Exp
open Value
open Type

let all_passed = ref true

let assert_eq line actual expected printer =
  try
    assert (expected = actual)
  with _ ->
    Printf.printf "Test Failed on file: %s (line: %d)\n" __FILE__ line;
    Printf.printf "expected:\n\t%s\nactual:\n\t%s\n" (printer expected) (printer actual);
    all_passed := false

let assert_exp_eq line actual expected =
  assert_eq line actual expected Exp.string_of_exp
let assert_value_eq line actual expected =
  assert_eq line actual expected Value.string_of_value
let assert_type_eq line actual expected =
  assert_eq line actual expected Type.string_of_type

let test line str exp value ty =
  let act_exp = Main.parse str in
  let act_val = Main.eval str in
  let act_ty = Main.tinf str in
  assert_exp_eq line act_exp exp;
  assert_value_eq line act_val value;
  assert_type_eq line act_ty ty

let () = test __LINE__ "123"
    (IntLit 123) (VInt 123) (TInt)
let () = test __LINE__ "00123"
    (IntLit 123) (VInt 123) (TInt)
let () = test __LINE__ "-123"
    (Sub (IntLit 0, IntLit 123)) (VInt (-123)) (TInt)
let () = test __LINE__ "true"
    (BoolLit true) (VBool true) (TBool)
let () = test __LINE__ "false"
    (BoolLit false) (VBool false) (TBool)
let () = test __LINE__ "()"
    (UnitLit) (VUnit) (TUnit)
let () = test __LINE__ "1 + 1"
    (Add(IntLit 1, IntLit 1)) (VInt 2) (TInt)
let () = test __LINE__ "1 + 2 + 3"
    (Add(Add(IntLit 1, IntLit 2), IntLit 3)) (VInt 6) (TInt)
let () = test __LINE__ "2 - 1"
    (Sub(IntLit 2, IntLit 1)) (VInt 1) (TInt)
let () = test __LINE__ "1 - 2 - 3"
    (Sub(Sub(IntLit 1, IntLit 2), IntLit 3)) (VInt (-4)) (TInt)
let () = test __LINE__ "2 * 1"
    (Mul(IntLit 2, IntLit 1)) (VInt 2) (TInt)
let () = test __LINE__ "2 / 1"
    (Div(IntLit 2, IntLit 1)) (VInt 2) (TInt)

let () = test __LINE__ "let x = 123 in x + x"
    (Let ("x", IntLit 123, Add(Var "x", Var "x")))
    (VInt 246) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f 123"
    (LetRec ("f", "x", Sub(Var "x", IntLit 1), App(Var "f", IntLit 123)))
    (VInt 122) (TInt)
let () = test __LINE__ "let rec f x = x - 1 in f"
    (LetRec ("f", "x", Sub(Var "x", IntLit 1), Var "f"))
    (VClos("f", "x", Sub(Var "x", IntLit 1), [])) (TArrow(TInt, TInt))
let () = test __LINE__ "let rec f x = x in f"
    (LetRec ("f", "x", Var "x", Var "f"))
    (VClos("f", "x", Var "x", [])) (TArrow(TVar"'a", TVar "'a"))
let () = test __LINE__ "let rec f x = x in f 123"
    (LetRec ("f", "x", Var "x", App(Var "f", IntLit 123)))
    (VInt 123) (TInt)
let () = test __LINE__ "fun x -> x"
    (Fun("x", Var "x"))
    (VClos ("$", "x", Var "x", [])) (TArrow(TVar "'a", TVar "'a"))
let () = test __LINE__ "fun x -> ()"
    (Fun("x", UnitLit))
    (VClos ("$", "x", UnitLit, [])) (TArrow(TVar "'a", TUnit))
let () = test __LINE__ "fun x -> (x + 1; x)"
    (Fun("x", Skip(Add(Var "x", IntLit 1), Var "x")))
    (VClos("$", "x", Skip(Add(Var "x", IntLit 1), Var "x"), [])) (TArrow(TInt, TInt))

let () = test __LINE__ "match 123 with | 123 -> true | _ -> false"
    (Match(IntLit 123, [(LiteralPat(IntLit 123), BoolLit true); (WildcardPat("_"), BoolLit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match true with | x -> x | _ -> false"
    (Match(BoolLit true, [(WildcardPat("x"), Var "x"); (WildcardPat("_"), BoolLit false)]))
    (VBool true) (TBool)
let () = test __LINE__ "match false with | x -> x | _ -> false"
    (Match(BoolLit false, [(WildcardPat("x"), Var "x"); (WildcardPat("_"), BoolLit false)]))
    (VBool false) (TBool)
let () = test __LINE__ "match (123::[]) with | [] -> 0 | x::xs -> x"
    (Match(ListCons(IntLit 123, ListEmpty), [(LiteralPat(ListEmpty), IntLit 0); (ListPat(WildcardPat("x"), WildcardPat("xs")), Var "x")]))
    (VInt 123) (TInt)
let () = test __LINE__ "match (123::[]) with [] -> [] | x::xs -> xs"
    (Match(ListCons(IntLit 123, ListEmpty), [(LiteralPat(ListEmpty), ListEmpty); (ListPat(WildcardPat("x"), WildcardPat("xs")), Var "xs")]))
    (VList []) (TList (TInt))

let () = test __LINE__ "[]"
    (ListEmpty)
    (VList []) (TList (TVar "'a"))
let () = test __LINE__ "1::2::3::[]"
    (ListCons(IntLit 1, ListCons(IntLit 2, ListCons(IntLit 3, ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "[1; 2; 3]"
    (ListCons(IntLit 1, ListCons(IntLit 2, ListCons(IntLit 3, ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "1::[2; 3]"
    (ListCons(IntLit 1, ListCons(IntLit 2, ListCons(IntLit 3, ListEmpty))))
    (VList [VInt 1; VInt 2; VInt 3]) (TList TInt)
let () = test __LINE__ "ListHead (1::[])"
    (ListHead (ListCons(IntLit 1, ListEmpty)))
    (VInt 1) (TInt)
let () = test __LINE__ "ListHead ([1]::[])"
    (ListHead (ListCons(ListCons(IntLit 1, ListEmpty), ListEmpty)))
    (VList [VInt 1]) (TList TInt)
let () = test __LINE__ "ListTail (1::[])"
    (ListTail (ListCons(IntLit 1, ListEmpty)))
    (VList []) (TList TInt)

let () = test __LINE__ "Print 123"
    (Print(IntLit 123))
    (VUnit) (TUnit)
let () = test __LINE__ "(); 123"
    (Skip(UnitLit, IntLit 123))
    (VInt 123) (TInt)

let () = if !all_passed then print_string "all tests passed !\n"
