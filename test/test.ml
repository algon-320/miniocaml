open Main__Exp
let assert_exp_eq expected actual =
  try
    assert (expected = actual)
  with _ ->
    Printf.printf "Test Failed on file: %s (line: %d)\n" __FILE__ __LINE__;
    Printf.printf "expected:\n\t%s\nactual:\n\t%s\n" (Main__Eval.string_of_exp expected) (Main__Eval.string_of_exp actual);
    exit 1

let () = assert_exp_eq (Add(IntLit 1, IntLit 1)) (Main.parse "1 + 1")
let () = assert_exp_eq (Sub(IntLit 2, IntLit 1)) (Main.parse "2 - 1")
let () = print_string "all tests passed !\n"
