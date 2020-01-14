(* command line interface *)

let usage = "Mini-OCaml compiler / interpreter

Usage:
    miniocaml [options...] souce-file    # as compiler
    miniocaml [options...]               # as interpreter

Options:
    --help, -h        print this help to stderr.

"

type options =
  | Help

let rec repl () =
  print_string "miniocaml # ";
  flush stdout;
  (try
     let ast = Main__Parser.main Main__Lexer.token (Lexing.from_channel stdin) in
     match ast with
     | Some ast ->
       let ty = Main__Tinf.rename_typevar @@ Main__Tinf.get_type ast in
       let v = Main__Eval.eval ast (Main__Eval.emptyenv ()) Main__Eval.ident_cont in
       Printf.printf "- : %s = %s\n" (Main__Type.string_of_type ty) (Main__Value.string_of_value v);
     | None -> ()
   with Failure e ->
     print_endline e;
     flush stdout;
  );
  repl ()

let main () =
  let argc = Array.length Sys.argv in
  let source = ref None in
  let rec process_arguments i =
    if i = argc then ()
    else (
      (match Sys.argv.(i) with
       | "--help" | "-h" ->
         print_string usage;
         exit 0
       | filename ->
         source := Some (Std.input_file ~bin:false filename)
      );
      process_arguments (i + 1)
    )
  in process_arguments 1;
  match !source with
  | Some(code) -> (
      failwith "compiler unimplemented"
    )
  | None -> repl ()
;;

main ()
