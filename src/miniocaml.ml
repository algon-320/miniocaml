(* command line interface *)

let usage = "Mini-OCaml compiler / interpreter

Usage:
    miniocaml [options...] souce-file    # as compiler
    miniocaml [options...]               # as interpreter

Options:
    --help, -h        print this help to stderr.
    -o 'executable'   specify name of the executable. (default: 'a.out')

"

type options =
  | Help

let rec repl () =
  print_string "miniocaml # ";
  flush stdout;
  (try
     let ast = Parser.main Lexer.token (Lexing.from_channel stdin) in
     match ast with
     | Some ast ->
       let ty = Tinf.rename_typevar @@ Tinf.get_type ast in
       let v = Eval.eval ast (Eval.emptyenv ()) Eval.ident_cont in
       Printf.printf "- : %s = %s\n" (Type.string_of_type ty) (Value.string_of_value v);
     | None -> ()
   with Failure e ->
     print_endline e;
     flush stdout;
  );
  repl ()

let compile code_str executable_filename =
  if not (Llvm_executionengine.initialize ()) then
    failwith "target is not supported";
  try
    let ast = match Parser.main Lexer.token (Lexing.from_string code_str) with
      | Some ast ->
        ignore (Tinf.get_type ast);
        ast
      | None ->
        Exp.UnitLit
    in
    let triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple triple in
    let reloc = Llvm_target.RelocMode.PIC in
    let target_machine = Llvm_target.TargetMachine.create target ~triple:triple ~reloc_mode:reloc in
    let mpm = Llvm.PassManager.create () in

    Llvm_scalar_opts.add_instruction_combination mpm;
    Llvm_scalar_opts.add_reassociation mpm;
    Llvm_scalar_opts.add_gvn mpm;
    Llvm_scalar_opts.add_cfg_simplification mpm;

    ignore (Irgen.gen_main ast mpm);

    Random.self_init ();
    let asm_tmp = Printf.sprintf "tmp%d.s" (Random.int 1000000000) in
    (* emit assembly *)
    Llvm_target.TargetMachine.emit_to_file Irgen.the_module Llvm_target.CodeGenFileType.AssemblyFile asm_tmp target_machine;
    (* generate executable using `clang` *)
    ignore (Sys.command @@ Printf.sprintf "clang %s -o %s" asm_tmp executable_filename);
    (* remove assembly file *)
    ignore (Sys.command @@ Printf.sprintf "rm %s" asm_tmp)
  with Failure e ->
    Printf.eprintf "compile error: %s\n" e;
    exit 1

let main () =
  let argc = Array.length Sys.argv in
  let source = ref None in
  let exe_filename = ref "a.out" in
  let rec process_arguments i =
    if i = argc then ()
    else (
      let skip =
        (match Sys.argv.(i) with
         | "--help" | "-h" ->
           print_string usage;
           ignore (exit 0);
           0
         | "-o" ->
           exe_filename := Sys.argv.(i + 1);
           1
         | filename ->
           source := Some (Std.input_file ~bin:false filename);
           0
        ) in
      process_arguments (i + 1 + skip)
    )
  in process_arguments 1;
  match !source with
  | Some(code) -> compile code !exe_filename
  | None -> repl ()
;;

main ()
