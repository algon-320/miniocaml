(* command line interface *)

let usage = "Mini-OCaml compiler / interpreter

Usage:
    miniocaml [repl-options...]                  # as interpreter
    miniocaml [compile-options...] souce-file    # as compiler

repl-options:
    --help, -h        print this help to stderr.
compile-options:
    --help, -h        print this help to stderr.
    -o 'executable'   specify name of the executable. (default: 'a.out')
    -emit-llvm        emit LLVM IR

"

let rec repl () =
  print_string "miniocaml # ";
  flush stdout;
  (
    try
      let ast = Parser_wrapper.parse @@ Lexing.from_channel stdin in
      match ast with
      | Some ast ->
        let ty = Tinf.rename_typevar @@ Tinf.get_type ast in
        (
          if not (Match_completeness.all_match_exhausted ast) then
            failwith "pattern matching is not exhausted"
        );
        let v = Eval.eval ast (Eval.emptyenv ()) Eval.ident_cont in
        Printf.printf "- : %s = %s\n" (Type.string_of_type ty) (Value.string_of_value v);
      | None -> ()
    with
    | Parser_state.Exit ->
      print_newline ();
      exit 0;
    | Failure e ->
      print_endline e;
      flush stdout;
  );
  repl ()


type options =
  | Executable of string
  | EmitLLVM

let compile filename options =
  if not (Llvm_executionengine.initialize ()) then
    failwith "target is not supported";
  try
    let executable_filename = ref "a.out" in
    let emit_llvm = ref false in

    let rec option_extract = function
      | [] -> ()
      | x::xs -> (
          match x with
          | Executable(exe) -> executable_filename := exe
          | EmitLLVM -> emit_llvm := true
        ); option_extract xs
    in option_extract options;

    let code_str = Std.input_file ~bin:false filename in
    let ast = match Parser_wrapper.parse @@ Lexing.from_string code_str with
      | Some ast ->
        let (_, _, theta, _) = Tinf.tinf [] ast 0 in
        Tinf.update_type_info theta;
        ast
      | None ->
        Exp.new_node @@ Exp.Lit(Exp.Unit)
    in

    let triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple triple in
    let reloc = Llvm_target.RelocMode.PIC in
    let target_machine = Llvm_target.TargetMachine.create target ~triple:triple ~reloc_mode:reloc in
    let mpm = Llvm.PassManager.create () in

    (* LLVM optimization passes *)
    Llvm_scalar_opts.add_instruction_combination mpm;
    Llvm_scalar_opts.add_memory_to_register_promotion mpm;
    Llvm_scalar_opts.add_memcpy_opt mpm;
    Llvm_scalar_opts.add_tail_call_elimination mpm;
    Llvm_scalar_opts.add_licm mpm;
    Llvm_scalar_opts.add_reassociation mpm;
    Llvm_scalar_opts.add_gvn mpm;
    Llvm_scalar_opts.add_cfg_simplification mpm;

    ignore (Irgen.gen_main ast);
    Llvm_analysis.assert_valid_module Irgen.the_module;
    ignore (Llvm.PassManager.run_module Irgen.the_module mpm);

    if !emit_llvm then
      let ir = Llvm.string_of_llmodule Irgen.the_module in
      Std.output_file ~filename:(filename ^ ".ll") ~text:ir
    else ()
    ;

    Random.self_init ();
    let asm_tmp = Printf.sprintf "tmp%d.s" (Random.int 1000000000) in
    (* emit assembly *)
    Llvm_target.TargetMachine.emit_to_file Irgen.the_module Llvm_target.CodeGenFileType.AssemblyFile asm_tmp target_machine;
    (* generate executable using `clang` *)
    ignore (Sys.command @@ Printf.sprintf "clang %s -lgc -o %s" asm_tmp !executable_filename);
    (* remove assembly file *)
    ignore (Sys.command @@ Printf.sprintf "rm %s" asm_tmp)
  with Failure e ->
    Printf.eprintf "compile error: %s\n" e;
    exit 1

let main () =
  let argc = Array.length Sys.argv in
  let source = ref None in
  let rec process_arguments i =
    if i = argc then []
    else (match Sys.argv.(i) with
        | "--help" | "-h" ->
          print_string usage;
          ignore (exit 0);
          process_arguments @@ i + 1
        | "-o" ->
          Executable(Sys.argv.(i + 1))::(process_arguments @@ i + 2)
        | "-emit-llvm" ->
          EmitLLVM::(process_arguments @@ i + 1)
        | filename ->
          source := Some filename;
          process_arguments (i + 1)
      )
  in
  let options = process_arguments 1 in
  match !source with
  | Some filename -> compile filename options
  | None -> repl ()
;;

main ()
