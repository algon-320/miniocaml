let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

let eval str =
  Eval.eval (parse str) (Eval.emptyenv ()) Eval.ident_cont

let tinf str =
  Tinf.pretty_format_type @@ Tinf.rename_typevar @@ Tinf.get_type @@ parse str

let fpm =
  let fpm = Llvm.PassManager.create_function Irgen.the_module in
  (* 
  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  Llvm_scalar_opts.add_instruction_combination fpm;
  (* reassociate expressions. *)
  Llvm_scalar_opts.add_reassociation fpm;
  (* Eliminate Common SubExpressions. *)
  Llvm_scalar_opts.add_gvn fpm;
  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  Llvm_scalar_opts.add_cfg_simplification fpm;
  *)
  ignore (Llvm.PassManager.initialize fpm);
  fpm

let irgen str =
  let ast = parse str in
  let _ = Tinf.tinf [] ast 0 in
  Llvm.dump_value (Irgen.gen_toplevel ast fpm);
  print_newline ()
let dump_module () =
  Llvm.dump_module Irgen.the_module;
  print_newline ()

let exec_jit str =
  let ast = parse str in
  let _ = Tinf.tinf [] ast 0 in
  if not (Llvm_executionengine.initialize ()) then
    failwith "unsupported target"
  else
    (
      ignore (Irgen.gen_toplevel ast fpm);
      let ee = Llvm_executionengine.create Irgen.the_module in
      let func_name = "_toplevel_" ^ (string_of_int (!Irgen.toplevel_count - 1)) in
      let fp = Llvm_executionengine.get_function_address func_name (Foreign.funptr Ctypes.(void @-> returning void)) ee in
      fp ();
    )