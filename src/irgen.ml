exception Error of string

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mini-ocaml"
let builder = Llvm.builder context
let named_values:(string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

let int_of_bool b = if b then 1 else 0

(* types *)
let void_t = Llvm.void_type context
let i8_ptr_t = Llvm.pointer_type (Llvm.i8_type context)
let int_t = Llvm.i64_type context
let byte_t = Llvm.i8_type context
let byte_ptr_t = Llvm.pointer_type byte_t
let bool_t = Llvm.i1_type context

(* lltypeを要素に持つリストの型 *)
let list_types:(Llvm.lltype, Llvm.lltype) Hashtbl.t = Hashtbl.create 10
let get_list_type lltype =
  try
    Hashtbl.find list_types lltype
  with Not_found ->
    let t = Llvm.named_struct_type context ((Llvm.string_of_lltype lltype) ^ "_list_t") in
    let () = Llvm.struct_set_body t [|lltype; Llvm.pointer_type t|] false in
    Hashtbl.add list_types lltype t;
    t

(* Type -> lltyp *)
let rec type_to_lltype = function
  | Type.TInt -> int_t
  | Type.TBool -> bool_t
  | Type.TUnit -> void_t
  | Type.TList(t) ->
    (
      let tt = type_to_lltype t in
      match t with
      | Type.TInt | Type.TBool -> get_list_type tt
      | Type.TList _ -> get_list_type (Llvm.pointer_type tt)
      | _ -> failwith "unimplemented"
    )
  | Type.TArrow(t1, t2) -> failwith "unimplemented"
  | Type.TVar(name) -> failwith "unimplemented"
let get_type e = type_to_lltype (Tinf.get_type e)

let rec gen_exp e = match e with
  | Exp.IntLit(n) -> Llvm.const_int int_t n
  | Exp.BoolLit(b) -> Llvm.const_int bool_t (int_of_bool b)
  | Exp.UnitLit -> Llvm.const_null int_t
  | Exp.Add(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_add lhs rhs "add" builder
  | Exp.Sub(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_sub lhs rhs "sub" builder
  | Exp.Mul(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_mul lhs rhs "mul" builder
  | Exp.Div(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_sdiv lhs rhs "sdiv" builder
  | Exp.Eq(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "eq" builder
  | Exp.Ne(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_icmp Llvm.Icmp.Ne lhs rhs "ne" builder
  | Exp.Gt(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "gt" builder
  | Exp.Lt(e1, e2) ->
    let (lhs, rhs) = (gen_exp e1, gen_exp e2) in
    Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "lt" builder
  | Exp.If(cond, e1, e2) ->
    let cond = gen_exp cond in

    let start_bb = Llvm.insertion_block builder in
    let the_function = Llvm.block_parent start_bb in

    let then_bb = Llvm.append_block context "then" the_function in
    Llvm.position_at_end then_bb builder;
    let then_val = gen_exp e1 in
    let final_then_bb = Llvm.insertion_block builder in

    let else_bb = Llvm.append_block context "else" the_function in
    Llvm.position_at_end else_bb builder;
    let else_val = gen_exp e2 in
    let final_else_bb = Llvm.insertion_block builder in

    let merge_bb = Llvm.append_block context "ifcont" the_function in
    Llvm.position_at_end merge_bb builder;
    let incoming = [(then_val, final_then_bb); (else_val, final_else_bb)] in
    let phi = Llvm.build_phi incoming "iftmp" builder in

    Llvm.position_at_end start_bb builder;
    ignore (Llvm.build_cond_br cond then_bb else_bb builder);

    (* jump to merge bb *)
    Llvm.position_at_end final_then_bb builder;
    ignore (Llvm.build_br merge_bb builder);
    Llvm.position_at_end final_else_bb builder;
    ignore (Llvm.build_br merge_bb builder);

    Llvm.position_at_end merge_bb builder;

    phi

  | Exp.ListEmpty -> Llvm.const_null byte_ptr_t
  | Exp.ListCons(head, tail) ->
    let list_t = get_type e in
    let head_v = gen_exp head in
    let tail_v =
      let p = gen_exp tail in
      match tail with
      | Exp.ListEmpty -> Llvm.build_pointercast p (Llvm.pointer_type list_t) "tail_v" builder
      | _ -> p in
    let cons_p = Llvm.build_alloca list_t "cons_p" builder in
    let head_p = Llvm.build_struct_gep cons_p 0 "cons_head_p" builder in
    let tail_p = Llvm.build_struct_gep cons_p 1 "cons_tail_p" builder in
    ignore (Llvm.build_store head_v head_p builder);
    ignore (Llvm.build_store tail_v tail_p builder);
    cons_p
  | Exp.ListHead(lst) ->
    let cons_p = gen_exp lst in
    let head_p = Llvm.build_struct_gep cons_p 0 "head_head_p" builder in
    Llvm.build_load head_p "head_v" builder
  | Exp.ListTail(lst) ->
    let cons_p = gen_exp lst in
    let tail_p = Llvm.build_struct_gep cons_p 1 "tail_tail_p" builder in
    Llvm.build_load tail_p "tail_v" builder

  | Exp.Skip(e1, e2) -> ignore (gen_exp e1); gen_exp e2
  | Exp.Print(e) ->
    failwith "Print: unimplemented"

  | _ -> failwith "gen_exp: unimplemented"

let gen_function fun_name args ret_type body fpm =
  let args_ty = Array.map (fun (_, ty) -> ty) args in
  let fun_ty = Llvm.function_type ret_type args_ty in
  let the_function = match Llvm.lookup_function fun_name the_module with
    | None -> Llvm.declare_function fun_name fun_ty the_module
    | Some _ -> raise (Error "the function has already been decleared")
  in
  Array.iteri (fun i arg ->
      let (arg_name, _) = args.(i) in
      Llvm.set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg
    ) (Llvm.params the_function);

  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;

  try
    let ret_val = gen_exp body in
    if ret_type = void_t then
      ignore (Llvm.build_ret_void builder)
    else
      ignore (Llvm.build_ret ret_val builder);
    Llvm_analysis.assert_valid_function the_function;
    ignore (Llvm.PassManager.run_function the_function fpm);
    the_function
  with e ->
    Llvm.delete_function the_function;
    raise e

let toplevel_count = ref 0
let gen_toplevel e fpm =
  let name = "_toplevel_" ^ (string_of_int !toplevel_count) in
  let code = gen_function name [||] void_t (Exp.Skip(e, Exp.UnitLit)) fpm in
  toplevel_count := !toplevel_count + 1;
  code
