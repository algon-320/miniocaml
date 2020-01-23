exception Error of string

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mini-ocaml"
let builder = Llvm.builder context
let env_depth : (string, int) Hashtbl.t = Hashtbl.create 10
let ptr = Llvm.pointer_type

let int_of_bool b = if b then 1 else 0

(* types *)
let void_t = Llvm.void_type context
let i8_t = Llvm.i8_type context
let int32_t = Llvm.i32_type context
let int64_t = Llvm.i64_type context
let bool_t = Llvm.i1_type context

let env_t =
  let t = Llvm.named_struct_type context "env_t" in
  Llvm.struct_set_body t [|ptr t; ptr i8_t|] false;
  t

let closure_t =
  let t = Llvm.named_struct_type context "closure_t" in
  Llvm.struct_set_body t [|ptr env_t; ptr i8_t|] false;
  t

let rec typeid_string = function
  | Type.TInt -> "int"
  | Type.TBool -> "bool"
  | Type.TList(ct) -> (typeid_string ct) ^ "_list"
  | Type.TUnit -> "unit"
  | Type.TArrow(t1, t2) -> Printf.sprintf "arrow_%s_to_%s" (typeid_string t1) (typeid_string t2)
  | Type.TVar(_) -> failwith "typeid_string: TVar unimplemented"
  | _ -> failwith "typeid_string: unimplemented"

(* content_tyを要素に持つリストの型 *)
let list_types : (Type.ty, Llvm.lltype) Hashtbl.t = Hashtbl.create 10
let rec get_list_type content_ty =
  try
    Hashtbl.find list_types content_ty
  with Not_found ->
    let t = Llvm.named_struct_type context ((typeid_string content_ty) ^ "_list_t") in
    (
      if content_ty = Type.TUnit then
        Llvm.struct_set_body t [|ptr t|] false
      else
        Llvm.struct_set_body t [|handling_lltype_of content_ty; ptr t|] false
    );
    Hashtbl.add list_types content_ty t;
    t

and lltype_of = function
  | Type.TInt -> int64_t
  | Type.TBool -> bool_t
  | Type.TUnit -> void_t
  | Type.TList(t) -> get_list_type t
  | Type.TArrow(_, _) -> closure_t
  | Type.TVar(name) -> failwith "lltype_of: TVar unimplemented"

and handling_lltype_of ty =
  if Type.is_atomic ty then
    lltype_of ty
  else
    ptr @@ lltype_of ty

let list_store_head list value content_ty =
  if content_ty = Type.TUnit then
    ()
  else
    let head_p = Llvm.build_struct_gep list 0 "list.head_p" builder in
    ignore (Llvm.build_store value head_p builder)
let list_load_head list content_ty =
  if content_ty = Type.TUnit then
    Llvm.undef void_t
  else
    let head_p = Llvm.build_struct_gep list 0 "list.head_p" builder in
    Llvm.build_load head_p "list.head" builder

let list_store_tail list value content_ty =
  let tail_p =
    if content_ty = Type.TUnit then
      Llvm.build_struct_gep list 0 "list.tail_p" builder
    else
      Llvm.build_struct_gep list 1 "list.tail_p" builder
  in
  ignore (Llvm.build_store value tail_p builder)
let list_load_tail list content_ty =
  let tail_p =
    if content_ty = Type.TUnit then
      Llvm.build_struct_gep list 0 "list.tail_p" builder
    else
      Llvm.build_struct_gep list 1 "list.tail_p" builder in
  Llvm.build_load tail_p "list.tail" builder

(* TODO: make it to simple while loop *)
let climbup_env_func =
  let fun_ty = Llvm.function_type (ptr env_t) [|ptr env_t; int64_t|] in
  let f = Llvm.declare_function "climbup_env" fun_ty the_module in
  let env = (Llvm.params f).(0) in
  let cnt = (Llvm.params f).(1) in
  let entry_bb = Llvm.append_block context "entry" f in
  let then_bb = Llvm.append_block context "then" f in
  let else_bb = Llvm.append_block context "else" f in

  Llvm.position_at_end entry_bb builder; (
    let cond = Llvm.build_icmp Llvm.Icmp.Eq cnt (Llvm.const_int int64_t 0) "if_zero" builder in
    ignore (Llvm.build_cond_br cond then_bb else_bb builder)
  );

  Llvm.position_at_end then_bb builder; (
    ignore (Llvm.build_ret env builder)
  );

  Llvm.position_at_end else_bb builder; (
    let parent_env_p = Llvm.build_struct_gep env 0 "env_parent_p" builder in
    let parent_env = Llvm.build_load parent_env_p "env_parent" builder in
    let pred_cnt = Llvm.build_sub cnt (Llvm.const_int int64_t 1) "pred_cnt" builder in
    let ret = Llvm.build_call f [|parent_env; pred_cnt|] "ret" builder in
    ignore (Llvm.build_ret ret builder)
  );

  f

let ast_type node = Hashtbl.find Tinf.type_info @@ Exp.take_id node

let global_constants : (Llvm.llvalue, Llvm.llvalue) Hashtbl.t = Hashtbl.create 256
let get_global_constant const =
  try
    Hashtbl.find global_constants const
  with Not_found ->
    let gv = Llvm.define_global "" const the_module in
    Hashtbl.add global_constants const gv;
    gv
let get_global_constant_string str =
  let v = get_global_constant @@ Llvm.const_stringz context str in
  Llvm.build_pointercast v (ptr i8_t) "" builder

let printf =
  let printf_type = Llvm.var_arg_function_type void_t [|(ptr i8_t)|] in
  Llvm.declare_function "printf" printf_type the_module

let printers : (Type.ty, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let rec get_printer t =
  try
    Hashtbl.find printers t
  with Not_found ->
    let prev_bb = Llvm.insertion_block builder in
    let f =
      match t with
      | Type.TUnit -> (
          let fun_ty = Llvm.function_type void_t [||] in
          let f = Llvm.declare_function "unit_printer" fun_ty the_module in
          let entry_bb = Llvm.append_block context "entry" f in
          Llvm.position_at_end entry_bb builder; (
            let p = get_global_constant_string "()" in
            ignore (Llvm.build_call printf [|p|] "" builder);
            ignore (Llvm.build_ret_void builder)
          );
          f
        )
      | Type.TInt -> (
          let fun_ty = Llvm.function_type void_t [|int64_t|] in
          let f = Llvm.declare_function "int_printer" fun_ty the_module in
          let v = (Llvm.params f).(0) in
          let entry_bb = Llvm.append_block context "entry" f in
          Llvm.position_at_end entry_bb builder; (
            let p = get_global_constant_string "%d" in
            ignore (Llvm.build_call printf [|p; v|] "" builder);
            ignore (Llvm.build_ret_void builder)
          );
          f
        )
      | Type.TBool -> (
          let fun_ty = Llvm.function_type void_t [|bool_t|] in
          let f = Llvm.declare_function "bool_printer" fun_ty the_module in
          let v = (Llvm.params f).(0) in
          let entry_bb = Llvm.append_block context "entry" f in
          let then_bb = Llvm.append_block context "then" f in
          let else_bb = Llvm.append_block context "else" f in
          let merge_bb = Llvm.append_block context "ifcont" f in

          Llvm.position_at_end entry_bb builder;
          ignore (Llvm.build_cond_br v then_bb else_bb builder);

          Llvm.position_at_end then_bb builder; (
            let p = get_global_constant_string "true" in
            ignore (Llvm.build_call printf [|p; v|] "" builder);
            ignore (Llvm.build_br merge_bb builder)
          );

          Llvm.position_at_end else_bb builder; (
            let p = get_global_constant_string "false" in
            ignore (Llvm.build_call printf [|p; v|] "" builder);
            ignore (Llvm.build_br merge_bb builder)
          );

          Llvm.position_at_end merge_bb builder;
          ignore (Llvm.build_ret_void builder);

          f
        )
      | Type.TList(ct) -> (
          let list_t = lltype_of t in
          let name = typeid_string t in
          let fun_ty = Llvm.function_type void_t [|ptr list_t|] in
          let inner =
            let f = Llvm.declare_function (name ^ "_inner_printer") fun_ty the_module in
            let v = (Llvm.params f).(0) in
            let entry_bb = Llvm.append_block context "entry" f in
            let ifnull_bb = Llvm.append_block context "ifnull" f in
            let else_bb = Llvm.append_block context "else" f in

            Llvm.position_at_end entry_bb builder; (
              let cond = Llvm.build_is_null v "is_null_cond" builder in
              ignore (Llvm.build_cond_br cond ifnull_bb else_bb builder)
            );

            Llvm.position_at_end ifnull_bb builder; (
              ignore (Llvm.build_ret_void builder)
            );

            Llvm.position_at_end else_bb builder; (
              let head = list_load_head v ct in
              let tail = list_load_tail v ct in
              let printer = get_printer ct in
              (
                if ct = Type.TUnit then
                  ignore (Llvm.build_call printer [||] "" builder)
                else
                  ignore (Llvm.build_call printer [|head|] "" builder)
              );
              let comma = get_global_constant_string "; " in
              ignore (Llvm.build_call printf [|comma|] "" builder);
              ignore (Llvm.build_call f [|tail|] "" builder);
              ignore (Llvm.build_ret_void builder)
            );
            f in
          let wrapper =
            let f = Llvm.declare_function (name ^ "_wrapper_printer") fun_ty the_module in
            let v = (Llvm.params f).(0) in
            let entry_bb = Llvm.append_block context "entry" f in
            Llvm.position_at_end entry_bb builder; (
              let lbracket = get_global_constant_string "[" in
              ignore (Llvm.build_call printf [|lbracket|] "" builder);
              ignore (Llvm.build_call inner [|v|] "" builder);
              let rbracket = get_global_constant_string "]" in
              ignore (Llvm.build_call printf [|rbracket|] "" builder);
              ignore (Llvm.build_ret_void builder);
            );
            f in
          wrapper
        )
      | _ -> failwith "get_printer: unimplemented"
    in
    Hashtbl.add printers t f;
    Llvm.position_at_end prev_bb builder;
    f

(* Boehm GC *)
let gcinit =
  let gcinit_type = Llvm.function_type void_t [||] in
  Llvm.declare_function "GC_init" gcinit_type the_module
let gcmalloc =
  let gcmalloc_type = Llvm.function_type (ptr i8_t) [|Llvm.i64_type context|] in
  Llvm.declare_function "GC_malloc" gcmalloc_type the_module
let build_gcmalloc size ret_type name builder =
  let p = Llvm.build_call gcmalloc [|size|] name builder in
  Llvm.build_pointercast p ret_type name builder
let build_gcmalloc_obj ty name builder =
  build_gcmalloc (Llvm.size_of ty) (ptr ty) name builder

(* store value to env *)
let env_store_value env value ty =
  let value =
    if ty = Type.TUnit then
      Llvm.const_pointer_null i8_t
    else if Type.is_atomic ty then
      let p = build_gcmalloc_obj (lltype_of ty) "" builder in
      ignore (Llvm.build_store value p builder);
      p
    else
      value
  in
  let value_p = Llvm.build_struct_gep env 1 "&env.value" builder in
  let addr = Llvm.build_pointercast value (ptr i8_t) "(i8*)" builder in
  Llvm.build_store addr value_p builder

(* load value from env *)
let env_load_value env ty =
  let value_p = Llvm.build_struct_gep env 1 "&env.value" builder in
  let value = Llvm.build_load value_p "(i8*)env.value" builder in
  let value = Llvm.build_pointercast value (ptr @@ lltype_of ty) "env.value" builder in
  if ty = Type.TUnit then
    Llvm.undef void_t
  else if Type.is_atomic ty then
    Llvm.build_load value "" builder
  else
    value

let build_env parent value =
  let new_env = build_gcmalloc_obj env_t "env" builder in
  let parent_p = Llvm.build_struct_gep new_env 0 "env.parent_p" builder in
  ignore (Llvm.build_store parent parent_p builder);
  (
    match value with
    | Some (v, ty) -> ignore (env_store_value new_env v ty)
    | None -> ()
  );
  new_env

let closure_count = ref 0

let rec gen_exp node env depth =
  match Exp.take_exp node with
  | Exp.IntLit(n) -> Llvm.const_int int64_t n
  | Exp.BoolLit(b) -> Llvm.const_int bool_t (int_of_bool b)
  | Exp.UnitLit -> Llvm.undef void_t
  | Exp.Add(e1, e2) ->
    Llvm.build_add (gen_exp e1 env depth) (gen_exp e2 env depth) "add" builder
  | Exp.Sub(e1, e2) ->
    Llvm.build_sub (gen_exp e1 env depth) (gen_exp e2 env depth) "sub" builder
  | Exp.Mul(e1, e2) ->
    Llvm.build_mul (gen_exp e1 env depth) (gen_exp e2 env depth) "mul" builder
  | Exp.Div(e1, e2) ->
    Llvm.build_sdiv (gen_exp e1 env depth) (gen_exp e2 env depth) "sdiv" builder
  | Exp.Eq(e1, e2) ->
    Llvm.build_icmp Llvm.Icmp.Eq (gen_exp e1 env depth) (gen_exp e2 env depth) "eq" builder
  | Exp.Ne(e1, e2) ->
    Llvm.build_icmp Llvm.Icmp.Ne (gen_exp e1 env depth) (gen_exp e2 env depth) "ne" builder
  | Exp.Gt(e1, e2) ->
    Llvm.build_icmp Llvm.Icmp.Sgt (gen_exp e1 env depth) (gen_exp e2 env depth) "gt" builder
  | Exp.Lt(e1, e2) ->
    Llvm.build_icmp Llvm.Icmp.Slt (gen_exp e1 env depth) (gen_exp e2 env depth) "lt" builder
  | Exp.If(cond, e1, e2) ->
    let cond = gen_exp cond env depth in
    let start_bb = Llvm.insertion_block builder in
    let the_function = Llvm.block_parent start_bb in
    let then_bb = Llvm.append_block context "then" the_function in
    let else_bb = Llvm.append_block context "else" the_function in
    let merge_bb = Llvm.append_block context "ifcont" the_function in

    Llvm.position_at_end start_bb builder;
    ignore (Llvm.build_cond_br cond then_bb else_bb builder);

    Llvm.position_at_end then_bb builder;
    let then_val = gen_exp e1 env depth in
    let final_then_bb = Llvm.insertion_block builder in
    ignore (Llvm.build_br merge_bb builder);

    Llvm.position_at_end else_bb builder;
    let else_val = gen_exp e2 env depth in
    let final_else_bb = Llvm.insertion_block builder in
    ignore (Llvm.build_br merge_bb builder);

    Llvm.position_at_end merge_bb builder;
    (
      match ast_type node with
      | Type.TUnit ->
        Llvm.undef void_t
      | _ ->
        let incoming = [(then_val, final_then_bb); (else_val, final_else_bb)] in
        Llvm.build_phi incoming "iftmp" builder
    )

  | Exp.ListEmpty -> Llvm.const_null @@ ptr @@ lltype_of @@ ast_type node
  | Exp.ListCons(head, tail) ->
    let list_t = lltype_of @@ ast_type node in
    let head_v = gen_exp head env depth in
    let tail_v = gen_exp tail env depth in
    let cons_p = build_gcmalloc_obj list_t "cons" builder in
    list_store_head cons_p head_v (ast_type head);
    list_store_tail cons_p tail_v (ast_type head);
    cons_p
  | Exp.ListHead(lst) ->
    let cons_p = gen_exp lst env depth in
    list_load_head cons_p @@ ast_type node
  | Exp.ListTail(lst) ->
    let cons_p = gen_exp lst env depth in
    let content_ty = match ast_type node with
      | Type.TList(ct) -> ct
      | _ -> failwith "type error"
    in
    list_load_tail cons_p @@ content_ty

  | Exp.Skip(e1, e2) -> ignore (gen_exp e1 env depth); gen_exp e2 env depth
  | Exp.Print(e1) ->
    let printer = get_printer @@ ast_type e1 in
    let value = gen_exp e1 env depth in
    (
      if ast_type e1 = Type.TUnit then
        ignore (Llvm.build_call printer [||] "" builder)
      else
        ignore (Llvm.build_call printer [|value|] "" builder)
    );
    let newline = get_global_constant_string "\n" in
    ignore (Llvm.build_call printf [|newline|] "" builder);
    Llvm.undef void_t

  | Exp.Var(name) -> (
      try
        let d = Hashtbl.find env_depth name in
        let args = [|env; Llvm.const_int int64_t (depth - d)|] in
        let target_env = Llvm.build_call climbup_env_func args ("&" ^ name ^"_target_env") builder in
        env_load_value target_env @@ ast_type node
      with Not_found ->
        failwith @@ Printf.sprintf "unbound value: %s" name
    )
  | Exp.Let(name, e1, e2) ->
    let v1 = gen_exp e1 env depth in
    let new_env = build_env env @@ Some (v1, ast_type e1) in

    Hashtbl.add env_depth name (depth + 1);
    let v2 = gen_exp e2 new_env (depth + 1) in
    Hashtbl.remove env_depth name;
    v2

  | Exp.Fun(arg, body) ->
    let closure = build_gcmalloc_obj closure_t "closure" builder in
    let closure_env = Llvm.build_struct_gep closure 0 "closure.env" builder in
    ignore (Llvm.build_store env closure_env builder);

    let current_bb = Llvm.insertion_block builder in
    Hashtbl.add env_depth arg (depth + 1);

    let f =
      let fun_ty =
        let ret_ty = handling_lltype_of @@ ast_type body in
        Llvm.function_type ret_ty [|ptr env_t|] in
      let fun_name = "closure_" ^ (string_of_int !closure_count) in
      closure_count := !closure_count + 1;
      Llvm.declare_function fun_name fun_ty the_module in

    let entry_bb = Llvm.append_block context "entry" f in
    let fun_env = (Llvm.params f).(0) in

    Llvm.position_at_end entry_bb builder; (
      let ret = gen_exp body fun_env (depth + 1) in
      if Llvm.type_of ret = void_t then
        ignore (Llvm.build_ret_void builder)
      else
        ignore (Llvm.build_ret ret builder)
    );

    Llvm.position_at_end current_bb builder;
    Hashtbl.remove env_depth arg;

    let fun_ptr = Llvm.build_struct_gep closure 1 "closure.fun_ptr" builder in
    let fun_addr = Llvm.build_pointercast f (ptr i8_t) "closure.fun_addr"builder in
    ignore (Llvm.build_store fun_addr fun_ptr builder);
    closure

  | Exp.LetRec(funname, argname, body, e) -> (
      let fun_env = build_env env None in
      Hashtbl.add env_depth funname (depth + 1);

      (
        let closure = build_gcmalloc_obj closure_t "closure" builder in
        let closure_env = Llvm.build_struct_gep closure 0 "closure.env" builder in
        ignore (Llvm.build_store fun_env closure_env builder);

        let current_bb = Llvm.insertion_block builder in
        Hashtbl.add env_depth argname (depth + 2);

        let f =
          let fun_ty =
            let ret_ty = handling_lltype_of @@ ast_type body in
            Llvm.function_type ret_ty [|ptr env_t|] in
          let fun_name = "closure_" ^ (string_of_int !closure_count) in
          closure_count := !closure_count + 1;
          Llvm.declare_function fun_name fun_ty the_module in

        let fun_ptr = Llvm.build_struct_gep closure 1 "closure.fun_ptr" builder in
        let fun_addr = Llvm.build_pointercast f (ptr i8_t) "closure.fun_addr"builder in
        ignore (Llvm.build_store fun_addr fun_ptr builder);

        let entry_bb = Llvm.append_block context "entry" f in
        let arg_env = (Llvm.params f).(0) in

        Llvm.position_at_end entry_bb builder; (
          let ret = gen_exp body arg_env (depth + 2) in
          if Llvm.type_of ret = void_t then
            ignore (Llvm.build_ret_void builder)
          else
            ignore (Llvm.build_ret ret builder)
        );

        Llvm.position_at_end current_bb builder;
        Hashtbl.remove env_depth argname;

        ignore (env_store_value fun_env closure @@ Type.TArrow(Type.TUnit, Type.TUnit));
      );

      let v = gen_exp e fun_env (depth + 1) in
      Hashtbl.remove env_depth funname;
      v
    )

  | Exp.App(f, e) ->
    let arg = gen_exp e env depth in
    let closure = gen_exp f env depth in
    let clos_env_p = Llvm.build_struct_gep closure 0 "&closure.env" builder in
    let clos_env = Llvm.build_load clos_env_p "closure.env" builder in

    let new_env = build_env clos_env (Some (arg, ast_type e)) in 
    let clos_fun_p = Llvm.build_struct_gep closure 1 "closure.fun_p" builder in
    let clos_fun_addr = Llvm.build_load clos_fun_p "closure.fun_addr" builder in
    let fun_type = match ast_type f with
      | TArrow(_, t) -> Llvm.function_type (handling_lltype_of t) [|ptr env_t|]
      | _ -> failwith "type error" in
    let clos_fun = Llvm.build_pointercast clos_fun_addr (ptr fun_type) "closure.fun" builder in
    Llvm.build_call clos_fun [|new_env|] "" builder

  | _ -> failwith "gen_exp: unimplemented"

let gen_main e =
  let fun_ty = Llvm.function_type int32_t [||] in
  let the_function = match Llvm.lookup_function "main" the_module with
    | None -> Llvm.declare_function "main" fun_ty the_module
    | Some _ -> failwith "the function has already been decleared"
  in
  let entry_bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end entry_bb builder;
  try
    ignore (Llvm.build_call gcinit [||] "" builder);
    let v = gen_exp e (Llvm.const_null (ptr env_t)) 0 in
    if Llvm.type_of v = int64_t then
      let ret = Llvm.build_trunc v int32_t "tmp" builder in
      ignore (Llvm.build_ret ret builder)
    else
      ignore (Llvm.build_ret (Llvm.const_int int32_t 0) builder)
    ;
    Llvm_analysis.assert_valid_function the_function;
    the_function
  with e ->
    Llvm.delete_function the_function;
    raise e
