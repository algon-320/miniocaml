type zam_instr =
  | ZAM_Ldi of int
  | ZAM_Ldb of bool
  | ZAM_Access of int
  | ZAM_Closure of zam_code
  | ZAM_Apply
  | ZAM_TailApply
  | ZAM_PushMark
  | ZAM_Grab
  | ZAM_Return
  | ZAM_Let
  | ZAM_EndLet
  | ZAM_Test of zam_code * zam_code
  | ZAM_Add
  | ZAM_Sub
  | ZAM_Mul
  | ZAM_Div
  | ZAM_Eq
  | ZAM_Ne
  | ZAM_Gt
  | ZAM_Lt
and zam_code = zam_instr list

type zam_value =
  | ZAM_IntVal  of int
  | ZAM_BoolVal of bool
  | ZAM_ClosVal of zam_code * zam_env
  | ZAM_Epsilon
and zam_stack = zam_value list
and zam_env = zam_value list

let string_of_zam_value = function
  | ZAM_IntVal(n) -> string_of_int n
  | ZAM_BoolVal(b) -> string_of_bool b
  | ZAM_ClosVal _ -> "(ZAM closure)"
  | ZAM_Epsilon -> "(ZAM Epsilon)"

let rec exec code env arg_stack ret_stack =
  match code with
  | [] ->
    (
      match (arg_stack, ret_stack) with
      | (v::_, []) -> v
      | ([], _) -> failwith "exec: argument stack is empty"
      | (_, _) -> failwith "exec: return stack is not empty"
    )
  | c::next ->
    match c with
    | ZAM_Ldi(n) ->
      exec next env (ZAM_IntVal(n)::arg_stack) ret_stack
    | ZAM_Ldb(b) ->
      exec next env (ZAM_BoolVal(b)::arg_stack) ret_stack
    | ZAM_Access(i) ->
      let v = List.nth env i in
      exec next env (v::arg_stack) ret_stack
    | ZAM_Closure(cc) ->
      exec next env (ZAM_ClosVal(cc, env)::arg_stack) ret_stack
    | ZAM_Apply ->
      (
        match arg_stack with
        | ZAM_ClosVal(cc, cenv)::v::s ->
          exec cc (v::ZAM_ClosVal(cc, cenv)::cenv) s (ZAM_ClosVal(next, env)::ret_stack)
        | _ -> failwith "ZAM_Apply: closure and argument is required"
      )
    | ZAM_TailApply ->
      (
        match arg_stack with
        | ZAM_ClosVal(cc, cenv)::v::s ->
          exec cc (v::ZAM_ClosVal(cc, cenv)::cenv) s ret_stack
        | _ -> failwith "ZAM_TailApply: closure and argument is required"
      )
    | ZAM_PushMark ->
      exec next env (ZAM_Epsilon::arg_stack) ret_stack
    | ZAM_Grab ->
      (
        match (arg_stack, ret_stack) with
        | (ZAM_Epsilon::s, ZAM_ClosVal(cc, cenv)::r) ->
          exec cc cenv (ZAM_ClosVal(next, env)::s) r
        | (v::s, r) ->
          exec next (v::ZAM_ClosVal(next, env)::env) s r
        | _ -> failwith "ZAM_Grab: condition of stacks is invalid"
      )
    | ZAM_Return ->
      (
        match (arg_stack, ret_stack) with
        | (v::ZAM_Epsilon::s, ZAM_ClosVal(cc, cenv)::r) ->
          exec cc cenv (v::s) r
        | (ZAM_ClosVal(cc, cenv)::v::s, r) ->
          exec cc (v::ZAM_ClosVal(cc, cenv)::cenv) s r
        | _ -> failwith "ZAM_Return: condition of stacks is invalid"
      )
    | ZAM_Let ->
      (
        match arg_stack with
        | v::s ->
          exec next (v::env) s ret_stack
        | _ -> failwith "ZAM_Let: arg_stack is empty"
      )
    | ZAM_EndLet ->
      (
        match env with
        | _::e ->
          exec next e arg_stack ret_stack
        | _ -> failwith "ZAM_EndLet: env is empty"
      )
    | ZAM_Test(c1, c2) ->
      (
        match arg_stack with
        | ZAM_BoolVal(true)::s ->
          exec (c1 @ next) env s ret_stack
        | ZAM_BoolVal(false)::s ->
          exec (c2 @ next) env s ret_stack
        | _ -> failwith "ZAM_Test: bool value is required"
      )
    | ZAM_Add -> bin_op (bin_op_int (+)) "ZAM_Add" next env arg_stack ret_stack
    | ZAM_Sub -> bin_op (bin_op_int (-)) "ZAM_Sub" next env arg_stack ret_stack
    | ZAM_Mul -> bin_op (bin_op_int ( * )) "ZAM_Mul" next env arg_stack ret_stack
    | ZAM_Div -> bin_op (bin_op_int (/)) "ZAM_Div" next env arg_stack ret_stack
    | ZAM_Ne -> bin_op (bin_op_bool (<>)) "ZAM_Ne" next env arg_stack ret_stack
    | ZAM_Eq -> bin_op (bin_op_bool (=)) "ZAM_Eq" next env arg_stack ret_stack
    | ZAM_Gt -> bin_op (bin_op_bool (>)) "ZAM_Gt" next env arg_stack ret_stack
    | ZAM_Lt -> bin_op (bin_op_bool (<)) "ZAM_Lt" next env arg_stack ret_stack

and bin_op f op_name next env arg_stack ret_stack =
  match arg_stack with
  | ZAM_IntVal(n1)::ZAM_IntVal(n2)::s ->
    exec next env ((f n1 n2)::s) ret_stack
  | _ -> failwith (op_name ^ ": two ints are required")
and bin_op_int f =
  fun x -> fun y -> ZAM_IntVal (f x y)
and bin_op_bool f =
  fun x -> fun y -> ZAM_BoolVal (f x y)
