open Exp
open Type

let ext env x v =
  (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v)::tl ->
    if x = y then v 
    else lookup x tl 

let rec occurs tx t =
  if tx = t then true 
  else 
    match t with
    | TArrow(t1,t2) -> (occurs tx t1) || (occurs tx t2)
    | _ -> false

let rec subst_ty theta t =
  let rec subst_ty1 theta1 s = 
    match theta1 with
    |	[] -> TVar(s)
    | (tx,t1):: theta2 -> 
      if tx = s then t1 
      else subst_ty1 theta2 s
  in match t with
  | TInt -> TInt
  | TBool -> TBool
  | TUnit -> TUnit
  | TArrow(t2, t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
  | TVar(s) -> subst_ty1 theta s
  | TList(t) -> TList(subst_ty theta t)

let subst_tyenv theta te =
  List.map (fun (x, t) -> (x, subst_ty theta t)) te

let subst_eql theta eql =
  List.map (fun (t1, t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

let compose_subst theta2 theta1 =
  let theta11 = 
    List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1
  in
  List.fold_left (fun tau -> fun (tx,t) -> 
      try 
        let _ = lookup tx theta1 in
        tau
      with Failure(_) ->
        (tx,t) :: tau)
    theta11
    theta2

let rec remove tenv name =
  match tenv with
  | [] -> []
  | (tn, ty)::t ->
    if tn = name then t
    else (tn, ty)::(remove t name)

let unify eql =
  let rec solve eql theta =
    match eql with
    | [] -> theta
    | (t1,t2):: eql2 ->
      if t1 = t2 then solve eql2 theta
      else 
        begin
          match (t1,t2) with
          | (TArrow(t11,t12),TArrow(t21,t22))
            -> solve ((t11,t21)::(t12,t22)::eql2) theta
          | (TList(t1),TList(t2))
            -> solve ((t1, t2)::eql2) theta
          | (TVar(s), _)
            -> if (occurs t1 t2) then failwith "unification failed"
            else solve (subst_eql [(s,t2)] eql2)
                (compose_subst [(s,t2)] theta)
          | (_,TVar(s))
            -> if (occurs t2 t1) then failwith "unification failed"
            else solve (subst_eql [(s,t1)] eql2)
                (compose_subst [(s,t1)] theta)
          | (_,_) -> failwith "unification failed"
        end
  in solve eql []

let theta0 = ([] : tysubst)

let new_typevar n = 
  (TVar ("'a" ^ (string_of_int n)), n+1)

(* ASTのノード→型 *)
let type_info : Type.ty Exp.ExpHash.t = Exp.ExpHash.create 256

let update_type_info theta =
  Exp.ExpHash.iter (fun k v ->
      let final_ty = subst_ty theta v in
      Exp.ExpHash.replace type_info k final_ty
    ) type_info

(* tinf : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec tinf te e n =
  let result = (
    match e with
    | Var(s) ->
      (
        try
          let t1 = lookup s te in (te, t1, theta0, n)
        with Failure(_) ->
          let (tx, n1) = new_typevar n in
          let te1 = ext te s tx in
          (te1, tx, theta0, n1)
      )
    | IntLit(_)   -> (te, TInt, theta0, n)
    | BoolLit(_)  -> (te, TBool, theta0, n)
    | UnitLit     -> (te, TUnit, theta0, n)

    | Add(e1,e2)
    | Sub(e1,e2)
    | Mul(e1,e2)
    | Div(e1,e2) ->
      let (te1, (t1, t2), theta1, n1) = tinf_inorder2 te (e1, e2) n in
      let theta2 = unify [(t1, TInt); (t2, TInt)] in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, TInt, theta3, n1)

    | If(e1,e2,e3) ->
      let (te1, (t1, t2, t3), theta1, n1) = tinf_inorder3 te (e1, e2, e3) n in
      let theta2 = unify [(t1, TBool); (t2, t3)] in
      let t2 = subst_ty theta2 t2 in
      let te4 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te4, t2, theta3, n1)

    | Eq(e1, e2)
    | Ne(e1, e2)
    | Gt(e1, e2)
    | Lt(e1, e2) ->
      let (te1, (t1, t2), theta1, n1) = tinf_inorder2 te (e1, e2) n in
      let theta2 = unify [(t1, t2)] in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, TBool, theta3, n1)

    | Fun(x,e) ->
      let (tx, n1) = new_typevar n in
      let te1 = ext te x tx in
      let (te2, t2, theta1, n2) = tinf te1 e n1 in
      let t1 = subst_ty theta1 tx in
      let te3 = remove te2 x in
      (te3, TArrow(t1, t2), theta1, n2)
    | App(e1,e2) ->
      let (te1, (t1, t2), theta1, n1) = tinf_inorder2 te (e1, e2) n in
      let (tx,n2) = new_typevar n1 in
      let theta2 = unify [(t1, TArrow(t2, tx))] in
      let t3 = subst_ty theta2 tx in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, t3, theta3, n2)
    | Let(x, e1, e2) ->
      tinf te (App(Fun(x, e2), e1)) n
    | LetRec(f, x, e1, e2) ->
      let (tx, n) = new_typevar n in
      let (tf, n) = new_typevar n in
      let te = ext (ext te x tx) f tf in
      let (te, t1, theta1, n) = tinf te e1 n in
      let tx2 = subst_ty theta1 tx in
      let theta2 = unify [(tf, TArrow(tx2, t1))] in
      let te = subst_tyenv theta2 te in
      let (te, t2, theta3, n) = tinf te e2 n in
      let te = remove (remove te f) x in
      let theta123 = compose_subst theta3 @@ compose_subst theta2 theta1 in
      (te, t2, theta123, n)

    | ListEmpty ->
      let (tx,n1) = new_typevar n in
      (te, TList(tx), theta0, n1)
    | ListCons(h, t) ->
      let (te1, (t1, t2), theta1, n1) = tinf_inorder2 te (h, t) n in
      let theta2 = unify [(TList(t1), t2)] in
      let t2 = subst_ty theta2 t2 in
      let te3 = subst_tyenv theta2 te1 in
      let theta4 = compose_subst theta2 theta1 in
      (te3, t2, theta4, n1)
    | ListHead(e1) ->
      let (te1, t1, theta1, n1) = tinf te e1 n in
      let (tx, n2) = new_typevar n1 in
      let theta2 = unify [(t1, TList(tx))] in
      let t2 = subst_ty theta2 tx in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, t2, theta3, n2)
    | ListTail(e1) ->
      tinf te e1 n

    | Match(e1, arms) ->
      (
        let (te, t1, theta1, n) = tinf te e1 n in
        match arms with
        | [] -> failwith "Match: at least one matching arm required."
        | _ ->
          let rec tinf_arms te arms n =
            match arms with
            | [] ->
              let (tx1, n) = new_typevar n in
              let (tx2, n) = new_typevar n in
              te, (tx1, tx2), theta0, n
            | (p, e)::t ->
              let (te, (t_p, t_e), theta2, n) = tinf_pat_arm te (p, e) n in
              let (te, (t_pt, t_et), theta3, n) = tinf_arms te t n in
              let t_p = subst_ty theta3 t_p in
              let t_e = subst_ty theta3 t_e in
              let theta4 = unify [(t_p, t_pt); (t_e, t_et)] in
              let t_p = subst_ty theta4 t_p in
              let t_e = subst_ty theta4 t_e in
              let te = subst_tyenv theta4 te in
              let theta5 = compose_subst (compose_subst theta4 theta3) (compose_subst theta2 theta1) in
              (te, (t_p, t_e), theta5, n)
          in
          let (te, (t_pat, t_res), theta2, n) = tinf_arms te arms n in
          let t1 = subst_ty theta2 t1 in
          let theta3 = unify [(t1, t_pat)] in
          let te = subst_tyenv theta3 te in
          let t_res = subst_ty theta3 t_res in
          let theta4 = compose_subst theta3 @@ compose_subst theta2 theta1 in
          (te, t_res, theta4, n)
      )

    | Skip(e1, e2) ->
      let (te1, (_, t2), theta1, n1) = tinf_inorder2 te (e1, e2) n in
      (te1, t2, theta1, n1)

    | Print(e) ->
      let (te, _, theta1, n) = tinf te e n in
      (te, TUnit, theta1, n)

    | CallCC(c) ->
      (* c: ('a -> 'a) -> 'a,  CallCC(c): 'a *)
      let (te1, t1, theta1, n1) = tinf te c n in
      let (tx, n2) = new_typevar n1 in
      let theta2 = unify [(t1, TArrow(TArrow(tx, tx), tx))] in
      let t2 = subst_ty theta2 tx in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, t2, theta3, n2)

    | OpAdd | OpSub | OpMul | OpDiv ->
      (te, TArrow(TInt, TArrow(TInt, TInt)), theta0, n)
    | OpEq | OpNe | OpGt | OpLt ->
      (te, TArrow(TInt, TArrow(TInt, TBool)), theta0, n)
  ) in
  let (_, t, th, _) = result in Exp.ExpHash.add type_info e (subst_ty th t);
  result

and tinf_pat_arm te (pat, e) n =
  match pat with
  | LiteralPat(lit) ->
    tinf_inorder2 te (lit, e) n
  | WildcardPat(s) ->
    let (tx, n1) = new_typevar n in
    let te1 = ext te s tx in
    let (te2, t2, theta2, n2) = tinf te1 e n1 in
    let t1 = subst_ty theta2 tx in
    let te3 = remove te2 s in
    (te3, (t1, t2), theta2, n2)
  | ListPat(h, t) ->
    let (te1, (t1, t2), theta1, n1) = tinf_pat_arm te (h, e) n in
    let (te2, (t3, t4), theta2, n2) = tinf_pat_arm te1 (t, e) n1 in
    let (t1, t2) = (subst_ty theta2 t1, subst_ty theta2 t2) in
    let theta3 = unify [(TList(t1), t3); (t2, t4)] in
    let (t1, t2) = (subst_ty theta3 t1, subst_ty theta3 t2) in
    let te3 = subst_tyenv theta3 te2 in
    let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
    (te3, (TList(t1), t2), theta4, n2)

and tinf_inorder2 te (e1, e2) n =
  let (te, t1, theta1, n) = tinf te e1 n in
  let (te, t2, theta2, n) = tinf te e2 n in
  let t1 = subst_ty theta2 t1 in
  let t2 = subst_ty theta1 t2 in
  let theta3 = compose_subst theta2 theta1 in
  (te, (t1, t2), theta3, n)
and tinf_inorder3 te (e1, e2, e3) n =
  let (te, t1, theta1, n) = tinf te e1 n in
  let (te, t2, theta2, n) = tinf te e2 n in
  let (te, t3, theta3, n) = tinf te e3 n in
  let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
  let (t1, t2) = (subst_ty theta4 t1, subst_ty theta4 t2) in
  (te, (t1, t2, t3), theta4, n)


(* 型変数を 'a -> 'b -> ... のように振り直す *)
let rename_typevar ty =
  let rec impl te ty n =
    let new_typevar_alpha n = 
      let (alpha, num) = (char_of_int (n mod 26 + (int_of_char 'a')), n / 26) in
      if num > 0 then
        (TVar (Printf.sprintf "'%c%d" alpha num), n+1)
      else
        (TVar (Printf.sprintf "'%c" alpha), n+1)
    in
    match ty with
    | TInt | TBool | TUnit -> (te, ty, n)
    | TVar(s) ->
      (
        try
          let t1 = lookup s te in
          (te, t1, n)
        with Failure(_) ->
          let (tx, n1) = new_typevar_alpha n in
          let te1 = ext te s tx in
          (te1, tx, n1)
      )
    | TArrow(t1, t2) ->
      let (te1, t1, n1) = impl te t1 n in
      let (te2, t2, n2) = impl te1 t2 n1 in
      (te2, TArrow(t1, t2), n2)
    | TList(t1) -> 
      let (te1, t1, n1) = impl te t1 n in
      (te1, TList(t1), n1)
  in
  let (_, t, _) = impl [] ty 0 in
  t

let get_type e =
  let (_, t, _, _) = tinf [] e 0 in
  t
