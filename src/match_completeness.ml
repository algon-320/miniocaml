open Exp

module PatSet = Set.Make (
  struct
    type t = Exp.pattern
    let compare = compare
  end)

type range =
  | Term of PatSet.t
  | Rec of PatSet.t * fuga list
and fuga =
  | Ok
  | Working of range


let get_universe ty =
  let open Exp in
  match ty with
  | Type.TInt
  | Type.TArrow _ -> PatSet.of_list [WildcardPat ""], []
  | Type.TUnit -> PatSet.of_list [LiteralPat(Unit)], []
  | Type.TBool -> PatSet.of_list [LiteralPat(CBool true); LiteralPat(CBool false)], []
  | Type.TList ct -> PatSet.of_list [LiteralPat(ListEmpty)], [ct; ty]
  | Type.TVar _ -> failwith "not concrete type"

let all_ok l len =
  (List.fold_left (fun a x -> (if x = Ok then 1 else 0) + a) 0 l) = len

let rec map3 f l1 l2 l3 =
  match l1, l2, l3 with
  | [], [], [] -> []
  | x::xs, y::ys, z::zs -> (f x y z)::(map3 f xs ys zs)
  | _ -> failwith "list length is not same"

let rec collect ty pats =
  let rec wrap ty pat =
    match pat with
    | LiteralPat _ ->
      let s = PatSet.singleton pat in
      let u, lst = get_universe ty in
      if PatSet.subset u s && List.compare_length_with lst 0 = 0 then Ok
      else Working (Term s)
    | WildcardPat _ -> Ok
    | ListPat(head, tail) ->
      let ct = match ty with TList ct -> ct | _ -> failwith "type error" in
      Working (Rec (PatSet.empty, [wrap ct head; wrap ty tail]))
  in
  match pats with
  | [] -> failwith "empty pattern"
  | head::[] -> wrap ty head
  | head::tail -> merge ty (wrap ty head) (collect ty tail)
and merge ty x1 x2 =
  match x1, x2 with
  | Ok, _ | _, Ok -> Ok
  | Working (Term s1), Working (Term s2) ->
    let tmp = PatSet.union s1 s2 in
    let u, _ = get_universe ty in
    if PatSet.subset u tmp then
      Ok
    else
      Working (Term tmp)
  | Working (Rec (s1, l1)), Working (Rec (s2, l2)) ->
    let union = PatSet.union s1 s2 in
    let u, lst = get_universe ty in
    let tmp = map3 (fun x1 x2 ty -> merge ty x1 x2) l1 l2 lst in
    if PatSet.subset u union && all_ok tmp (List.length lst) then
      Ok
    else
      Working (Rec (union, tmp))
  | Working (Term te), Working (Rec (s, re))
  | Working (Rec (s, re)), Working (Term te) ->
    let union = PatSet.union te s in
    let u, lst = get_universe ty in
    if PatSet.subset u union && all_ok re (List.length lst) then
      Ok
    else
      Working (Rec (union, re))

let is_exhausted ty pats =
  match collect ty pats with
  | Ok -> true
  | _ -> false

let rec all_match_exhausted ast =
  let op1 t1 = all_match_exhausted t1 in
  let op2 t1 t2 = (all_match_exhausted t1) && (all_match_exhausted t2) in
  let op3 t1 t2 t3 = (all_match_exhausted t1) && (all_match_exhausted t2) && (all_match_exhausted t3) in
  match take_exp ast with
  | Add(t1, t2) | Sub(t1, t2) | Mul(t1, t2) | Div(t1, t2)
  | Eq(t1, t2) | Ne(t1, t2) | Gt(t1, t2) | Lt(t1, t2)
  | Let(_, t1, t2) | LetRec(_, _, t1, t2) | App(t1, t2) | ListCons(t1, t2)
  | Skip(t1, t2) -> op2 t1 t2
  | If(t1, t2, t3) -> op3 t1 t2 t3
  | Fun(_, t1) -> op1 t1
  | ListHead(t1) -> op1 t1
  | ListTail(t1) -> op1 t1
  | Print(t1) -> op1 t1
  | CallCC(t1) -> op1 t1
  | Match(t1, pats) ->
    let p, _ = List.split pats in
    (op1 t1) && (is_exhausted (Tinf.ast_type t1) p)
  | _ -> true