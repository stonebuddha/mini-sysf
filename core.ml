open Batteries
open Syntax

exception No_rule_applies

let is_ty_abb ctx i =
  match get_binding ctx i with
  | TyAbbBind _ -> true
  | _ -> false

let get_ty_abb ctx i =
  match get_binding ctx i with
  | TyAbbBind (tyT, _) -> tyT
  | _ -> raise No_rule_applies

let rec compute_type ctx tyT =
  match tyT with
  | TyVar (i, _) when is_ty_abb ctx i -> get_ty_abb ctx i
  | TyApp (TyAbs (_, _, tyT12), tyT2) -> type_subst_top tyT2 tyT12
  | _ -> raise No_rule_applies

let rec simplify_type ctx tyT =
  let tyT =
    match tyT with
    | TyApp (tyT1, tyT2) -> TyApp (simplify_type ctx tyT1, simplify_type ctx tyT2)
    | _ -> tyT
  in
  try
    let tyT' = compute_type ctx tyT in
    simplify_type ctx tyT'
  with No_rule_applies -> tyT

let rec type_eqv ctx tyT1 tyT2 =
  let tyT1 = simplify_type ctx tyT1 in
  let tyT2 = simplify_type ctx tyT2 in
  failwith "TODO"

let get_kind ctx i =
  match get_binding ctx i with
  | TyVarBind kd -> kd
  | TyAbbBind (_, Some kd) -> kd
  | _ -> failwith "failure with get_kind"

let rec kind_of ctx tyT =
  match tyT with
  | TyVar (i, _) -> get_kind ctx i
  | TyUnit -> KdType
  | TyBool -> KdType
  | TyInt -> KdType
  | TyFloat -> KdType
  | TyString -> KdType
  | TyArrow (tyT1, tyT2) ->
    if kind_of ctx tyT1 = KdType && kind_of ctx tyT2 = KdType then KdType
    else failwith "type kind expected"
  | TyProd tyTs ->
    if List.for_all (fun tyT -> kind_of ctx tyT = KdType) tyTs then KdType
    else failwith "type kind expected"
  | TyVariant ftys ->
    if List.for_all (fun (_, tyT) -> kind_of ctx tyT = KdType) ftys then KdType
    else failwith "type kind expected"
  | TyRec (x, kd1, tyT2) ->
    let ctx' = add_binding ctx x (TyVarBind kd1) in
    let kd2 = kind_of ctx' tyT2 in
    if kd1 = kd2 then kd1 else failwith "kinds mismatch"
  | TyAll (x, kd1, tyT2) ->
    let ctx' = add_binding ctx x (TyVarBind kd1) in
    if kind_of ctx' tyT2 = KdType then KdType
    else failwith "type kind expected"
  | TyAbs (x, kd1, tyT2) ->
    let ctx' = add_binding ctx x (TyVarBind kd1) in
    let kd2 = kind_of ctx' tyT2 in
    KdArrow (kd1, kd2)
  | TyApp (tyT1, tyT2) ->
    let kd1 = kind_of ctx tyT1 in
    let kd2 = kind_of ctx tyT2 in
    (match kd1 with
     | KdArrow (kd11, kd12) when kd2 = kd11 -> kd12
     | _ -> failwith "failure with type application")

let check_kind_type ctx tyT =
  match kind_of ctx tyT with
  | KdType -> ()
  | _ -> failwith "check_kind_type"

let rec type_of ctx tm = TyUnit
