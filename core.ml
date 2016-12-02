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
    | TyApp (tyT1, tyT2) -> TyApp (simplify_type ctx tyT1, tyT2)
    | _ -> tyT
  in
  try
    let tyT' = compute_type ctx tyT in
    simplify_type ctx tyT'
  with No_rule_applies -> tyT

let rec type_eqv ctx tyT1 tyT2 =
  let tyT1 = simplify_type ctx tyT1 in
  let tyT2 = simplify_type ctx tyT2 in
  match (tyT1, tyT2) with
  | (TyVar (i, _), _) when is_ty_abb ctx i -> type_eqv ctx (get_ty_abb ctx i) tyT2
  | (_, TyVar (i, _)) when is_ty_abb ctx i -> type_eqv ctx tyT1 (get_ty_abb ctx i)
  | (TyVar (i, _), TyVar (j, _)) -> i = j
  | (TyUnit, TyUnit) -> true
  | (TyBool, TyBool) -> true
  | (TyInt, TyInt) -> true
  | (TyFloat, TyFloat) -> true
  | (TyString, TyString) -> true
  | (TyArrow (tyT11, tyT12), TyArrow (tyT21, tyT22)) -> type_eqv ctx tyT11 tyT21 && type_eqv ctx tyT12 tyT22
  | (TyProd tyTs1, TyProd tyTs2) ->
    List.length tyTs1 = List.length tyTs2 &&
    List.for_all (fun (tyT1, tyT2) -> type_eqv ctx tyT1 tyT2) (List.combine tyTs1 tyTs2)
  | (TyVariant ftys1, TyVariant ftys2) ->
    List.length ftys1 = List.length ftys2 &&
    List.for_all
      (fun (tag2, tyT2) ->
         try
           let tyT1 = List.assoc tag2 ftys1 in
           type_eqv ctx tyT1 tyT2
         with Not_found -> false)
      ftys2
  | (TyRec (x, kd11, tyT12), TyRec (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_eqv ctx' tyT12 tyT22
  | (TyAll (x, kd11, tyT12), TyAll (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_eqv ctx' tyT12 tyT22
  | (TyAbs (x, kd11, tyT12), TyAbs (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_eqv ctx' tyT12 tyT22
  | (TyApp (tyT11, tyT12), TyApp (tyT21, tyT22)) -> type_eqv ctx tyT11 tyT21 && type_eqv ctx tyT12 tyT22
  | _ -> false

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
     | _ -> failwith "failure with type-level application")

let check_kind_type ctx tyT =
  match kind_of ctx tyT with
  | KdType -> ()
  | _ -> failwith "check_kind_type"

let rec type_of ctx tm =
  match tm with
  | TmVar (i, _) -> get_type_from_context ctx i
  | TmUnit -> TyUnit
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmInt _ -> TyInt
  | TmFloat _ -> TyFloat
  | TmString _ -> TyString
  | TmAbs (x, tyT1, tm2) ->
    check_kind_type ctx tyT1;
    let ctx' = add_binding ctx x (VarBind tyT1) in
    let tyT2 = type_of ctx' tm2 in
    TyArrow (tyT1, type_shift (-1) tyT2)
  | TmApp (tm1, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let tyT2 = type_of ctx tm2 in
    (match simplify_type ctx tyT1 with
     | TyArrow (tyT11, tyT12) when type_eqv ctx tyT2 tyT11 -> tyT12
     | _ -> failwith "failure when typing application")
  | TmTuple tms -> TyProd (List.map (type_of ctx) tms)
  | TmProj (tm1, i) ->
    (match simplify_type ctx (type_of ctx tm1) with
     | TyProd tyTs -> (try List.nth tyTs (i - 1)
                       with Not_found -> failwith "failure with projection")
     | _ -> failwith "failure with projection")
  | TmTag (tag, tm1, tyT2) ->
    check_kind_type ctx tyT2;
    let tyT1 = type_of ctx tm1 in
    (match simplify_type ctx tyT2 with
     | TyVariant ftys -> (try if type_eqv ctx tyT1 (List.assoc tag ftys) then tyT2 else failwith "failure with tag expression"
                          with Not_found -> failwith "failure with tag expression")
     | _ -> failwith "failure with tag expression")
  | TmCase (tm1, cases) ->
    (match simplify_type ctx (type_of ctx tm1) with
     | TyVariant ftys ->
       if List.length ftys = List.length cases then
         try
           let tyTrets =
             List.map
               (fun (tag, tyT) ->
                  let (x, tm) = List.assoc tag cases in
                  let ctx' = add_binding ctx x (VarBind tyT) in
                  type_shift (-1) (type_of ctx' tm))
               ftys
           in
           let tyThd = List.hd tyTrets in
           let tyTtl = List.tl tyTrets in
           if List.for_all (type_eqv ctx tyThd) tyTtl then tyThd else failwith "failure with case expression"
         with Not_found -> failwith "failure with case expression"
       else
         failwith "failure with case expression"
     | _ -> failwith "failure with case expression")
  | TmFold tyT ->
    (match simplify_type ctx tyT with
     | TyRec (x, kd1, tyT2) -> TyArrow (type_subst_top tyT tyT2, tyT)
     | _ -> failwith "failure with fold")
  | TmUnfold tyT ->
    (match simplify_type ctx tyT with
     | TyRec (x, kd1, tyT2) -> TyArrow (tyT, type_subst_top tyT tyT2)
     | _ -> failwith "failure with unfold")
  | TmTAbs (x, kd1, tm2) ->
    let ctx' = add_binding ctx x (TyVarBind kd1) in
    let tyT2 = type_of ctx' tm2 in
    TyAll (x, kd1, tyT2)
  | TmTApp (tm1, tyT2) ->
    let tyT1 = type_of ctx tm1 in
    let kd2 = kind_of ctx tyT2 in
    (match simplify_type ctx tyT1 with
     | TyAll (_, kd11, tyT12) ->
       if kd11 = kd2 then type_subst_top tyT2 tyT12 else failwith "failure when typing type application"
     | _ -> failwith "failure when typing type application")
  | TmLet (x, tm1, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let tyT2 = type_of (add_binding ctx x (VarBind tyT1)) tm2 in
    type_shift (-1) tyT2
  | TmFix tm1 ->
    let tyT1 = type_of ctx tm1 in
    (match simplify_type ctx tyT1 with
     | TyArrow (tyT11, tyT12) when type_eqv ctx tyT11 tyT12 -> tyT12
     | _ -> failwith "failure when typing fix expression")
  | TmIf (tm1, tm2, tm3) ->
    if type_eqv ctx (type_of ctx tm1) TyBool then
      let tyT2 = type_of ctx tm2 in
      if type_eqv ctx (type_of ctx tm3) tyT2 then tyT2 else failwith "failure with if expression"
    else
      failwith "failure with if expression"
  | TmAscribe (tm1, tyT2) ->
    check_kind_type ctx tyT2;
    if type_eqv ctx (type_of ctx tm1) tyT2 then tyT2 else failwith "failure with ascription"
  | TmPrimBinOp (bop, tm1, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let tyT2 = type_of ctx tm2 in
    (match bop with
     | PBIntAdd -> if type_eqv ctx tyT1 TyInt && type_eqv ctx tyT2 TyInt then TyInt else failwith "failure with bop"
     | PBEq ->
       if
         (type_eqv ctx tyT1 TyUnit && type_eqv ctx tyT2 TyUnit) ||
         (type_eqv ctx tyT1 TyBool && type_eqv ctx tyT2 TyBool) ||
         (type_eqv ctx tyT1 TyInt && type_eqv ctx tyT2 TyInt) ||
         (type_eqv ctx tyT1 TyFloat && type_eqv ctx tyT2 TyFloat) ||
         (type_eqv ctx tyT1 TyString && type_eqv ctx tyT2 TyString)
       then
         TyBool
       else
         failwith "failure with bop")
