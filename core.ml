open Batteries
open Syntax
open Smt

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

let rec encode_term ctx smt_ctx tm =
  match tm with
  | TmVar (i, _) ->
    (match List.nth smt_ctx i with
     | Some e -> e
     | None ->
       match get_binding ctx i with
       | TmAbbBind (tm1, Some _) -> encode_term ctx smt_ctx tm1
       | _ -> failwith "encode_term")
  | _ -> smt_constant_true ()

let encode_binding (x, bind) (ctx, smt_ctx) =
  match bind with
  | VarBind tyT ->
    (match simplify_type ctx tyT with
     | TyBase btyT ->
       let dec =
         match btyT with
         | BTyUnit -> smt_declare_unit x
         | BTyBool -> smt_declare_bool x
         | BTyInt -> smt_declare_int x
         | BTyFloat -> smt_declare_float x
       in
       (add_binding ctx x bind, Some dec :: smt_ctx)
     | TyRefined (y, btyT, tms) ->
       let dec =
         match btyT with
         | BTyUnit -> smt_declare_unit x
         | BTyBool -> smt_declare_bool x
         | BTyInt -> smt_declare_int x
         | BTyFloat -> smt_declare_float x
       in
       let smt_ctx' = Some dec :: smt_ctx in
       List.iter (fun tm -> smt_assert (encode_term (add_binding ctx y (VarBind (TyBase btyT))) smt_ctx' tm)) tms;
       (add_binding ctx x bind, smt_ctx')
     | TyArrow (_, tyT1, tyT2) -> failwith "FIXME"
     | _ -> (add_binding ctx x bind, Some (smt_declare_uninterp x) :: smt_ctx))
  | _ -> (add_binding ctx x bind, None :: smt_ctx)

let rec type_sub ctx tyT1 tyT2 =
  let tyT1 = simplify_type ctx tyT1 in
  let tyT2 = simplify_type ctx tyT2 in
  match (tyT1, tyT2) with
  | (TyVar (i, _), _) when is_ty_abb ctx i -> type_sub ctx (get_ty_abb ctx i) tyT2
  | (_, TyVar (i, _)) when is_ty_abb ctx i -> type_sub ctx tyT1 (get_ty_abb ctx i)
  | (TyVar (i, _), TyVar (j, _)) -> i = j
  | (TyBase btyT1, _) -> type_sub ctx (TyRefined ("_v", btyT1, [TmTrue])) tyT2
  | (_, TyBase btyT2) -> type_sub ctx tyT1 (TyRefined ("_v", btyT2, [TmTrue]))
  | (TyArrow (x, tyT11, tyT12), TyArrow (_, tyT21, tyT22)) ->
    let ctx' = add_binding ctx x (VarBind tyT21) in
    type_sub ctx tyT21 tyT11 && type_sub ctx' tyT12 tyT22
  | (TyProd tyTs1, TyProd tyTs2) ->
    List.length tyTs1 = List.length tyTs2 &&
    List.for_all (fun (tyT1, tyT2) -> type_sub ctx tyT1 tyT2) (List.combine tyTs1 tyTs2)
  | (TyVariant ftys1, TyVariant ftys2) ->
    List.length ftys1 = List.length ftys2 &&
    List.for_all
      (fun (tag2, tyT2) ->
         try
           let tyT1 = List.assoc tag2 ftys1 in
           type_sub ctx tyT1 tyT2
         with Not_found -> false)
      ftys2
  | (TyRec (x, kd11, tyT12), TyRec (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_sub ctx' tyT12 tyT22 (* iso-recursive-style subtype checking *)
  | (TyAll (x, kd11, tyT12), TyAll (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_sub ctx' tyT12 tyT22
  | (TyAbs (x, kd11, tyT12), TyAbs (_, kd21, tyT22)) ->
    let ctx' = add_name ctx x in
    kd11 = kd21 && type_sub ctx' tyT12 tyT22
  | (TyApp (tyT11, tyT12), TyApp (tyT21, tyT22)) -> type_sub ctx tyT11 tyT21 && type_sub ctx tyT12 tyT22
  | (TyRefined (x, btyT1, tms1), TyRefined (_, btyT2, tms2)) when btyT1 = btyT2 ->
    print_endline ("ADMITTED: " ^ string_of_type ctx tyT1 ^ " <: " ^ string_of_type ctx tyT2);
    smt_reset ();
    smt_assert (smt_constant_false ());
    smt_check ()
  | _ -> false

let type_eqv ctx tyT1 tyT2 =
  type_sub ctx tyT1 tyT2 && type_sub ctx tyT2 tyT1

let get_kind ctx i =
  match get_binding ctx i with
  | TyVarBind kd -> kd
  | TyAbbBind (_, Some kd) -> kd
  | _ -> failwith "failure with get_kind"

let rec kind_of ctx tyT =
  match tyT with
  | TyVar (i, _) -> get_kind ctx i
  | TyBase _ -> KdType
  | TyArrow (x, tyT1, tyT2) ->
    if kind_of ctx tyT1 = KdType && kind_of (add_binding ctx x (VarBind tyT1)) tyT2 = KdType then KdType
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
  | TyRefined (x, btyT, tms) ->
    let ctx' = add_binding ctx x (VarBind (TyBase btyT)) in
    if List.for_all (fun tm -> type_sub ctx' (type_of ctx' tm) (TyBase BTyBool)) tms then KdType
    else failwith "failure with refined type"

and check_kind_type ctx tyT =
  match kind_of ctx tyT with
  | KdType -> ()
  | _ -> failwith "check_kind_type"

and type_of ctx tm =
  match tm with
  | TmVar (i, _) ->
    let tyT = get_type_from_context ctx i in
    (match simplify_type ctx tyT with
     | TyBase btyT -> TyRefined ("_v", btyT, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
     | TyRefined (x, btyT, _) -> TyRefined ("_v", btyT, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
     | _ -> tyT)
  | TmUnit -> TyRefined ("_v", BTyUnit, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), TmUnit)])
  | TmTrue -> TyRefined ("_v", BTyBool, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), TmTrue)])
  | TmFalse -> TyRefined ("_v", BTyBool, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), TmFalse)])
  | TmInt i -> TyRefined ("_v", BTyInt, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), TmInt i)])
  | TmFloat f -> TyRefined ("_v", BTyFloat, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), TmFloat f)])
  | TmAbs (x, tyT1, tm2) ->
    check_kind_type ctx tyT1;
    let ctx' = add_binding ctx x (VarBind tyT1) in
    let tyT2 = type_of ctx' tm2 in
    TyArrow (x, tyT1, tyT2)
  | TmApp (tm1, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let tyT2 = type_of ctx tm2 in
    (match simplify_type ctx tyT1 with
     | TyArrow (x, tyT11, tyT12) when type_sub ctx tyT2 tyT11 -> term_type_subst_top tm2 tyT12
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
     | TyVariant ftys -> (try if type_sub ctx tyT1 (List.assoc tag ftys) then tyT2 else failwith "failure with tag expression"
                          with Not_found -> failwith "failure with tag expression")
     | _ -> failwith "failure with tag expression")
  | TmCase (tm1, opt, cases) ->
    (match simplify_type ctx (type_of ctx tm1) with
     | TyVariant ftys ->
       if List.length ftys = List.length cases then
         try
           let tyTrets =
             List.map
               (fun (tag, tyT) ->
                  let (x, tm) = List.assoc tag cases in
                  let ctx' = add_binding ctx x (VarBind tyT) in
                  (ctx', type_of ctx' tm))
               ftys
           in
           match opt with
           | Some tyTret ->
             if List.for_all (fun (ctx', tyT') -> type_sub ctx' tyT' (type_shift 1 tyTret)) tyTrets then
               tyTret
             else failwith "failure with case expression"
           | _ -> failwith "FIXME"
         with Not_found -> failwith "failure with case expression"
       else
         failwith "failure with case expression"
     | _ -> failwith "failure with case expression")
  | TmFold tyT ->
    (match simplify_type ctx tyT with
     | TyRec (x, kd1, tyT2) -> TyArrow ("_", type_subst_top tyT tyT2, type_shift 1 tyT) (* TODO: deal with fold directly *)
     | _ -> failwith "failure with fold")
  | TmUnfold tyT ->
    (match simplify_type ctx tyT with
     | TyRec (x, kd1, tyT2) -> TyArrow ("_", tyT, type_shift 1 (type_subst_top tyT tyT2)) (* TODO: deal with unfold directly *)
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
  | TmLet (x, tm1, opt, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let ctx' = add_binding ctx x (VarBind tyT1) in
    let tyT2 = type_of ctx' tm2 in
    (match opt with
     | Some tyTret ->
       if type_sub ctx' tyT2 (type_shift 1 tyTret) then
         tyTret
       else failwith "failure with let expression"
     | _ -> failwith "FIXME")
  | TmFix tm1 ->
    let tyT1 = type_of ctx tm1 in
    (match simplify_type ctx tyT1 with
     | TyArrow (_, tyT11, tyT12) when can_type_escape_one_level tyT12 && type_eqv ctx tyT11 (type_shift (-1) tyT12) ->
       type_shift (-1) tyT12
     | _ -> failwith "failure when typing fix expression")
  | TmIf (tm1, opt, tm2, tm3) ->
    if type_sub ctx (type_of ctx tm1) (TyBase BTyBool) then
      let ctx2 = add_binding ctx "_" (VarBind (TyRefined ("_v", BTyUnit, [tm1]))) in
      let ctx3 = add_binding ctx "_" (VarBind (TyRefined ("_v", BTyUnit, [TmPrimUnOp (PUNot, tm1)]))) in
      let tyT2 = type_of ctx2 tm2 in
      let tyT3 = type_of ctx3 tm3 in
      match opt with
      | Some tyTret ->
        if type_sub ctx2 tyT2 (type_shift 1 tyTret) && type_sub ctx3 tyT3 (type_shift 1 tyTret) then
          tyTret
        else failwith "failure with if expression"
      | _ -> failwith "FIXME"
    else
      failwith "failure with if expression"
  | TmAscribe (tm1, tyT2) ->
    check_kind_type ctx tyT2;
    if type_sub ctx (type_of ctx tm1) tyT2 then tyT2 else failwith "failure with ascription"
  | TmPrimBinOp (bop, tm1, tm2) ->
    let tyT1 = type_of ctx tm1 in
    let tyT2 = type_of ctx tm2 in
    (match bop with
     | PBIntAdd
     | PBIntDiff
     | PBIntMul ->
       if type_sub ctx tyT1 (TyBase BTyInt) && type_sub ctx tyT2 (TyBase BTyInt) then
         TyRefined ("_v", BTyInt, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
       else failwith "failure with bop"
     | PBIntDiv ->
       if type_sub ctx tyT1 (TyBase BTyInt) &&
          type_sub ctx tyT2 (TyRefined ("_v", BTyInt, [TmPrimBinOp (PBNe, TmVar (0, 1 + ctx_length ctx), TmInt 0)]))
       then TyRefined ("_v", BTyInt, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
       else failwith "failure with bop"
     | PBEq
     | PBNe ->
       if
         (type_sub ctx tyT1 (TyBase BTyUnit) && type_sub ctx tyT2 (TyBase BTyUnit)) ||
         (type_sub ctx tyT1 (TyBase BTyBool) && type_sub ctx tyT2 (TyBase BTyBool)) ||
         (type_sub ctx tyT1 (TyBase BTyInt) && type_sub ctx tyT2 (TyBase BTyInt)) ||
         (type_sub ctx tyT1 (TyBase BTyFloat) && type_sub ctx tyT2 (TyBase BTyFloat))
       then
         TyRefined ("_v", BTyBool, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
       else
         failwith "failure with bop"
     | PBLt
     | PBLe
     | PBGt
     | PBGe ->
       if
         (type_sub ctx tyT1 (TyBase BTyInt) && type_sub ctx tyT2 (TyBase BTyInt)) ||
         (type_sub ctx tyT1 (TyBase BTyFloat) && type_sub ctx tyT2 (TyBase BTyFloat))
       then
         TyRefined ("_v", BTyBool, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
       else
         failwith "failure with bop")
  | TmPrimUnOp (uop, tm1) ->
    let tyT1 = type_of ctx tm1 in
    (match uop with
     | PUNot ->
       if type_sub ctx tyT1 (TyBase BTyBool) then
         TyRefined ("_v", BTyBool, [TmPrimBinOp (PBEq, TmVar (0, 1 + ctx_length ctx), term_shift 1 tm)])
       else failwith "failure with uop")
