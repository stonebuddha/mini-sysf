open Batteries

type kind =
  | KdType
  | KdArrow of kind * kind

type ty =
  | TyVar of int * int
  | TyUnit
  | TyBool
  | TyInt
  | TyFloat
  | TyString
  | TyArrow of ty * ty
  | TyProd of ty list
  | TyVariant of (string * ty) list
  | TyRec of string * kind * ty
  | TyAll of string * kind * ty
  | TyAbs of string * kind * ty
  | TyApp of ty * ty

type prim_bin_op =
  | PBIntAdd
  | PBEq

type term =
  | TmVar of int * int
  | TmUnit
  | TmTrue
  | TmFalse
  | TmInt of int
  | TmFloat of float
  | TmString of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTuple of term list
  | TmProj of term * int
  | TmTag of string * term * ty
  | TmCase of term * (string * (string * term)) list
  | TmFold of ty
  | TmUnfold of ty
  | TmTAbs of string * kind * term
  | TmTApp of term * ty
  | TmLet of string * term * term
  | TmFix of term
  | TmIf of term * term * term
  | TmAscribe of term * ty
  | TmPrimBinOp of prim_bin_op * term * term

type binding =
  | NameBind
  | TyVarBind of kind
  | TyAbbBind of ty * (kind option)
  | TmAbbBind of term * (ty option)

type command =
  | Eval of term
  | Bind of string * binding

type context = (string * binding) list

let empty_ctx = []

let ctx_length ctx = List.length ctx

let add_binding ctx x bind = (x, bind) :: ctx

let add_name ctx x = add_binding ctx x NameBind

let index_to_name ctx i =
  let (x, _) = List.nth ctx i in
  x

let rec name_to_index ctx x =
  match ctx with
  | [] -> raise Not_found
  | (y, _) :: rest -> if y = x then 0 else 1 + (name_to_index rest x)

let rec is_name_bound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else is_name_bound rest x

let type_map on_var c tyT =
  let rec walk c tyT =
    match tyT with
    | TyVar (x, n) -> on_var c x n
    | TyUnit -> TyUnit
    | TyBool -> TyBool
    | TyInt -> TyInt
    | TyFloat -> TyFloat
    | TyString -> TyString
    | TyArrow (tyT1, tyT2) -> TyArrow (walk c tyT1, walk c tyT2)
    | TyProd tyTs -> TyProd (List.map (walk c) tyTs)
    | TyVariant ftys -> TyVariant (List.map (fun (tag, tyT) -> (tag, walk c tyT)) ftys)
    | TyRec (x, kd1, tyT2) -> TyRec (x, kd1, walk (c + 1) tyT2)
    | TyAll (x, kd1, tyT2) -> TyAll (x, kd1, walk (c + 1) tyT2)
    | TyAbs (x, kd1, tyT2) -> TyAbs (x, kd1, walk (c + 1) tyT2)
    | TyApp (tyT1, tyT2) -> TyApp (walk c tyT1, walk c tyT2)
  in
  walk c tyT

let term_map on_var on_type c tm =
  let rec walk c tm =
    match tm with
    | TmVar (x, n) -> on_var c x n
    | TmUnit -> TmUnit
    | TmTrue -> TmTrue
    | TmFalse -> TmFalse
    | TmInt i -> TmInt i
    | TmFloat f -> TmFloat f
    | TmString s -> TmString s
    | TmAbs (x, tyT1, tm2) -> TmAbs (x, on_type c tyT1, walk (c + 1) tm2)
    | TmApp (tm1, tm2) -> TmApp (walk c tm1, walk c tm2)
    | TmTuple tms -> TmTuple (List.map (walk c) tms)
    | TmProj (tm, i) -> TmProj (walk c tm, i)
    | TmTag (tag, tm1, tyT2) -> TmTag (tag, walk c tm1, on_type c tyT2)
    | TmCase (tm1, cases) -> TmCase (walk c tm1, List.map (fun (tag, (x, tm)) -> (tag, (x, walk (c + 1) tm))) cases)
    | TmFold tyT -> TmFold (on_type c tyT)
    | TmUnfold tyT -> TmUnfold (on_type c tyT)
    | TmTAbs (x, kd1, tm2) -> TmTAbs (x, kd1, walk (c + 1) tm2)
    | TmTApp (tm1, tyT2) -> TmTApp (walk c tm1, on_type c tyT2)
    | TmLet (x, tm1, tm2) -> TmLet (x, walk c tm1, walk (c + 1) tm2)
    | TmFix tm1 -> TmFix (walk c tm1)
    | TmIf (tm1, tm2, tm3) -> TmIf (walk c tm1, walk c tm2, walk c tm3)
    | TmAscribe (tm1, tyT2) -> TmAscribe (walk c tm1, on_type c tyT2)
    | TmPrimBinOp (bop, tm1, tm2) -> TmPrimBinOp (bop, walk c tm1, walk c tm2)
  in
  walk c tm

let type_shift_above d c tyT =
  type_map
    (fun c x n -> if x >= c then TyVar (x + d, n + d) else TyVar (x, n + d))
    c tyT

let term_shift_above d c tm =
  term_map
    (fun c x n -> if x >= c then TmVar (x + d, n + d) else TmVar (x, n + d))
    (type_shift_above d)
    c tm

let term_shift d tm = term_shift_above d 0 tm

let type_shift d tyT = type_shift_above d 0 tyT

let binding_shift d bind =
  match bind with
  | NameBind -> NameBind
  | TyVarBind kd -> TyVarBind kd
  | TyAbbBind (tyT, opt) -> TyAbbBind (type_shift d tyT, opt)
  | TmAbbBind (tm, opt) -> TmAbbBind (term_shift d tm, Option.map (type_shift d) opt)

let term_subst j tm_s tm =
  term_map
    (fun j x n -> if x = j then term_shift j tm_s else TmVar (x, n))
    (fun j tyT -> tyT)
    j tm

let term_subst_top tm_s tm =
  term_shift (-1) (term_subst 0 (term_shift 1 tm_s) tm)

let type_subst j tyT_s tyT =
  type_map
    (fun j x n -> if x = j then type_shift j tyT_s else TyVar (x, n))
    j tyT

let type_subst_top tyT_s tyT =
  type_shift (-1) (type_subst 0 (type_shift 1 tyT_s) tyT)

let type_term_subst j tyT_s tm =
  term_map
    (fun j x n -> TmVar (x, n))
    (fun j tyT -> type_subst j tyT_s tyT)
    j tm

let type_term_subst_top tyT_s tm =
  term_shift (-1) (type_term_subst 0 (type_shift 1 tyT_s) tm)

let get_binding ctx i =
  let (_, bind) = List.nth ctx i in
  binding_shift (i + 1) bind

let get_type_from_context ctx i =
  match get_binding ctx i with
  | TmAbbBind (_, Some tyT) -> tyT
  | _ -> failwith "failure with get_type_from_context"
