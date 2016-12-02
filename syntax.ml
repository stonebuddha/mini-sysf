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

let index_to_name ctx n =
  let (x, _) = List.nth ctx n in
  x

let rec name_to_index ctx x =
  match ctx with
  | [] -> raise Not_found
  | (y, _) :: rest -> if y = x then 0 else 1 + (name_to_index rest x)

let rec is_name_bound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else is_name_bound rest x
