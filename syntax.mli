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

type context
val empty_ctx : context
val ctx_length : context -> int
val add_binding : context -> string -> binding -> context
val add_name : context -> string -> context
val index_to_name : context -> int -> string
(* val get_binding : context -> int -> binding *)
val name_to_index : context -> string -> int
val is_name_bound : context -> string -> bool
(* val get_type_from_context : context -> int -> ty *)