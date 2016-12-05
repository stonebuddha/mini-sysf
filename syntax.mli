open Batteries

type kind =
  | KdType
  | KdArrow of kind * kind

type base_ty =
  | BTyUnit
  | BTyBool
  | BTyInt
  | BTyFloat

type prim_bin_op =
  | PBIntAdd
  | PBIntDiff
  | PBIntMul
  | PBIntDiv
  | PBEq
  | PBNe
  | PBLt
  | PBLe
  | PBGt
  | PBGe

type prim_un_op =
  | PUNot

type ty =
  | TyVar of int * int
  | TyBase of base_ty
  | TyArrow of string * ty * ty
  | TyProd of ty list
  | TyVariant of (string * ty) list
  | TyRec of string * kind * ty
  | TyAll of string * kind * ty
  | TyAbs of string * kind * ty
  | TyApp of ty * ty
  | TyRefined of string * base_ty * term list

and term =
  | TmVar of int * int
  | TmUnit
  | TmTrue
  | TmFalse
  | TmInt of int
  | TmFloat of float
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTuple of term list
  | TmProj of term * int
  | TmTag of string * term * ty
  | TmCase of term * ty option * (string * (string * term)) list
  | TmFold of ty
  | TmUnfold of ty
  | TmTAbs of string * kind * term
  | TmTApp of term * ty
  | TmLet of string * term * term
  | TmFix of term
  | TmIf of term * ty option * term * term
  | TmAscribe of term * ty
  | TmPrimBinOp of prim_bin_op * term * term
  | TmPrimUnOp of prim_un_op * term

type binding =
  | NameBind
  | VarBind of ty
  | TyVarBind of kind
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
val get_binding : context -> int -> binding
val name_to_index : context -> string -> int
val is_name_bound : context -> string -> bool
val get_type_from_context : context -> int -> ty

val term_shift : int -> term -> term
val term_subst_top : term -> term -> term
val type_shift : int -> ty -> ty
val type_subst_top : ty -> ty -> ty
val type_term_subst_top : ty -> term -> term
val term_type_subst_top : term -> ty -> ty

val string_of_kind : context -> kind -> string
val string_of_type : context -> ty -> string
val string_of_term : context -> term -> string
