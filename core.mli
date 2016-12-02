open Syntax

val type_of : context -> term -> ty
val kind_of : context -> ty -> kind
val type_eqv : context -> ty -> ty -> bool
val simplify_type : context -> ty -> ty
