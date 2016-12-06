type smt_expr
type smt_sort

val smt_reset : unit -> unit
val smt_check : unit -> bool
val smt_declare_unit : string -> smt_expr
val smt_declare_bool : string -> smt_expr
val smt_declare_int : string -> smt_expr
val smt_declare_float : string -> smt_expr
val smt_declare_uninterp : string -> smt_expr
val smt_constant_unit : unit -> smt_expr
val smt_constant_true : unit -> smt_expr
val smt_constant_false : unit -> smt_expr
val smt_constant_int : int -> smt_expr
val smt_constant_float : float -> smt_expr
val smt_assert : smt_expr -> unit
val smt_sort_unit : unit -> smt_sort
val smt_sort_bool : unit -> smt_sort
val smt_sort_int : unit -> smt_sort
val smt_sort_float : unit -> smt_sort
val smt_sort_uninterp : unit -> smt_sort
val smt_make_conj : smt_expr -> smt_expr -> smt_expr
val smt_make_not : smt_expr -> smt_expr
val smt_make_eq : smt_expr -> smt_expr -> smt_expr
val smt_make_ne : smt_expr -> smt_expr -> smt_expr
val smt_make_lt : smt_expr -> smt_expr -> smt_expr
val smt_make_le : smt_expr -> smt_expr -> smt_expr
val smt_make_gt : smt_expr -> smt_expr -> smt_expr
val smt_make_ge : smt_expr -> smt_expr -> smt_expr
val smt_make_int_add : smt_expr -> smt_expr -> smt_expr
val smt_make_int_diff : smt_expr -> smt_expr -> smt_expr
val smt_make_int_mul : smt_expr -> smt_expr -> smt_expr
val smt_make_int_div : smt_expr -> smt_expr -> smt_expr
