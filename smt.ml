let _ = Z3.Log.open_ "z3.log"
let z3_cfg = ["timeout", "10000"]
let z3_ctx = Z3.mk_context z3_cfg
let z3_solver = Z3.Solver.mk_solver z3_ctx None
let z3_uninterp = ref (Z3.Sort.mk_uninterpreted z3_ctx (Z3.Symbol.mk_string z3_ctx "T"))

type smt_expr = Z3.Expr.expr
type smt_sort = Z3.Sort.sort

let smt_reset () =
  Z3.Solver.reset z3_solver;
  z3_uninterp := Z3.Sort.mk_uninterpreted z3_ctx (Z3.Symbol.mk_string z3_ctx "T")

let smt_check () =
  match Z3.Solver.check z3_solver [] with
  | Z3.Solver.UNSATISFIABLE -> true
  | _ -> false

let smt_declare_unit _ = Z3.Boolean.mk_false z3_ctx

let smt_declare_bool x = Z3.Boolean.mk_const z3_ctx (Z3.Symbol.mk_string z3_ctx x)

let smt_declare_int x = Z3.Arithmetic.Integer.mk_const z3_ctx (Z3.Symbol.mk_string z3_ctx x)

let smt_declare_float x = Z3.FloatingPoint.mk_const z3_ctx (Z3.Symbol.mk_string z3_ctx x) (Z3.FloatingPoint.mk_sort_64 z3_ctx)

let smt_declare_uninterp x = Z3.Expr.mk_const z3_ctx (Z3.Symbol.mk_string z3_ctx x) (!z3_uninterp)

let smt_constant_unit () = Z3.Boolean.mk_false z3_ctx

let smt_constant_true () = Z3.Boolean.mk_true z3_ctx

let smt_constant_false () = Z3.Boolean.mk_false z3_ctx

let smt_constant_int i = Z3.Arithmetic.Integer.mk_numeral_i z3_ctx i

let smt_constant_float f = Z3.FloatingPoint.mk_numeral_f z3_ctx f (Z3.FloatingPoint.mk_sort_64 z3_ctx)

let smt_assert e = Z3.Solver.add z3_solver [e]

let smt_sort_unit () = Z3.Boolean.mk_sort z3_ctx

let smt_sort_bool () = Z3.Boolean.mk_sort z3_ctx

let smt_sort_int () = Z3.Arithmetic.Integer.mk_sort z3_ctx

let smt_sort_float () = Z3.FloatingPoint.mk_sort_64 z3_ctx

let smt_sort_uninterp () = !z3_uninterp
