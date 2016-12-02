%{
open Syntax
%}

%token TUNIT
%token TBOOL
%token TINT
%token TFLOAT
%token TSTRING
%token ALL
%token AS
%token LAMBDA
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token FIX
%token UNIT
%token TRUE
%token FALSE
%token FOLD
%token UNFOLD
%token CASE
%token OF
%token EOF

%token <string> UCID
%token <string> LCID
%token <int> INTV
%token <float> FLOATV
%token <string> STRINGV

%token SEMI
%token EQ
%token LPAREN
%token RPAREN
%token LT
%token GT
%token COMMA
%token COLON
%token ARROW
%token DOT
%token LSQUARE
%token RSQUARE
%token DOTONE
%token DOTTWO
%token DARROW
%token VBAR
%token LCURLY
%token RCURLY
%token ADD
%token DEQ

%start <Syntax.context -> (Syntax.command list * Syntax.context)> top_level

%%

top_level: top = rev_top_level; EOF { fun ctx -> let (cmds, ctx) = top ctx in (List.rev cmds, ctx) };

rev_top_level:
  | (* empty *) { fun ctx -> ([], ctx) }
  | top = rev_top_level; SEMI; cmd = command
    { fun ctx ->
      let (cmds, ctx) = top ctx in
      let (cmd, ctx) = cmd ctx in
      (cmd :: cmds, ctx) }
  ;

command:
  | e = term { fun ctx -> (Eval (e ctx), ctx) }
  | x = LCID; EQ; e = term { fun ctx -> (Bind (x, TmAbbBind (e ctx, None)), add_name ctx x) }
  | x = UCID; EQ; t = ty { fun ctx -> (Bind (x, TyAbbBind (t ctx)), add_name ctx x) }
  ;

atom_ty:
  | LPAREN; t = ty; RPAREN { t }
  | x = UCID
    { fun ctx -> TyVar (name_to_index ctx x, ctx_length ctx) }
  | TUNIT { fun ctx -> TyUnit }
  | TBOOL { fun ctx -> TyBool }
  | TINT { fun ctx -> TyInt }
  | TFLOAT { fun ctx -> TyFloat }
  | TSTRING { fun ctx -> TyString }
  | LT; ts = separated_list(COMMA, field_ty); GT { fun ctx -> TyVariant (List.map (fun t -> t ctx) ts) }
  ;

field_ty:
  | x = LCID; COLON; t = ty { fun ctx -> (x, t ctx) }
  ;

arrow_ty:
  | t1 = atom_ty; ARROW; t2 = arrow_ty { fun ctx -> TyArrow (t1 ctx, t2 ctx) }
  | t = atom_ty { t }
  ;

ty:
  | t = arrow_ty { t }
  | ALL; x = UCID; DOT; t = ty
    { fun ctx -> let ctx' = add_name ctx x in TyAll (x, t ctx') }
  ;

term:
  | e = app_term { e }
  | LAMBDA; x = LCID; COLON; t = ty; DOT; e = term
    { fun ctx -> let ctx' = add_name ctx x in TmAbs (x, t ctx, e ctx') }
  | LET; x = LCID; EQ; e1 = term; IN; e2 = term
    { fun ctx -> let ctx' = add_name ctx x in TmLet (x, e1 ctx, e2 ctx') }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term
    { fun ctx -> TmIf (e1 ctx, e2 ctx, e3 ctx) }
  | LAMBDA; x = UCID; DOT; e = term
    { fun ctx -> let ctx' = add_name ctx x in TmTAbs (x, e ctx') }
  | CASE; e = term; OF; cs = separated_list(VBAR, case)
    { fun ctx -> TmCase (e ctx, List.map (fun c -> c ctx) cs) }
  ;

case: LT; tag = LCID; EQ; x = LCID; GT; DARROW; e = app_term { fun ctx -> let ctx' = add_name ctx x in (tag, (x, e ctx')) };

app_term:
  | e = path_term { e }
  | e1 = app_term; e2 = path_term
    { fun ctx -> TmApp (e1 ctx, e2 ctx) }
  | FIX; e = path_term
    { fun ctx -> TmFix (e ctx) }
  | FOLD; LSQUARE; t = ty; RSQUARE
    { fun ctx -> TmFold (t ctx) }
  | UNFOLD; LSQUARE; t = ty; RSQUARE
    { fun ctx -> TmUnfold (t ctx) }
  | e = app_term; LSQUARE; t = ty; RSQUARE
    { fun ctx -> TmTApp (e ctx, t ctx) }
  | e1 = path_term; bop = prim_bin_op; e2 = path_term
    { fun ctx -> TmPrimBinOp (bop, e1 ctx, e2 ctx) }
  ;

prim_bin_op:
  | ADD { PBIntAdd }
  | DEQ { PBEq }
  ;

path_term:
  | e = path_term; DOTONE { fun ctx -> TmFst (e ctx) }
  | e = path_term; DOTTWO { fun ctx -> TmSnd (e ctx) }
  | e = ascribe_term { e }
  ;

ascribe_term:
  | e = atom_term; AS; t = ty { fun ctx -> TmAscribe (e ctx, t ctx) }
  | e = atom_term { e }
  ;

atom_term:
  | LPAREN; e = term; RPAREN { e }
  | x = LCID { fun ctx -> TmVar (name_to_index ctx x, ctx_length ctx) }
  | UNIT { fun ctx -> TmUnit }
  | TRUE { fun ctx -> TmTrue }
  | FALSE { fun ctx -> TmFalse }
  | i = INTV { fun ctx -> TmInt i }
  | f = FLOATV { fun ctx -> TmFloat f }
  | s = STRINGV { fun ctx -> TmString s }
  | LCURLY; e1 = term; COMMA; e2 = term; RCURLY { fun ctx -> TmPair (e1 ctx, e2 ctx) }
  | LT; tag = LCID; EQ; e = term; GT; AS; t = ty { fun ctx -> TmTag (tag, e ctx, t ctx) }
  ;
