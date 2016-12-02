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

%start <Syntax.context -> (Syntax.command list * Syntax.context)> top_level

%%

top_level: top = rev_top_level { fun ctx -> let (cmds, ctx) = top ctx in (List.rev cmds, ctx) };

rev_top_level:
  | (* empty *) { fun ctx -> ([], ctx) }
  | top = rev_top_level; SEMI; cmd = command
    { fun ctx ->
      let (cmds, ctx) = top ctx in
      let (cmd, ctx) = cmd ctx in
      (cmd :: cmds, ctx) }
  ;

command:
  | t = term { fun ctx -> (Eval (t ctx), ctx) }
  | x = LCID; EQ; t = term { fun ctx -> (Bind (x, TmAbbBind (t ctx, None)), add_name ctx x) }
  | x = UCID; EQ; t = ty { fun ctx -> (Bind (x, TyAbbBind (t ctx)), add_name ctx x) }
  ;

atom_type:
  | LPAREN; t = ty; RPAREN { t }
  | x = UCID
    { fun ctx -> TyVar (name_to_index ctx x, ctx_length ctx) }
  | TUNIT { fun ctx -> TyUnit }
  | TBOOL { fun ctx -> TyBool }
  | TINT { fun ctx -> TyInt }
  | TFLOAT { fun ctx -> TyFloat }
  | TSTRING { fun ctx -> TyString }
  | LT; ts = separated_list(COMMA, field_type); GT { fun ctx -> TyVariant (List.map (fun t -> t ctx) ts) }
  ;

field_type:
  | x = LCID; COLON; t = ty { fun ctx -> (x, t ctx) }
  ;

arrow_type:
  | t1 = atom_type; ARROW; t2 = arrow_type { fun ctx -> TyArrow (t1 ctx, t2 ctx) }
  | t = atom_type { t }
  ;

ty:
  | t = arrow_type { t }
  | ALL; x = UCID; DOT; t = ty
    { fun ctx -> let ctx' = add_name ctx x in TyAll (x, t ctx') }
  ;

term:
  | t = app_term { t }
  ;

app_term:
  | t = path_term { t }
  ;

path_term:
  | t = ascribe_term { t }
  ;

ascribe_term:
  | t1 = atom_term; AS; t2 = ty { fun ctx -> TmAscribe (t1 ctx, t2 ctx) }
  | t = atom_term { t }
  ;

atom_term:
  | LPAREN; t = term; RPAREN { t }
  ;
