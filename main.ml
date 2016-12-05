open Batteries
open Format
open Syntax
open Core

let parse_args () =
  let in_file = ref (None : string option) in
  Arg.parse []
    (fun s ->
       match !in_file with
       | Some _ -> failwith "you must specify exactly one input file"
       | None -> in_file := Some s)
    "";
  match !in_file with
  | Some s -> s
  | None -> failwith "you must specify an input file"

let parse_file in_file =
  let pi = open_in in_file in
  let lexbuf = Lexing.from_channel pi in
  let result =
    try Parser.top_level Lexer.read lexbuf
    with Parsing.Parse_error -> failwith "parse error"
  in
  Parsing.clear_parser (); close_in pi; result

let check_binding ctx bind =
  match bind with
  | NameBind -> NameBind
  | VarBind tyT -> VarBind tyT
  | TyVarBind kd -> TyVarBind kd
  | TyAbbBind (tyT, None) -> TyAbbBind (tyT, Some (kind_of ctx tyT))
  | TyAbbBind (tyT, Some kd) ->
    if kd = kind_of ctx tyT then TyAbbBind (tyT, Some kd) else failwith "kinds mismatch"
  | TmAbbBind (tm, None) -> TmAbbBind (tm, Some (type_of ctx tm))
  | TmAbbBind (tm, Some tyT) ->
    if type_eqv ctx tyT (type_of ctx tm) then TmAbbBind (tm, Some tyT) else failwith "types mismatch"

let string_of_binding_type ctx bind =
  match bind with
  | NameBind -> ""
  | VarBind tyT -> ": " ^ string_of_type ctx tyT
  | TyVarBind kd -> ":: " ^ string_of_kind ctx kd
  | TyAbbBind (tyT, opt) -> "= " ^ string_of_type ctx tyT ^ " :: " ^
                            (match opt with
                             | Some kd -> string_of_kind ctx kd
                             | None -> string_of_kind ctx (kind_of ctx tyT))
  | TmAbbBind (tm, opt) -> ": " ^
                           (match opt with
                            | Some tyT -> string_of_type ctx tyT
                            | None -> string_of_type ctx (type_of ctx tm))

let rec process_command ctx cmd =
  match cmd with
  | Eval tm ->
    let tyT = type_of ctx tm in
    print_endline (string_of_type ctx tyT);
    ctx
  | Bind (x, bind) ->
    let bind = check_binding ctx bind in
    print_endline (x ^ " " ^ string_of_binding_type ctx bind);
    add_binding ctx x bind

let process_file f ctx =
  let (cmds, _) = parse_file f ctx in
  let g ctx c =
    let results = process_command ctx c in
    print_flush ();
    results
  in
  List.fold_left g ctx cmds

let main () =
  let in_file = parse_args () in
  let _ = process_file in_file empty_ctx in
  ()

let () = main ()
