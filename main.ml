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

let rec process_command ctx cmd = ctx

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
