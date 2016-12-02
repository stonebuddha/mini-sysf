open Batteries
open Format
open Syntax

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
  print_endline "Hello World!"

let () = main ()
