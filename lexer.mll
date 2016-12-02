{
open Parser

let string_buffer = ref (Bytes.create 2048)
let string_end = ref 0

let reset_str () = string_end := 0

let add_str ch =
  let x = !string_end in
  let buffer = !string_buffer in
  if x = String.length buffer then
    begin
      let new_buffer = Bytes.create (x * 2) in
      Bytes.blit buffer 0 new_buffer 0 x;
      Bytes.set new_buffer x ch;
      string_buffer := new_buffer;
      string_end := x + 1
    end
  else
    begin
      Bytes.set buffer x ch;
      string_end := x + 1
    end

let get_str () = String.sub (!string_buffer) 0 (!string_end)
}

let white = [' ' '\009' '\012']
let int = '-'?['0'-'9']+
let float = ['0'-'9']+'.'['0'-'9']*
let ucid = ['A'-'Z']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
let lcid = ['a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*

rule read =
  parse
  | white+            { read lexbuf }
  | white*("\r")?"\n" { read lexbuf }
  | int               { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOATV (float_of_string (Lexing.lexeme lexbuf)) }
  | "\""              { reset_str (); string lexbuf }
  | "Unit"            { TUNIT }
  | "Bool"            { TBOOL }
  | "Int"             { TINT }
  | "Float"           { TFLOAT }
  | "String"          { TSTRING }
  | "All"             { ALL }
  | "as"              { AS }
  | "lambda"          { LAMBDA }
  | "let"             { LET }
  | "in"              { IN }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "fix"             { FIX }
  | "unit"            { UNIT }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "fold"            { FOLD }
  | "unfold"          { UNFOLD }
  | "case"            { CASE }
  | "of"              { OF }
  | ucid              { UCID (Lexing.lexeme lexbuf) }
  | lcid              { LCID (Lexing.lexeme lexbuf) }
  | ";"               { SEMI }
  | "="               { EQ }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | "<"               { LT }
  | ">"               { GT }
  | ","               { COMMA }
  | ":"               { COLON }
  | "->"              { ARROW }
  | "."               { DOT }
  | "["               { LSQUARE }
  | "]"               { RSQUARE }
  | ".1"              { DOTONE }
  | ".2"              { DOTTWO }
  | "=>"              { DARROW }
  | "|"               { VBAR }
  | "{"               { LCURLY }
  | "}"               { RCURLY }
  | "+"               { ADD }
  | "=="              { DEQ }
  | eof               { EOF }
  | _                 { failwith "illegal character" }

and string =
  parse
  | '"'               { STRINGV (get_str ()) }
  | '\\'              { add_str (escaped lexbuf); string lexbuf }
  | eof               { failwith "string not terminated" }
  | _                 { add_str (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped =
  parse
  | 'n'               { '\n' }
  | 't'               { '\t' }
  | '\\'              { '\\' }
  | '"'               { '\034' }
  | '\''              { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
      {
        let x = int_of_string (Lexing.lexeme lexbuf) in
        if x > 255 then
          failwith "illegal character constant"
        else
          Char.chr x
      }
  | [^ '"' '\\' 't' 'n' '\'']
      {
        failwith "illegal character constant"
      }
