{
open Parser
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
  | "Unit"            { TUNIT }
  | "Bool"            { TBOOL }
  | "Int"             { TINT }
  | "Float"           { TFLOAT }
  | "String"          { TSTRING }
  | "All"             { ALL }
  | "as"              { AS }
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
  | eof               { EOF }
