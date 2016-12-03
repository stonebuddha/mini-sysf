{
open Parser
}

let white = [' ' '\009' '\012']
let int = ['0'-'9']+
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
  | "Rec"             { REC }
  | "return"          { RETURN }
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
  | "=>"              { DARROW }
  | "|"               { VBAR }
  | "{"               { LCURLY }
  | "}"               { RCURLY }
  | "+"               { ADD }
  | "=="              { EQEQ }
  | "::"              { COLONCOLON }
  | "*"               { STAR }
  | "-"               { DIFF }
  | "/"               { DIV }
  | "<>"              { LTGT }
  | "<=?"             { LEQ }
  | "<?"              { LTQ }
  | ">=?"             { GEQ }
  | ">?"              { GTQ }
  | eof               { EOF }
  | _                 { failwith "illegal character" }
