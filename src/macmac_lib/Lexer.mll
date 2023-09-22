{
open Lexing
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'

let drop_str n s = 
  let len = String.length s in
    String.sub s n (len - n)
let drop_str_r n s = 
  let len = String.length s in
    String.sub s 0 (len - n)
}

let digit = ['0'-'9']
let sign = ['-' '+']
let exponent = ['e' 'E']
let alpha = ['a'-'z' 'A'-'Z']

let float_constant = sign? digit+ ('.' digit+)? (exponent sign? digit+)?

let id_head = (alpha | '_' | '+' | '-' | '*' | '/' | '!' | '#' | '$' | '%' | '^' | '&' | '<' | '=' | '>' )
let identifier = 
  id_head (id_head | digit)*

let keyword = ':' (alpha | digit) +

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)

rule token = parse
  | float_constant { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | keyword { KEYWORD (drop_str 1 (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }

  | "true" { BOOL true }
  | "false" { BOOL false }
  | "nil" { NIL }
  | "()" { NIL }

  (* primitive types *)
  (* | "Int" { INT_T }*)
  (* | "Str" { STR_T }*)
  (* | "F64" { F64_T }*)
  (* | "Type" { TYPE_T }*)
  (* | "Keyword" { KEYWORD_T }*)

  (*This is for disambiguiate, as we allow arbitrary sequence of expressions, and
    `ID (..)` is a tuple followed by an ID, while `ID(..)` is a call *)
  | identifier { IDENT (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 32) lexbuf }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline { token lexbuf } (* just ignore *)
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf } 
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf } 
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf } 
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }
