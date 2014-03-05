{
  open Wparser
  open Lexing
  open Printf

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | digit+ as num       { INUM (int_of_string num) }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "while"             { WHILE }
  | ":="                { ASSGN }
  | "do"                { DO }
  | "done"              { DONE }
  | "begin"             { BEGIN }
  | '{'                 { BEGIN }
  | '}'                 { END }
  | "end"               { END }
  | "proc"              { PROC }
  | "write"             { WRITE }
  | "read"              { READ }
  | "is"                { IS }
  | "var"               { VAR }
  | "skip"              { SKIP }
  | ';'                 { CONS }
  | "call"              { CALL }
  | '='                 { EQ }
  | "<="                { LEQ }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "raise"             { RAISE }
  | "try"               { TRY }
  | "rescue"            { RESCUE }
  | "with"              { WITH }
  | '!'                 { NOT }
  | '&'                 { AND }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { MULT }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '#' [^ '\n']* '\n'  { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']+         { token lexbuf }
  | '\n'                { incr_linenum lexbuf; token lexbuf }
  | id as word          { ID word }
  | _ as c              {
      printf "Lexer error: Unrecognized character: %c\n" c;
      token lexbuf
    }
  | eof                 { EOF }