(* Quentin Carbonneaux - 2016 *)
{
open IMP_Grammar

type position =
  { pos_line: int
  ; pos_bol: int
  ; pos_char: int
  }

let kw = Hashtbl.create 23
let () =
  List.iter (fun (k,t) -> Hashtbl.add kw k t)
  [ "function", TFUNC
  ; "var", TVAR
  ; "true", TTRUE
  ; "false", TFALSE
  ; "random", TRANDOM
  ; "skip", TSKIP
  ; "while", TWHILE
  ; "do", TDO
  ; "loop", TLOOP
  ; "break", TBREAK
  ; "returns", TRETURNS
  ; "end", TEND
  ; "if", TIF
  ; "then", TTHEN
  ; "else", TELSE
  ; "not", TNOT
  ; "and", TAND
  ; "or", TOR
  ; "assume", TASSUME
  ]

let pos lexbuf =
  { pos_line = lexbuf.Lexing.pos_lnum
  ; pos_bol = lexbuf.Lexing.pos_bol
  ; pos_char = lexbuf.Lexing.pos_cnum
  }

let newline lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with Lexing.
      pos_lnum = pos.Lexing.pos_lnum + 1;
      pos_bol = pos.Lexing.pos_cnum;
    }

}

let first = ['a'-'z' 'A'-'Z' '_']
let ident = first (first | ['0'-'9'])*

rule token = parse
  | ['\r' ' ' '\t']+ { token lexbuf }
  | '\n'             { newline lexbuf; token lexbuf }
  | '#' ([^'\n']*)   { token lexbuf }
  | ['0'-'9']+       { TNUM (int_of_string (Lexing.lexeme lexbuf)) }
  | ";"              { TSEMI }
  | ","              { TCOMMA }
  | "("              { TLPAR }
  | ")"              { TRPAR }
  | "<="             { TLEQ }
  | "<"              { TLT }
  | ">="             { TGEQ }
  | ">"              { TGT }
  | "="              { TEQ }
  | "!="             { TNE }
  | "+"              { TADD }
  | "-"              { TSUB }
  | "*"              { TMUL }
  | ident            { let id = Lexing.lexeme lexbuf in
                       try Hashtbl.find kw id with Not_found -> TIDENT id }
  | eof              { TEOF }
