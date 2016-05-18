(* Quentin Carbonneaux - 2016 *)
{
open IMP_Grammar

let kw = Hashtbl.create 23
let kwp = Hashtbl.create 23
let () =
  List.iter (fun (k,t) -> Hashtbl.add kw k t)
  [ "var", TVAR
  ; "true", TTRUE
  ; "false", TFALSE
  ; "random", TRANDOM
  ; "skip", TSKIP
  ; "returns", TRETURNS
  ; "not", TNOT
  ; "and", TAND
  ; "or", TOR
  ; "focus", TFOCUS
  ];
  List.iter (fun (k,t) -> Hashtbl.add kwp k t)
  [ "break", (fun p -> TBREAK p)
  ; "assume", (fun p -> TASSUME p)
  ; "if", (fun p -> TIF p)
  ; "while", (fun p -> TWHILE p)
  ; "loop", (fun p -> TLOOP p)
  ; "function", (fun p -> TFUNC p)
  ; "end", (fun p -> TEND p)
  ; "then", (fun p -> TTHEN p)
  ; "else", (fun p -> TELSE p)
  ; "do", (fun p -> TDO p)
  ; "weaken", (fun p -> TWEAK p)
  ]

let pos { Lexing.lex_start_p = start; _ } =
  { Types.pos_file = start.Lexing.pos_fname
  ; pos_line = start.Lexing.pos_lnum
  ; pos_bol = start.Lexing.pos_bol
  ; pos_char = start.Lexing.pos_cnum
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
  | "("              { TLPAR (pos lexbuf) }
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
                       let p = pos lexbuf in
                       try Hashtbl.find kw id with Not_found ->
                       try Hashtbl.find kwp id p with Not_found ->
                       TIDENT (id, p) }
  | eof              { TEOF }
