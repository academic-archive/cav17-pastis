(* Quentin Carbonneaux - 2016 *)

open Types
include IMP_Types

let parse_file f =
  let ic =
    try open_in f with
    Sys_error _ ->
      Format.eprintf "cannot open file '%s'@." f;
      raise Utils.Error
  in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.
      pos_fname = f
    };
  try
    let l = IMP_Grammar.file IMP_Lexer.token lexbuf in
    List.map (fun f ->
      { f with fun_focus = List.map Focus.interpret f.fun_focus } ) l
  with Parsing.Parse_error ->
    let startp = Lexing.lexeme_start_p lexbuf in
    Format.eprintf "%s:%i:%i: syntax error near '%s'@."
      startp.Lexing.pos_fname
      startp.Lexing.pos_lnum
      (startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1)
      (Lexing.lexeme lexbuf);
    raise Utils.Error
