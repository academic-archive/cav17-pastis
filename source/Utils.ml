(* Quentin Carbonneaux - 2016 *)

open Types

exception Error
exception Todo of string

let _TODO s = raise (Todo s)

let print_position fmt pos =
  Format.fprintf fmt "%s:%i:%i"
    pos.pos_file pos.pos_line
    (pos.pos_char - pos.pos_bol + 1)
