(* Quentin Carbonneaux - 2016 *)

type id = string

type position =
  { pos_file: string
  ; pos_line: int
  ; pos_bol: int
  ; pos_char: int
  }

type 'a func_ =
  { fun_name: id
  ; fun_vars: id list
  ; fun_args: id list
  ; fun_rets: id list
  ; fun_body: 'a
  ; fun_start_p: position
  ; fun_end_p: position
  }

type expr =
  | ERandom
  | EVar of id
  | ENum of int
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr

type logic =
  | LTrue
  | LFalse
  | LRandom
  | LLE of expr * expr
  | LLT of expr * expr
  | LGE of expr * expr
  | LGT of expr * expr
  | LEQ of expr * expr
  | LNE of expr * expr
  | LAnd of logic * logic
  | LOr of logic * logic
  | LNot of logic
