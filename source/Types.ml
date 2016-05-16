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

module IMP = struct

  type instr =
    | IBreak
    | IWeaken
    | IAssume of logic
    | IAssign of id * expr
    | IIf of logic * block * block
    | IWhile of logic * block
    | ILoop of block
    | ICall of id list * id * expr list

  and block =
    { b_start_p: position
    ; b_end_p: position
    ; b_body: (instr * position) list
    }

  type func = block func_

end

module Graph = struct

  type node = int

  type action =
    | AWeaken
    | AGuard of logic
    | AAssign of id * expr
    | ACall of id list * id * expr list

  type graph =
    { g_start: node
    ; g_end: node
    ; g_edges: ((action * node) list) array
    ; g_position: position array
    }

  type func = graph func_

end
