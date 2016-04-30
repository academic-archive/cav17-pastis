(* Quentin Carbonneaux - 2016 *)

module IMP = struct

  type id = string

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

  type position =
    { pos_file: string
    ; pos_line: int
    ; pos_bol: int
    ; pos_char: int
    }

  type instr =
    | IBreak
    | IAssume of logic
    | IAssign of id * expr
    | IIf of logic * block
    | IIfElse of logic * block * block
    | IWhile of logic * block
    | ILoop of block
    | ICall of id list * id * expr list

  and block = (instr * position) list

  type func =
    { fun_name: id
    ; fun_vars: id list
    ; fun_args: id list
    ; fun_rets: id list
    ; fun_body: block
    }

end
