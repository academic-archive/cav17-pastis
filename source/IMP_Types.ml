(* Quentin Carbonneaux - 2016 *)

open Types

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

type func = (Function.focus, block) func_
