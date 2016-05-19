(* Quentin Carbonneaux - 2016 *)

open Types

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

type func = (Function.focus, graph) func_