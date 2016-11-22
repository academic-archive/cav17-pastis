(* Quentin Carbonneaux - 2016 *)

open Types

type node = int

type action =
  | ANone
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

type func = (Focus.focus, graph) func_
