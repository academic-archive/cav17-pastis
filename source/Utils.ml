(* Quentin Carbonneaux - 2016-2017 *)

open Types

exception Error
exception Todo of string

let _TODO s = raise (Todo s)

let dummy_position =
  { pos_file = ""
  ; pos_line = -1
  ; pos_bol = -1
  ; pos_char = -1 }

let print_position fmt pos =
  Format.fprintf fmt "%s:%i:%i"
    pos.pos_file pos.pos_line
    (pos.pos_char - pos.pos_bol + 1)

let negate_cmp = function
  | Le -> Gt
  | Lt -> Ge
  | Ge -> Lt
  | Gt -> Le
  | Eq -> Ne
  | Ne -> Eq

let fresh_name =
  let cnt = ref 0 in
  fun () ->
    let n = "fresh." ^ string_of_int !cnt in
    incr cnt;
    n

let is_fresh n =
  String.length n >= 6 &&
  String.sub n 0 6 = "fresh."

let show_remove_png fpng =
  let viewers = [ "feh"; "display"; "open" ] in
  match Unix.fork () with
  | 0 ->
    List.iter begin fun prog ->
      let cmd = Printf.sprintf "%s %s >/dev/null 2>&1" prog fpng in
      if (try Sys.command cmd with Sys_error _ -> 1) = 0
      then (Sys.remove fpng; exit 0)
    end viewers;
    Printf.eprintf "Could open file '%s' for display.\n" fpng;
    exit 0
  | _ -> ()

let exit n = exit n
