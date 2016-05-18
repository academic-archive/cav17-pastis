(* Quentin Carbonneaux - 2016 *)

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

let children = ref []

let show_remove_pdf fpdf =
  let viewers = [ "mupdf"; "xpdf"; "open" ] in
  match Unix.fork () with
  | 0 ->
    let finish () =
      Sys.remove fpdf;
      exit 0 in
    List.iter begin fun prog ->
      let cmd = Printf.sprintf "%s %s >/dev/null 2>&1" prog fpdf in
      if (try Sys.command cmd with Sys_error _ -> 1) = 0 then finish ()
    end viewers;
    Printf.eprintf "Could open file '%s' for display.\n" fpdf;
    finish ()
  | pid -> children := pid :: !children

let exit n =
  List.iter (fun pid -> ignore (Unix.waitpid [] pid)) !children;
  exit n
