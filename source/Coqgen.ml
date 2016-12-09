open Types
open Graph_Types
open Presburger       
       
(* todo: "dump" is probably not quite the right word, what should it be called? *)
       
(* The first approximation, every guard edge is labelled with just LTrue. *)
let dump_edges' fmt s es =
  List.iter (fun (a,s') ->
	     Format.fprintf fmt "@[<hov>(%d,@,ANone,@,%d)@]::@," s s')
	    es		 

let dump_edges fmt es =
  Format.fprintf fmt "@[<hov>";
  Array.iteri (dump_edges' fmt) es;
  Format.fprintf fmt "nil";
  Format.fprintf fmt "@]"		 
		 
let dump_func_body fmt i g =
  Format.fprintf fmt "@[<h>Add LoadPath \"coq\".@.Require Import List.@.Require Import CFG.@]@.@.";
  Format.fprintf fmt "@[<v>Definition func%d : graph :=@," i; 
  Format.fprintf fmt "  {| g_start := %d;@," g.g_start;
  Format.fprintf fmt "     g_end := %d;@," g.g_end;
  Format.fprintf fmt "     g_edges := %a@," dump_edges g.g_edges;
  Format.fprintf fmt "  |}.@]"

let dump_func fmt i f =
  dump_func_body fmt i f.fun_body
 
		 
let dump_ai_func_bounds print_bound fmt bounds =
  Format.fprintf fmt "@[<v>";
  Array.iter (fun v -> Format.fprintf fmt "%a@," print_bound v)
	    bounds;
  Format.fprintf fmt "@]"
		 
let dump_ai_bounds print_bound fmt bounds =
  Format.fprintf fmt "@[<v>";
  Hashtbl.iter (fun funname b ->
		(*todo: print the correct function name*)
	        Format.fprintf fmt "the bounds for func0 are%, %a@,"
		  (dump_ai_func_bounds (print_bound (fun x -> "ID"^funname^"_"^x))) b)
	       bounds;
  Format.fprintf fmt "@]"
		 
let dump_graph fs print_bound ai_bounds = 
  let oc = open_out "generated_coq.v" in
  let fmt = Format.formatter_of_out_channel oc in
  List.iteri (dump_func fmt) fs;
  dump_ai_bounds print_bound fmt ai_bounds;
  Format.fprintf fmt "@.";
  close_out oc

