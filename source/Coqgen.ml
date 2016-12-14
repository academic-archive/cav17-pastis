open Types
open Graph_Types
open Presburger

(* todo: "dump" is probably not quite the right word, what should it be called? *)

let mkvarname funname x = "ID"^funname^"_"^x

(* Assumes that we have a state s in the context. *)
let rec dump_expr varname fmt  = function
    ERandom -> Format.fprintf fmt "sorry, I don't know how to handle random."
  | EVar x  -> Format.fprintf fmt "(EVar %s)" (varname x)
  | ENum n  -> Format.fprintf fmt "(ENum %d)" n
  | EAdd (e1, e2) -> Format.fprintf fmt "(EAdd %a@ %a)" (dump_expr varname) e1 (dump_expr varname) e2
  | ESub (e1, e2) -> Format.fprintf fmt "(ESub %a@ %a)" (dump_expr varname) e1 (dump_expr varname) e2
  | EMul (e1, e2) -> Format.fprintf fmt "(EMul %a@ %a)" (dump_expr varname) e1 (dump_expr varname) e2

(* Assumes that we have a state s in the context. *)
let rec dump_logic varname fmt = function
    LTrue -> Format.fprintf fmt "True"
  | LFalse -> Format.fprintf fmt "False"
  | LRandom -> Format.fprintf fmt "sorry, I don't know how to handle random."
  | LCmp(e1,cmp,e2) -> Format.fprintf fmt "((eval %a@ s) %s@ (eval %a@ s))%%Z"
				      (dump_expr varname) e1
				      (match cmp with Le -> "<=" | Lt -> "<" | Ge -> ">=" | Gt -> ">" | Eq -> "=" | Ne -> "<>")
				      (dump_expr varname) e2
  | LAnd(l1,l2) -> Format.fprintf fmt "(%a@ /\\ %a)" (dump_logic varname) l1 (dump_logic varname) l2
  | LOr(l1,l2)  -> Format.fprintf fmt "(%a@ \\/ %a)" (dump_logic varname) l1 (dump_logic varname) l2
  | LNot l -> Format.fprintf fmt "(~ %a)" (dump_logic varname) l

let dump_action varname fmt = function
  | ANone -> Format.fprintf fmt "ANone"
  | AWeaken -> Format.fprintf fmt "AWeaken"
  | AGuard a -> Format.fprintf fmt "(AGuard@ (fun s => %a))" (dump_logic varname) a
  | AAssign(x, e) -> Format.fprintf fmt "(AAssign@ %s@ %a)" (varname x) (dump_expr varname) e
  | ACall  (xs,y,es) -> Format.fprintf fmt "TODO: implement ACall"

let dump_edges' varname fmt s es =
  List.iter (fun (a,s') ->
	     Format.fprintf fmt "@[<hov>(%d,@,%a,@,%d)@]::@," s (dump_action varname) a s')
	    es

let dump_edges varname fmt es =
  Format.fprintf fmt "@[<hov>";
  Array.iteri (dump_edges' varname fmt) es;
  Format.fprintf fmt "nil";
  Format.fprintf fmt "@]"

let dump_func fmt i f =
  let varname = (mkvarname f.fun_name) in
  Format.fprintf fmt "@[<h>Add LoadPath \"coq\".@.";
  Format.fprintf fmt "Require Import List.@.";
  Format.fprintf fmt "Require Import ZArith.@.";
  Format.fprintf fmt "Require Import Arith.@.";
  Format.fprintf fmt "Require Import CFG.@.";
  Format.fprintf fmt "\n\nOpaque Zplus.\nOpaque Zmult.@]@.@.";

  Format.fprintf fmt "@[<v>";
  List.iteri (fun i x -> Format.fprintf fmt "Notation %s := %d.@," (varname x) i) f.fun_vars;
  Format.fprintf fmt "@]";

  Format.fprintf fmt "@[<v>Definition func%d : graph :=@," i;
  Format.fprintf fmt "  {| g_start := %d;@," f.fun_body.g_start;
  Format.fprintf fmt "     g_end := %d;@," f.fun_body.g_end;
  Format.fprintf fmt "     g_edges := %a@," (dump_edges varname) f.fun_body.g_edges;
  Format.fprintf fmt "  |}.@]"

let dump_ai_func_bounds print_bound fmt bounds =
  Format.fprintf fmt "@[<v>";
  Array.iteri (fun i v -> Format.fprintf fmt "| %d => %a@," i print_bound v)
	      bounds;
  Format.fprintf fmt "| _ => False@,";
  Format.fprintf fmt "@]"

let dump_ai_bounds print_bound fmt bounds =
  Format.fprintf fmt "@[<v>";
  Hashtbl.iter (fun funname b ->
		(*todo: print the correct function name*)
	        Format.fprintf fmt "Definition func0_bounds (p : node) (s : state) := @,";
		Format.fprintf fmt "  match p with@,";
		Format.fprintf fmt "    @[<v4>%a@]@," (dump_ai_func_bounds (print_bound (mkvarname funname))) b)
     	                                              bounds;
  Format.fprintf fmt "  end.@,";
  Format.fprintf fmt "Theorem func0_bounds_corrects : forall s p' s', steps (g_start func0) s func0 p' s' -> func0_bounds p' s'.@,";
  Format.fprintf fmt "Proof. prove_ai_bounds_correct. Qed.";
  Format.fprintf fmt "@]"

let dump fs print_bound ai_bounds _annot =
  let oc = open_out "generated_coq.v" in
  let fmt = Format.formatter_of_out_channel oc in
  List.iteri (dump_func fmt) fs;
  Format.fprintf fmt "@,@,@,";
  dump_ai_bounds print_bound fmt ai_bounds;
  Format.fprintf fmt "@.";
  close_out oc
