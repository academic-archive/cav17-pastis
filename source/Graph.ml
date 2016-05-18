(* Quentin Carbonneaux - 2016 *)

open Types
include Graph_Types

(* Construct a graph representation of the
   given IMP program.  The nodes of the graph
   are program points and the edges have
   actions on them.
*)
let from_imp impf =

  let h_position = Hashtbl.create 51 in
  let h_edges = Hashtbl.create 51 in

  let next_node = ref 0 in
  let new_node pos =
    let node = !next_node in
    Hashtbl.add h_position node pos;
    incr next_node;
    node
  in

  let new_edge src act dst =
    if dst < 0 then
      failwith "invalid input program";
    Hashtbl.add h_edges src (act, dst)
  in

  let rec goi fin loo pos = function
    | IMP.IBreak -> loo
    | IMP.IWeaken ->
      let beg = new_node pos in
      new_edge beg AWeaken fin;
      beg
    | IMP.IAssume log ->
      let beg = new_node pos in
      new_edge beg (AGuard log) fin;
      beg
    | IMP.IAssign (id, e) ->
      let beg = new_node pos in
      new_edge beg (AAssign (id, e)) fin;
      beg
    | IMP.IIf (log, bi, be) ->
      let beg = new_node pos in
      let begi = gob fin loo bi.IMP.b_body in
      let bege = gob fin loo be.IMP.b_body in
      new_edge beg (AGuard log) begi;
      new_edge beg (AGuard (LNot log)) bege;
      beg
    | IMP.IWhile (log, b) ->
      let jmp = new_node b.IMP.b_end_p in
      let begb = gob jmp fin b.IMP.b_body in
      new_edge jmp (AGuard log) begb;
      new_edge jmp (AGuard (LNot log)) fin;
      jmp
    | IMP.ILoop b ->
      let jmp = new_node b.IMP.b_end_p in
      let begb = gob jmp fin b.IMP.b_body in
      new_edge jmp (AGuard LTrue) begb;
      begb
    | IMP.ICall (idl, idf, el) ->
      let beg = new_node pos in
      new_edge beg (ACall (idl, idf, el)) fin;
      beg

  and gob fin loo = function
    | [] -> fin
    | (i, pos) :: b ->
      let fin = gob fin loo b in
      goi fin loo pos i
  in

  let g_end = new_node impf.fun_end_p in
  let g_start = gob g_end (-1) impf.fun_body.IMP.b_body in
  let g_position = Array.make !next_node impf.fun_end_p in
  let g_edges = Array.make !next_node [] in
  for i = 0 to !next_node - 1 do
    g_position.(i) <- Hashtbl.find h_position i;
    g_edges.(i) <- Hashtbl.find_all h_edges i;
  done;
  { fun_name = impf.fun_name
  ; fun_vars = impf.fun_vars
  ; fun_args = impf.fun_args
  ; fun_rets = impf.fun_rets
  ; fun_body = { g_start; g_end; g_edges; g_position }
  ; fun_start_p = impf.fun_start_p
  ; fun_end_p = impf.fun_end_p
  }

module AbsInt:
sig
  type absval
  val analyze: dump:bool -> func list -> id ->
               (string, absval array) Hashtbl.t
  val is_nonneg: absval -> expr -> bool
end
= Graph_Apron
