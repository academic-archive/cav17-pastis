(* Quentin Carbonneaux - 2016 *)

open Types
include Graph_Types

type stats =
  { mutable weaken_map: (id * int) list }

let stats =
  { weaken_map = [] }

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
  { impf with fun_body = { g_start; g_end; g_edges; g_position } }

(* Place weakening points automatically.
   A good heuristic is to insert them at
   the end of guard edges.
*)
let auto_weaken ({ fun_name; fun_body = g } as graphf) =

  let is_guard = function
    | AGuard _ -> true
    | _ -> false
  in

  let weaken = ref [] in
  let nweaken = ref 0 in
  let add_weaken =
    let nnodes = Array.length g.g_edges in
    fun dst ->
      weaken := [AWeaken, dst] :: !weaken;
      incr nweaken;
      !nweaken - 1 + nnodes
  in

  let new_edges =
    Array.mapi begin fun src ->
      List.map begin function
        | act, dst when is_guard act ->
          let d_edges = g.g_edges.(dst) in
          if d_edges = []
          || not (List.for_all (fun (a, _) -> is_guard a) d_edges)
          then (act, add_weaken dst)
          else (act, dst)
        | e -> e
      end
    end g.g_edges
  in

  let weaken = Array.of_list (List.rev !weaken) in
  let new_position =
    Array.map begin function
      | [_, dst] -> g.g_position.(dst)
      | _ -> assert false
    end weaken
  in
  let fun_body =
    { g with
      g_edges = Array.append new_edges weaken;
      g_position = Array.append g.g_position new_position;
    } in
  stats.weaken_map <- (fun_name, !nweaken) :: stats.weaken_map;
  { graphf with fun_body }

module type AbsInt = sig
  type absval
  val analyze: dump:bool -> func list -> id ->
               (string, absval array) Hashtbl.t
  val is_nonneg: absval -> expr -> bool
  val get_nonneg: absval -> Polynom.Poly.t list
end

module AbsInt = struct
  module Apron: AbsInt = Graph_AI_Apron
  module Simple: AbsInt = Graph_AI_Simple
end
