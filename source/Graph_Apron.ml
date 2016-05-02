(* Quentin Carbonneaux - 2016 *)

(* Interface with the APRON library to compute
   logical invariants at all program points.
   We base that on Bertrand Jeannet's fixpoint
   library (in lib/).
*)

open Types
open Types.Graph

type transfer =
  | TGuard of logic
  | TAssign of id * expr
  | TCall of func * func * id list * expr list
  | TReturn of func * func * id list * expr list

module HyperGraph = struct

  (* The fixpoint library needs to represent programs
     using hypergraphs.  We are close enough with
     our Graph representation, but tweaks are needed
     for function call/returns.
  *)

  type vertex = id * int  (* pair of function name and node id *)
  type hedge = int

  let compare =
    { PSHGraph.hashv =
      { Hashhe.hash = (Hashtbl.hash: vertex -> int)
      ; Hashhe.equal = (==)
      }
    ; PSHGraph.hashh =
      { Hashhe.hash = abs
      ; Hashhe.equal = (==)
      }
    ; PSHGraph.comparev = compare
    ; PSHGraph.compareh = compare
    }

  let from_graph fl =
    let new_edge =
      let next_edge = ref (-1) in
      fun () -> incr next_edge; !next_edge in

    (* vertex:   type of vertices
       hedge:    type of hyper-edges
       unit:     data associated to the vertices
       transfer: data associated to the hyper-edges
       unit:     data associated to the whole graph
    *)
    let g: (vertex, hedge, unit, transfer, unit) PSHGraph.t =
      PSHGraph.create compare 3 () in

    (* Add all the vertices first. *)
    List.iter begin fun f ->
      for node = 0 to Array.length f.fun_body.g_edges - 1 do
        PSHGraph.add_vertex g (f.fun_name, node) ();
      done;
    end fl;

    (* Then add all the edges.
       It is trivial except for the ACall case.
    *)
    List.iter begin fun f ->
      Array.iteri begin fun src el ->
        List.iter begin fun (act, dst) ->
          let src = f.fun_name, src in
          let dst = f.fun_name, dst in
          match act with
          | AGuard log ->
            PSHGraph.add_hedge g (new_edge ())
              (TGuard log) ~pred:[|src|] ~succ:[|dst|];
          | AAssign (id, e) ->
            PSHGraph.add_hedge g (new_edge ())
              (TAssign (id, e)) ~pred:[|src|] ~succ:[|dst|];
          | ACall (idl, idf', el) ->
            let f' = List.find (fun f -> f.fun_name = idf') fl in
            PSHGraph.add_hedge g (new_edge ())
              (TCall (f, f', idl, el))
              ~pred:[|src|] ~succ:[|idf', f'.fun_body.g_start|];
            PSHGraph.add_hedge g (new_edge ())
              (TReturn (f, f', idl, el))
              ~pred:[|src; idf', f'.fun_body.g_end|] ~succ:[|dst|];
        end el;
      end f.fun_body.g_edges;
    end fl

end
