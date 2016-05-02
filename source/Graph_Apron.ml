(* Quentin Carbonneaux - 2016 *)

(* Interface with the APRON library to compute
   logical invariants at all program points.
   We base that on Bertrand Jeannet's fixpoint
   library (in lib/).
*)

open Types
open Types.Graph

module DNF = struct
  type 'a t = 'a list list
  let lift x = [[x]]
  let true_ = [[]]
  let false_ = []
  let disjunct = List.rev_append
  let rec conjunct a b =
    match a with
    | [] -> []
    | x :: a ->
      disjunct (List.map ((@) x) b) (conjunct a b)
end

module Translate = struct

  (* The logical conditions (type logic) and program expressions
     (type expr) need to be turned into what Apron understands.

     One difficulty is that abstract interpretation can only
     manipulate conjunctive states, so we use the DNF module
     to normalize all disjunctions at the top of logic
     expressions.
  *)

  module E = Apron.Texpr1
  module C = Apron.Tcons1

  let texpr_of_expr e: E.expr =
    let rec tr = function
      | ERandom -> E.Cst (Apron.Coeff.i_of_float neg_infinity infinity)
      | EVar id -> E.Var (Apron.Var.of_string id)
      | ENum n -> E.Cst (Apron.Coeff.Scalar (Apron.Scalar.Mpqf (Mpqf.of_int n)))
      | ESub (ENum 0, e) -> E.Unop (E.Neg, tr e, E.Int, E.Rnd)
      | EAdd (e1, e2) -> E.Binop (E.Add, tr e1, tr e2, E.Int, E.Rnd)
      | ESub (e1, e2) -> E.Binop (E.Sub, tr e1, tr e2, E.Int, E.Rnd)
      | EMul (e1, e2) -> E.Binop (E.Mul, tr e1, tr e2, E.Int, E.Rnd)
    in tr e

  let tcons_of_logic env l: C.earray list =
    let cmp e1 c e2 =
      let e1 = texpr_of_expr e1 in
      let e2 = texpr_of_expr e2 in
      C.make (E.of_expr env (E.Binop (E.Sub, e1, e2, E.Int, E.Rnd))) c
    in
    let rec tpos = function
      | LTrue | LRandom -> DNF.true_
      | LFalse -> DNF.false_
      | LLE (e1, e2) -> DNF.lift (cmp e2 C.SUPEQ e1)
      | LLT (e1, e2) -> DNF.lift (cmp e2 C.SUP e1)
      | LGE (e1, e2) -> DNF.lift (cmp e1 C.SUPEQ e2)
      | LGT (e1, e2) -> DNF.lift (cmp e1 C.SUP e2)
      | LEQ (e1, e2) -> DNF.lift (cmp e1 C.EQ e2)
      | LNE (e1, e2) -> DNF.lift (cmp e1 C.DISEQ e2)
      | LAnd (l1, l2) -> DNF.conjunct (tpos l1) (tpos l2)
      | LOr (l1, l2) -> DNF.disjunct (tpos l1) (tpos l2)
      | LNot l -> tneg l
    and tneg = function
      | LFalse | LRandom -> DNF.true_
      | LTrue -> DNF.false_
      | LGT (e1, e2) -> DNF.lift (cmp e2 C.SUPEQ e1)
      | LGE (e1, e2) -> DNF.lift (cmp e2 C.SUP e1)
      | LLT (e1, e2) -> DNF.lift (cmp e1 C.SUPEQ e2)
      | LLE (e1, e2) -> DNF.lift (cmp e1 C.SUP e2)
      | LNE (e1, e2) -> DNF.lift (cmp e1 C.EQ e2)
      | LEQ (e1, e2) -> DNF.lift (cmp e1 C.DISEQ e2)
      | LOr (l1, l2) -> DNF.conjunct (tneg l1) (tneg l2)
      | LAnd (l1, l2) -> DNF.disjunct (tneg l1) (tneg l2)
      | LNot l -> tpos l
    in
    let formula = List.filter ((<>) []) (tpos l) in
    List.map begin fun cnj ->
      let ea = C.array_make env (List.length cnj) in
      List.iteri (C.array_set ea) cnj;
      ea
    end formula

end

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
