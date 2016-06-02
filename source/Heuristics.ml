(* Quentin Carbonneaux - 2016 *)

open Types
open Graph
open Polynom
open Focus_Builder

module PSet = Set.Make(struct
  type t = Poly.t
  let compare = Poly.compare
end)

(* The heuristic to infer focus functions.
*)
let add_focus ai_results ai_get_nonneg gfunc =

  let assigns =
    Array.fold_left
      (List.fold_left begin fun l (a, _) ->
          match a with
          | AAssign (v, e) -> (v, Poly.of_expr e) :: l
          | _ -> l
        end)
      [] gfunc.fun_body.g_edges
  in
  (* Collect all conditions used by the program. *)
  let base =
    Hashtbl.fold begin fun _ abs_array base ->
      Array.fold_left begin fun base abs ->
        List.fold_left
          (fun b p -> PSet.add p b) base
          (ai_get_nonneg abs)
      end base abs_array
    end ai_results PSet.empty
  in
  (* Close them under all the program assignments. *)
  let close acc (v, pe) =
    let mv = Monom.of_var v in
    if abs_float (Poly.get_coeff mv pe) = 1. then acc else
    PSet.fold (fun p -> PSet.add (poly_subst v pe p))
      base acc
  in
  let base = List.fold_left close base assigns in
  (* Remove constants. *)
  let base =
    PSet.filter begin fun p ->
      match Poly.is_const p with
      | Some _ -> false
      | None -> true
    end base
  in

  if false then
  PSet.iter (fun p ->
    Format.eprintf " %a@." Poly.print p) base;

  let res =
    PSet.fold begin fun pol res ->
      max0_ge_0 pol ::
      max0_ge_arg pol ::
      max0_le_arg (check_ge pol (Poly.zero ())) ::
      res
    end base []
  in
  let fun_focus =
    List.rev_append
      gfunc.fun_focus
      (List.map export res)
  in { gfunc with fun_focus }


(* Place weakening points automatically.
   A good heuristic is to insert them at
   the end of guard edges.
*)
let add_weaken ({ fun_name; fun_body = g } as graphf) =
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
