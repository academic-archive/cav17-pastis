(* Quentin Carbonneaux - 2016 *)

open Types
open Polynom
open Focus_Builder

module PSet = Set.Make(struct
  type t = Poly.t
  let compare = Poly.compare
end)

(* The heuristic to infer focus functions. *)

let add_focus ai_results ai_get_nonneg gfunc =

  let assigns =
    Array.fold_left
      (List.fold_left (fun l (a, _) ->
        match a with
        | Graph.AAssign (v, e) -> (v, Poly.of_expr e) :: l
        | _ -> l))
      [] gfunc.fun_body.Graph.g_edges
  in

  (* Collect all conditions used by the program. *)
  let base =
    Hashtbl.fold (fun _ abs_array base ->
      Array.fold_left (fun base abs ->
        List.fold_left
          (fun b p -> PSet.add p b) base
          (ai_get_nonneg abs)
      ) base abs_array
    ) ai_results PSet.empty
  in

  (* Close them under all the program assignments. *)
  let close v pe acc =
    let mv = Monom.of_var v in
    if abs_float (Poly.get_coeff mv pe) = 1. then acc else
    PSet.fold (fun p -> PSet.add (poly_subst v pe p))
      base acc
  in
  let base =
    List.fold_left (fun base (v, e) -> close v e base)
      base assigns
  in

  (* Remove constants. *)
  let base =
    PSet.filter (fun p ->
      match Poly.is_const p with
      | Some _ -> false
      | None -> true
    ) base
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
