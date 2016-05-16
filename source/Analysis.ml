(* Quentin Carbonneaux - 2016 *)

open Types
open Polynom

type stats =
  { mutable num_lpvars: int
  ; mutable num_constraints: int
  }

let stats =
  { num_lpvars = 0
  ; num_constraints = 0
  }

let reset_stats () = begin
    stats.num_lpvars <- 0;
    stats.num_constraints <- 0;
  end

module Potential
: sig
  type annot
  val new_annot: Monom.t list -> annot
  val exec_assignment: (id * expr) -> annot -> annot
  val constrain: annot -> annot -> unit
  val constrain_with: Poly.t list -> annot -> annot -> unit
end
= struct

  (* This is the core of the analysis.
     We define what potential annotations are,
     and how they are:

       1. changed by assignments,
       2. constrained between each other (=, or >=).

  *)

  (* LP variables are simple integers. *)
  type lpvar = int

  (* To generate few LP constraints, our potential
     does not map monomials to LP variables directly
     but to linear combinations of those.
     A linear combination is represented as a hash
     table mapping LP variables to their coefficient
     in the linear sum.
  *)
  module M = Map.Make(Monom)
  type linexpr = (lpvar, float) Hashtbl.t
  type annot = linexpr M.t

  let new_lpvar () =
    stats.num_lpvars <- stats.num_lpvars + 1;
    Clp.add_column
      { Clp.column_obj = 0.
      ; Clp.column_lower = neg_infinity
      ; Clp.column_upper = infinity
      ; Clp.column_elements = [| |]
      };
    Clp.number_columns () - 1

  let new_linexpr () =
    Hashtbl.create 5

  let new_annot monoms =
    List.fold_left begin fun annot m ->
      let le = new_linexpr () in
      let v = new_lpvar () in
      Hashtbl.add le v 1.;
      M.add m le annot
    end M.empty monoms

  (* Performs: le2 += k * le1 *)
  let linexpr_addmul le2 k le1 =
    Hashtbl.iter begin fun v kv1 ->
      let kv2 = try Hashtbl.find le2 v with Not_found -> 0. in
      Hashtbl.replace le2 v (kv2 +. k *. kv1)
    end le1

  let exec_assignment (v, e) annot =
    match e with
    | ERandom -> Utils._TODO "non-deterministic assignments"
    | e ->
      let e = Poly.of_expr e in
      (* For all monoms of annot: *)
      M.fold begin fun monom le ->
        (* 1. Perform the assignment on the monom. *)
        monom_subst v e monom |>
        (* 2. Update coefficients of the new annotation. *)
        Poly.fold
          begin fun monom k new_annot ->
            let le_old =
              try M.find monom new_annot
              with Not_found -> new_linexpr ()
            in
            linexpr_addmul le_old k le;
            M.add monom le_old new_annot
          end
      end annot M.empty

  (* Constrain the linear expression le to be
     non-negative if ge is true and zero if ge
     is false.
  *)
  let add_lprow ge le =
    let l =
      Hashtbl.fold begin fun v kv l ->
        if abs_float kv <= fsmall
          then l
          else (v, kv) :: l
      end le []
    in
    if l <> [] then begin
      stats.num_constraints <- stats.num_constraints + 1;
      Clp.add_row
        { Clp.row_lower = 0.
        ; Clp.row_upper = if ge then infinity else 0.
        ; Clp.row_elements = Array.of_list l
        };
    end

  (* Enforce annot1 = annot2. *)
  let constrain annot1 annot2 =
    begin
      (* Constrain common coefficients. *)
      M.iter begin fun m le1 ->
        let le = Hashtbl.copy le1 in
        begin
          try linexpr_addmul le (-1.) (M.find m annot2)
          with Not_found -> ()
        end;
        add_lprow false le
      end annot1;
      (* Set the coefficients in annot2 only to 0. *)
      M.iter begin fun m le2 ->
        if not (M.mem m annot1) then
          add_lprow false le2;
      end annot2;
    end

  let constrain_with _ = Utils._TODO "contraints on annotations"

end
