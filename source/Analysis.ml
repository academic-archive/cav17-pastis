(* Quentin Carbonneaux - 2016 *)

open Graph
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
  val of_poly: Poly.t -> annot
  val exec_assignment: (id * expr) -> annot -> annot
  val constrain_eq: annot -> annot -> unit
  val rewrite: Poly.t list -> annot -> annot
  val solve_min: Poly.t list -> annot -> Poly.t option
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

  let add_poly le =
    Poly.fold
      begin fun monom k new_annot ->
        let le_old =
          try M.find monom new_annot
          with Not_found -> new_linexpr ()
        in
        linexpr_addmul le_old k le;
        M.add monom le_old new_annot
      end

  let exec_assignment (v, e) annot =
    match e with
    | ERandom -> Utils._TODO "non-deterministic assignments"
    | e ->
      let e = Poly.of_expr e in
      M.fold begin fun monom le ->
        add_poly le (monom_subst v e monom)
      end annot M.empty

  (* Constrain the linear expression le to be
     non-negative if ge is true and zero if ge
     is false.
  *)
  let add_lprow ?k:(k=0.) ge le =
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
        { Clp.row_lower = k
        ; Clp.row_upper = if ge then infinity else k
        ; Clp.row_elements = Array.of_list l
        };
    end

  let of_poly p =
    Poly.fold begin fun monom k annot ->
      let v = new_lpvar () in
      let le = new_linexpr () in
      Hashtbl.add le v 1.;
      add_lprow ~k false le;
      M.add monom le annot
    end p M.empty

  (* Constrain annot1 = annot2. *)
  let constrain_eq annot1 annot2 =
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

  let expand l pl =
    let le = new_linexpr () in
    (* Compute Σ(kᵢ * plᵢ) as an annotation. *)
    let plsum, kpl =
      List.fold_left begin fun (plsum, kpl) p ->
        let kp = new_lpvar () in
        Hashtbl.clear le;
        Hashtbl.add le kp 1.;
        (add_poly le p plsum, kp :: kpl)
      end (M.empty, []) pl in
    (* Add plsum and l (in plsum). *)
    let plsum =
      M.merge begin fun _ leo vo ->
        let le =
          match leo with
          | Some le -> le
          | None -> new_linexpr ()
        in
        match vo with
        | Some v -> Hashtbl.add le v 1.; Some le
        | None -> failwith "impossible"
      end plsum l in
    (plsum, kpl)

  (* Returns annot' such that  annot' >= annot, using a
     list of non-negative polynomials.  If the polynomials
     are actually null, the returned potential has the
     same value as the passed one.
  *)
  let rewrite pl annot =
    let l = M.map (fun _ -> new_lpvar ()) annot in
    let exannot, kpl = expand l pl in
    constrain_eq exannot annot;
    let annot', kpl' = expand l pl in
    let le = new_linexpr () in
    List.iter2 begin fun kp1 kp2 ->
      Hashtbl.clear le;
      assert (kp1 <> kp2);
      Hashtbl.add le kp1 (+1.);
      Hashtbl.add le kp2 (-1.);
      add_lprow true le;
    end kpl kpl';
    annot'

  let solve_min pl annot =
    let absl = ref [] in
    let l = M.map begin fun _ ->
        let v = new_lpvar () and abs = new_lpvar () in
        List.iter Clp.add_row
          [ { Clp.row_lower = 0.; row_upper = infinity
            ; row_elements = [| abs, 1.; v, +1. |] }
          ; { Clp.row_lower = 0.; row_upper = infinity
            ; row_elements = [| abs, 1.; v, -1. |] }
          ];
        absl := abs :: !absl;
        v
      end annot in
    let _, kpl = expand l pl in
    let obj = Clp.objective_coefficients () in
    List.iter (fun k -> obj.(k) <- 1.) kpl;
    List.iter (fun k -> obj.(k) <- 1.) !absl;
    Clp.change_objective_coefficients obj;
    Clp.initial_solve ();
    match Clp.status () with
    | 0 ->
      let sol = Clp.primal_column_solution () in
      Some (M.fold begin fun m le poly ->
          let k =
            Hashtbl.fold begin fun v kv k ->
              k +. kv *. sol.(v)
            end le 0.
          in Poly.add_monom m k poly
        end annot (Poly.zero ())
      )
    | _ -> None

end

let run ai_results focus fl start query =

  let f = List.find (fun f -> f.fun_name = start) fl in
  let body = f.fun_body in

  let monoms =
    List.fold_left begin fun monoms (_, p) ->
      Poly.fold (fun m _ ms -> m :: ms) p monoms
    end [] focus
  in

  (* Find all the non-negative focus functions at a
     given program point.
  *)
  let find_focus f node =
    let ai = Hashtbl.find ai_results f in
    let is_nonneg = AbsInt.is_nonneg ai.(node) in
    focus |>
      List.filter (fun (chk, _) -> List.for_all is_nonneg chk) |>
      List.map snd
  in

  (* Create a new potential annotation resulting from
     executing one action (backwards).
  *)
  let do_action node act a =
    match act with
    | Graph.AWeaken -> Potential.rewrite (find_focus start node) a
    | Graph.AGuard _ -> a
    | Graph.AAssign (v, e) -> Potential.exec_assignment (v, e) a
    | Graph.ACall _ -> Utils._TODO "calls"
  in

  (* Annotate all program points starting from
     a given node.  The potential at the end of
     the function is set to query.

     We follow a depth first-search on the tree
     and update annotations for nodes lazily, this
     allows to reduce the number of LP variables.
  *)
  let annot = Array.map (fun _ -> `Todo) body.Graph.g_position in
  let rec dfs node =
    match annot.(node) with
    | `Done a -> a
    | `Doing ->
      let a = Potential.new_annot monoms in
      annot.(node) <- `Done a;
      a
    | `Todo ->
      annot.(node) <- `Doing;
      let a =
        match body.Graph.g_edges.(node) with
        | [] ->
          if node <> body.Graph.g_end then Utils._TODO "mmh?";
          Potential.of_poly query
        | (act, node') :: edges ->
          let annot = do_action node act (dfs node') in
          List.fold_left begin fun annot (act, node') ->
            let annot' = do_action node act (dfs node') in
            Potential.constrain_eq annot annot';
            annot
          end annot edges
      in
      begin match annot.(node) with
      | `Doing -> ()
      | `Done a' -> Potential.constrain_eq a a'
      | `Todo -> failwith "unreachable"
      end;
      annot.(node) <- `Done a;
      a
  in

  let start_node = body.Graph.g_start in
  let start_annot = dfs start_node in
  Potential.solve_min (find_focus start start_node) start_annot
