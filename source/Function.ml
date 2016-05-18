(* Quentin Carbonneaux - 2016 *)

(*
   a >= b                            when a >= b  (check_ge)

   max(0, a) >= 0                                 (max0_ge_0)
   max(0, a) >= a                                 (max0_ge_arg)
   a >= max(0, a)                    when a >= 0  (max0_le_arg)
   max(0, a) >= max(0, b)            when a >= b  (max0_monotonic)
   max(0, b) + a - b >= max(0, a)    when a >= b  (max0_sublinear)

   binom(k, a) >= binom(k, b)        when a >= b  (binom_monotonic)

   a * b >= c * d                    when a >= c  (product)
                                     and b >= d
*)

open Polynom

type focus = Types.expr list * Poly.t

module Builder
: sig
  type t
  val check_ge: Types.expr -> Types.expr -> t
  val one_ge_0: t
  val max0_ge_0: Poly.t -> t
  val max0_ge_arg: Poly.t -> t
  val max0_le_arg: t -> t
  val max0_monotonic: t -> t
  val max0_sublinear: t -> t
  val binom_monotonic: int -> t -> t
  val product: t -> t -> t
  val export: t -> focus
end
= struct

  type 'a property =
    | Ge0 of 'a
    | Ge of 'a * 'a

  let prop_Ge0 = function
    | Ge0 p -> p
    | Ge (p1, p2) -> Poly.sub p1 p2

  let prop_Ge = function
    | Ge0 p -> p, Poly.zero ()
    | Ge (p1, p2) -> p1, p2

  (* Polynom helper functions. *)

  let poly_binom k p =
    if k < 0 then failwith "invalid argument" else
    let rec prod k accu =
      let k = k - 1 in
      if k < 0 then accu else
      let p_min_k = Poly.sub p (Poly.const (float_of_int k)) in
      prod k (Poly.mul p_min_k accu)
    in prod k (Poly.const 1.)

  (* Invariant function building blocks. *)

  type t =
    { proves: Poly.t property
    ; checks: (Types.expr property) list }

  let check_ge a b =
    let pa = Poly.of_expr a in
    let pb = Poly.of_expr b in
    { proves = Ge (pa, pb)
    ; checks = [Ge (a, b)] }

  let one_ge_0 =
    { proves = Ge0 (Poly.const 1.)
    ; checks = [] }

  let max0_ge_0 a =
    { proves = Ge0 (poly_max a)
    ; checks = [] }

  let max0_ge_arg a =
    { proves = Ge (poly_max a, a)
    ; checks = [] }

  let max0_le_arg i =
    let a = prop_Ge0 i.proves in
    { proves = Ge (a, poly_max a)
    ; checks = i.checks }

  let max0_monotonic i =
    let a, b = prop_Ge i.proves in
    { proves = Ge (poly_max a, poly_max b)
    ; checks = i.checks }

  let max0_sublinear i =
    let a, b = prop_Ge i.proves in
    { proves = Ge (Poly.add (poly_max b) (Poly.sub a b), poly_max a)
    ; checks = i.checks }

  let binom_monotonic k i =
    let a, b = prop_Ge i.proves in
    { proves = Ge (poly_binom k a, poly_binom k b)
    ; checks = i.checks }

  let product i1 i2 =
    let a1, b1 = prop_Ge i1.proves in
    let a2, b2 = prop_Ge i2.proves in
    { proves = Ge (Poly.mul a1 a2, Poly.mul b1 b2)
    ; checks = i1.checks @ i2.checks }

  let export i =
    let mkexpr = function
      | Ge0 e -> e
      | Ge (e1, e2) -> Types.ESub (e1, e2) in
    (List.map mkexpr i.checks, prop_Ge0 i.proves)

end

let interpret fe =
  let open Types in
  let check_arity f args pos n =
    if List.length args <> n then begin
      Format.eprintf "%a: invalid number of arguments in %s (%d, got %d)@."
        Utils.print_position pos f n (List.length args);
      raise Utils.Error
    end in
  let arg_expr pos f args n =
    match List.nth args n with
    | FBase e -> e
    | _ ->
      Format.eprintf "%a: expression expected as argument %d of %s@."
        Utils.print_position pos n f;
      raise Utils.Error
  in
  let arg_num pos f args n =
    match List.nth args n with
    | FBase (ENum n) -> n
    | _ ->
      Format.eprintf "%a: number expected as argument %d of %s@."
        Utils.print_position pos n f;
      raise Utils.Error
  in
  let rec i = function
    | FBase e ->
      Builder.check_ge e (ENum 0)
    | FApply (">=", [FBase e1; FBase e2], _) ->
      Builder.check_ge e1 e2
    | FApply ("max0_ge_0" as f, args, pos) ->
      check_arity f args pos 1;
      let e = arg_expr pos f args 0 in
      Builder.max0_ge_0 (Poly.of_expr e)
    | FApply ("max0_ge_arg" as f, args, pos) ->
      check_arity f args pos 1;
      let e = arg_expr pos f args 0 in
      Builder.max0_ge_arg (Poly.of_expr e)
    | FApply ("max0_le_arg" as f, args, pos) ->
      check_arity f args pos 1;
      let f = arg_func pos f args 0 in
      Builder.max0_le_arg f
    | FApply ("max0_monotonic" as f, args, pos) ->
      check_arity f args pos 1;
      let f = arg_func pos f args 0 in
      Builder.max0_monotonic f
    | FApply ("max0_sublinear" as f, args, pos) ->
      check_arity f args pos 1;
      let f = arg_func pos f args 0 in
      Builder.max0_sublinear f
    | FApply ("binom_monotonic" as f, args, pos) ->
      check_arity f args pos 2;
      let n = arg_num pos f args 0 in
      let f = arg_func pos f args 1 in
      Builder.binom_monotonic n f
    | FApply ("product" as f, args, pos) ->
      check_arity f args pos 2;
      let f1 = arg_func pos f args 0 in
      let f2 = arg_func pos f args 1 in
      Builder.product f1 f2
    | FApply (f, _, pos) ->
      Format.eprintf "%a: unknown focus function '%s'@."
        Utils.print_position pos f;
      raise Utils.Error
  and arg_func _pos f args n =
    i (List.nth args n)
  in Builder.export (i fe)
