(* Quentin Carbonneaux - 2016 *)

(*
   a >= b                            when a >= b  (check)

   max(0, a) >= 0                                 (max0_ge_0)
   max(0, a) >= a                                 (max0_ge_arg)
   a >= max(0, a)                    when a >= 0  (max0_le_arg)
   max(0, a) >= max(0, b)            when a >= b  (max0_monotonic)
   max(0, b) + a - b >= max(0, a)    when a >= b  (max0_sublinear)

   binom(k, a) >= binom(k, b)        when a >= b  (binom_monotonic)
*)

open Polynom

module Builder
: sig
  type t
  val check: Poly.t -> Poly.t -> t
  val max0_ge_0: Poly.t -> t
  val max0_ge_arg: Poly.t -> t
  val max0_le_arg: t -> t
  val max0_monotonic: t -> t
  val max0_sublinear: t -> t
  val binom_monotonic: int -> t -> t
  val export: t -> (Poly.t * Poly.t list)
end
= struct

  type property =
    | Ge0 of Poly.t
    | Ge of Poly.t * Poly.t

  let prop_Ge0 = function
    | Ge0 p -> p
    | Ge (p1, p2) -> Poly.sub p1 p2

  let prop_Ge = function
    | Ge0 p -> p, Poly.zero ()
    | Ge (p1, p2) -> p1, p2

  (* Polynom helper functions. *)

  let poly_max p =
    Poly.of_monom (Monom.of_factor (Factor.Max p) 1) 1.

  let poly_binom k p =
    if k < 0 then failwith "invalid argument" else
    let rec fact k =
      if k <= 1 then 1. else
      fact (k-1) *. float_of_int k
    in
    let rec prod k accu =
      let k = k - 1 in
      if k < 0 then accu else
      let p_min_k = Poly.sub p (Poly.const (float_of_int k)) in
      prod k (Poly.mul p_min_k accu)
    in
    Poly.scale (fact k) (prod k (Poly.const 1.))

  (* Invariant function building blocks. *)

  type t = { proves: property; checks: property list }

  let check a b =
    { proves = Ge (a, b)
    ; checks = [Ge (a, b)] }

  let max0_ge_0 a =
    { proves = Ge0 (poly_max a)
    ; checks = [] }

  let max0_ge_arg a =
    { proves = Ge (poly_max a, Poly.zero ())
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

  let export i =
    (prop_Ge0 i.proves, List.map prop_Ge0 i.checks)

end
