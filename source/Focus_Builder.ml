(* Quentin Carbonneaux - 2016 *)

open Polynom

(* Polynom helper functions. *)

let poly_binom k p =
  if k < 0 then failwith "invalid argument" else
  let rec prod k accu =
    let k = k - 1 in
    if k < 0 then accu else
    let p_min_k = Poly.sub p (Poly.const (float_of_int k)) in
    prod k (Poly.mul p_min_k accu)
  in prod k (Poly.const 1.)

(* Focus functions can be build incrementally by
   composing smaller lemmas defined below.
*)

type 'a property =
  | Ge0 of 'a        (* a >= 0 *)
  | Ge of 'a * 'a    (* a >= b *)

let prop_Ge0 = function
  | Ge0 p -> p
  | Ge (p1, p2) -> Poly.sub p1 p2

let prop_Ge = function
  | Ge0 p -> p, Poly.zero ()
  | Ge (p1, p2) -> p1, p2

type t =
  { proves: Poly.t property
  ; checks: Types.expr list }

let export i =
  (i.checks, prop_Ge0 i.proves)

(* Focus function building blocks:

   a >= b                            when a >= b  (check_ge)

   max(0, a) >= 0                                 (max0_ge_0)
   max(0, a) >= a                                 (max0_ge_arg)
   a >= max(0, a)                    when a >= 0  (max0_le_arg)
   max(0, a) >= max(0, b)            when a >= b  (max0_monotonic)
   max(0, b) + a - b >= max(0, a)    when a >= b  (max0_sublinear)
   a * max(0, x) >= max(0, a * x)    when a >= 0  (max0_sublinear_mul)

   binom(k, a) >= binom(k, b)        when a >= b  (binom_monotonic)

   x * a >= x * b                    when a >= b  (product)
                                     and x >= 0
*)

let check_ge a b =
  { proves = Ge (Poly.of_expr a, Poly.of_expr b)
  ; checks = [Types.ESub (a, b)] }

let max0_ge_0 a =
  let a = Poly.of_expr a in
  { proves = Ge0 (poly_max a)
  ; checks = [] }

let max0_ge_arg a =
  let a = Poly.of_expr a in
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

let max0_sublinear_mul i e =
  let a = prop_Ge0 i.proves in
  let p = Poly.of_expr e in
  { proves = Ge (Poly.mul a (poly_max p), poly_max (Poly.mul a p))
  ; checks = i.checks }

let binom_monotonic k i =
  let a, b = prop_Ge i.proves in
  { proves = Ge (poly_binom k a, poly_binom k b)
  ; checks = i.checks }

let product i1 i2 =
  let a, b = prop_Ge i2.proves in
  let x = prop_Ge0 i1.proves in
  { proves = Ge (Poly.mul x a, Poly.mul x b)
  ; checks = i1.checks @ i2.checks }
