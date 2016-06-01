(* Quentin Carbonneaux - 2016 *)

(*
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

open Polynom

type focus = Types.expr list * Poly.t

module Builder
: sig
  type t
  val check_ge: Types.expr -> Types.expr -> t
  val max0_ge_0: Types.expr -> t
  val max0_ge_arg: Types.expr -> t
  val max0_le_arg: t -> t
  val max0_monotonic: t -> t
  val max0_sublinear: t -> t
  val max0_sublinear_mul: t -> Types.expr -> t
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
    { proves = Ge (Poly.of_expr a, Poly.of_expr b)
    ; checks = [Ge (a, b)] }

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

  let export i =
    let mkexpr = function
      | Ge0 e -> e
      | Ge (e1, e2) -> Types.ESub (e1, e2) in
    (List.map mkexpr i.checks, prop_Ge0 i.proves)

end

type _ interp =
  | IFunc: 'a interp -> (Builder.t -> 'a) interp
  | IExpr: 'a interp -> (Types.expr -> 'a) interp
  | INum: 'a interp -> (int -> 'a) interp
  | IEnd: Builder.t interp

type dp = DP: 'a interp * 'a -> dp

let interp_map =
  [ "max0_ge_0",       DP (IExpr IEnd, Builder.max0_ge_0)
  ; "max0_ge_arg",     DP (IExpr IEnd, Builder.max0_ge_arg)
  ; "max0_le_arg",     DP (IFunc IEnd, Builder.max0_le_arg)
  ; "max0_monotonic",  DP (IFunc IEnd, Builder.max0_monotonic)
  ; "max0_sublinear",  DP (IFunc IEnd, Builder.max0_sublinear)
  ; "max0_sublinear_mul", DP (IFunc (IExpr IEnd), Builder.max0_sublinear_mul)
  ; "binom_monotonic", DP (INum (IFunc IEnd), Builder.binom_monotonic)
  ; "product",         DP (IFunc (IFunc IEnd), Builder.product)
  ; ">=",              DP (IExpr (IExpr IEnd), Builder.check_ge)
  ]

let interpret fe =
  let nth n =
    if n > 2 then string_of_int (n+1) ^ "-th" else
    [| "first"; "second"; "third" |].(n)
  in

  let rec iargs pos fname (DP (i, f)) l n: Builder.t =
    let getarg = function
      | h :: tl -> h, tl
      | [] ->
        Format.eprintf "%a: too few arguments for %s@."
          Utils.print_position pos fname;
        raise Utils.Error
    in
    match i with
    | IEnd ->
      if l = [] then f else begin
        Format.eprintf "%a: too many arguments for %s (%d expected)@."
          Utils.print_position pos fname n;
        raise Utils.Error
      end
    | IFunc i ->
      let arg, l = getarg l in
      iargs pos fname (DP (i, f (interp arg))) l (n+1)
    | IExpr i ->
      let arg, l = getarg l in
      let arg = match arg with
        | Types.FBase e -> e
        | _ ->
          Format.eprintf "%a: expression expected as %s argument of %s@."
            Utils.print_position pos (nth n) fname;
          raise Utils.Error in
      iargs pos fname (DP (i, f arg)) l (n+1)
    | INum i ->
      let arg, l = getarg l in
      let arg = match arg with
        | Types.FBase (Types.ENum n) -> n
        | _ ->
          Format.eprintf "%a: number expected as %s argument of %s@."
            Utils.print_position pos (nth n) fname;
          raise Utils.Error in
      iargs pos fname (DP (i, f arg)) l (n+1)

  and interp = function
    | Types.FBase e -> Builder.check_ge e (Types.ENum 0)
    | Types.FApply (f, l, pos) ->
      try iargs pos f (List.assoc f interp_map) l 0
      with Not_found ->
        Format.eprintf "%a: unknown focus function '%s'@."
          Utils.print_position pos f;
        raise Utils.Error

  in Builder.export (interp fe)
