(* Quentin Carbonneaux - 2016 *)

open Polynom

module Builder = Focus_Builder

type focus = Poly.t list * Poly.t

type _ interp =
  | IFunc: 'a interp -> (Builder.t -> 'a) interp
  | IExpr: 'a interp -> (Poly.t -> 'a) interp
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
  ; "binom_monotonic", DP (INum (IFunc (IFunc IEnd)), Builder.binom_monotonic)
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
        | Types.FBase e -> Poly.of_expr e
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
    | Types.FBase e -> Builder.check_ge (Poly.of_expr e) (Poly.zero ())
    | Types.FApply (f, l, pos) ->
      try iargs pos f (List.assoc f interp_map) l 0
      with Not_found ->
        Format.eprintf "%a: unknown focus function '%s'@."
          Utils.print_position pos f;
        raise Utils.Error

  in Builder.export (interp fe)
