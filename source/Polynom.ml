(* Quentin Carbonneaux - 2016 *)

module type Factor = sig
  type t and poly
  val compare: t -> t -> int
  val subst: Types.id -> poly -> t -> poly
end

module type Monom = sig
  type t and factor and poly
  val is_one: t -> bool
  val compare: t -> t -> int
  val fold: (factor -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val one: t
  val of_factor: factor -> t
  val mul_factor: factor -> int -> t -> t
  val mul: t -> t -> t
  val subst: Types.id -> poly -> t -> poly
end

module type Poly = sig
  type t and factor and monom
  val zero: unit -> t
  val const: float -> t
  val of_factor: factor -> t
  val compare: t -> t -> int
  val fold: (monom -> float -> 'a -> 'a) -> t -> 'a -> 'a
  val mul_monom: monom -> float -> t -> t
  val add_scale: float -> t -> t -> t
  val add: t -> t -> t
  val mul: t -> t -> t
  val pow: int -> t -> t
  val subst: Types.id -> t -> t -> t
end

(* we have to tie Poly.factor and Factor.t,
   also Monom.poly and Poly.t
*)

module MkFactor(Pol: Poly)
: Factor with type poly = Pol.t
= struct

  type t =
    | Var of Types.id
    | Max of Pol.t

  type poly = Pol.t

  let of_var v = Var v

  let compare a b =
    match a, b with
    | Var v1, Var v2 -> compare v1 v2
    | Max p1, Max p2 -> Pol.compare p1 p2
    | Var _, Max _ -> -1
    | Max _, Var _ -> +1

  let pol_of_factor: t -> Pol.t
  = fun x -> Pol.of_factor (Obj.magic x)

  let subst id p = function
    | Var v when v = id -> p
    | Max p' -> pol_of_factor (Max (Pol.subst id p p'))
    | f -> pol_of_factor f

end

module MkMonom(Pol: Poly)(Fac: Factor with type poly = Pol.t)
: Monom with type factor = Fac.t and type poly = Pol.t
= struct

  (* Monoms of a polynomial are maps from factors
     to their power in the monomial.
  *)

  module M = Map.Make(Fac)
  type t = int M.t and factor = Fac.t and poly = Pol.t

  let is_one = M.is_empty
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold
  let one = M.empty
  let of_factor factor = M.add factor 1 one

  let get_pow f m =
    try M.find f m with Not_found -> 0

  let mul_factor f e m =
    M.add f (e + get_pow f m) m

  let mul m m' =
    if is_one m then m' else
    if is_one m' then m else
    fold mul_factor m' m

  let subst id p m =
    fold begin fun f e res ->
      Pol.mul (Pol.pow e (Fac.subst id p f)) res
    end m (Pol.const 1.)

end

module MkPoly(Fac: Factor)(Mon: Monom with type factor = Fac.t)
: Poly with type factor = Fac.t and type monom = Mon.t
= struct

  (* Polynomials are represented as maps from monomials
     to their coefficient.
  *)

  module M = Map.Make(Mon)
  type t = float M.t and factor = Fac.t and monom = Mon.t

  let zero = M.empty
  let const k = M.add Mon.one k zero
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold
  let of_factor f = M.add (Mon.of_factor f) 1. zero

  let get_coeff m pol =
    try M.find m pol with Not_found -> 0.

  let mul_factor f e pol =
    fold begin fun m coeff res ->
      M.add (Mon.mul_factor f e m) coeff res
    end pol zero

  let mul_monom m coeff pol =
    fold begin fun m' coeff' res ->
      M.add (Mon.mul m m') (coeff *. coeff') res
    end pol zero

  let add_scale scale =
    let f = function Some c -> c | None -> 0. in
    M.merge (fun _ ac bc -> Some (scale *. f ac +. f bc))

  let add = add_scale 1.

  let mul p1 p2 =
    fold begin fun m coeff res ->
      add (mul_monom m coeff p2) res
    end p1 zero

  let rec pow n pol =
    if n < 0 then failwith "invalid argument" else
    if n = 0 then const 1. else
    mul pol (pow (n-1) pol)

  let subst id p p' =
    let p: Mon.poly = Obj.magic p in
    fold begin fun m k res ->

      (* Because we will always be using this module in
         the recursive definition below, the Obj.magic
         is safe.  [Mon.poly] is the same type as [t]
         at runtime.
      *)
      add_scale k (Obj.magic (Mon.subst id p m): t) res

    end p' zero

  let zero () = zero

end

module
  rec Poly
  : Poly with type factor = Factor.t
    and type monom = Monom.t
  = MkPoly(Factor)(Monom)

  and Monom
  : Monom with type factor = Factor.t
    and type poly = Poly.t
  = MkMonom(Poly)(Factor)

  and Factor
  : Factor with type poly = Poly.t
  = MkFactor(Poly)
