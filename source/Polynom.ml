(* Quentin Carbonneaux - 2016 *)

module type Factor = sig
  type poly
  type t = Var of Types.id | Max of poly
  val compare: t -> t -> int
end

module type Monom = sig
  type t and factor
  val is_one: t -> bool
  val compare: t -> t -> int
  val fold: (factor -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val one: t
  val of_factor: factor -> int -> t
  val mul_factor: factor -> int -> t -> t
  val mul: t -> t -> t
end

module type Poly = sig
  type t and monom
  val zero: unit -> t
  val const: float -> t
  val of_monom: monom -> float -> t
  val compare: t -> t -> int
  val fold: (monom -> float -> 'a -> 'a) -> t -> 'a -> 'a
  val mul_monom: monom -> float -> t -> t
  val add_monom: monom -> float -> t -> t
  val add_scale: float -> t -> t -> t
  val add: t -> t -> t
  val mul: t -> t -> t
  val pow: int -> t -> t
end

(* we have to tie Poly.factor and Factor.t,
   also Monom.poly and Poly.t
*)

module MkFactor(Pol: Poly)
: Factor with type poly = Pol.t
= struct

  type poly = Pol.t
  type t =
    | Var of Types.id
    | Max of poly

  let compare a b =
    match a, b with
    | Var v1, Var v2 -> compare v1 v2
    | Max p1, Max p2 -> Pol.compare p1 p2
    | Var _, Max _ -> -1
    | Max _, Var _ -> +1

end

module MkMonom(Fac: Factor)
: Monom with type factor = Fac.t
= struct

  (* Monoms of a polynomial are maps from factors
     to their power in the monomial.
  *)

  module M = Map.Make(Fac)
  type factor = Fac.t
  type t = int M.t

  let is_one = M.is_empty
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold
  let one = M.empty
  let of_factor f e = M.add f e one

  let get_pow f m =
    try M.find f m with Not_found -> 0

  let mul_factor f e m =
    M.add f (e + get_pow f m) m

  let mul m m' =
    if is_one m then m' else
    if is_one m' then m else
    fold mul_factor m' m

end

module MkPoly(Mon: Monom)
: Poly with type monom = Mon.t
= struct

  (* Polynomials are represented as maps from monomials
     to their coefficient.
  *)

  module M = Map.Make(Mon)
  type monom = Mon.t
  type t = float M.t

  let zero = M.empty
  let const k = M.add Mon.one k zero
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold
  let of_monom m k = M.add m k zero

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

  let add_monom m k pol =
    M.add m (get_coeff m pol +. k) pol

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

  let zero () = zero

end

module
  rec Poly
  : Poly with type monom = Monom.t
  = MkPoly(Monom)

  and Monom
  : Monom with type factor = Factor.t
  = MkMonom(Factor)

  and Factor
  : Factor with type poly = Poly.t
  = MkFactor(Poly)


let rec mul_fmp a b =
  let mkmonom = function `Factor (fa, ea) ->
    `Monom (Monom.of_factor fa ea, 1.)
  in match a, b with
  | `Poly pa, `Poly pb -> `Poly (Poly.mul pa pb)
  | `Poly pa, `Monom (mb, kb) -> `Poly (Poly.mul_monom mb kb pa)
  | `Monom (ma, ka), `Poly pb -> `Poly (Poly.mul_monom ma ka pb)
  | `Poly _, (`Factor _ as b) -> mul_fmp a (mkmonom b)
  | (`Factor _ as a), `Poly _ -> mul_fmp (mkmonom a) b
  | `Monom (ma, ka), `Monom (mb, kb) -> `Monom (Monom.mul ma mb, ka *. kb)
  | `Monom (ma, ka), `Factor (fb, eb) -> `Monom (Monom.mul_factor fb eb ma, ka)
  | `Factor (fa, ea), `Monom (mb, kb) -> `Monom (Monom.mul_factor fa ea mb, kb)
  | (`Factor _ as a), (`Factor _ as b) -> mul_fmp (mkmonom a) (mkmonom b)

let rec factor_subst id p = function
  | Factor.Var v when v = id -> `Poly p
  | Factor.Max p' -> `Factor (Factor.Max (poly_subst id p p'), 1)
  | f -> `Factor (f, 1)

and monom_subst id p m =
  Monom.fold begin fun f e res ->
    let fe =
      match factor_subst id p f with
      | `Poly p -> `Poly (Poly.pow e p)
      | `Factor (f, e') -> `Factor (f, e * e')
    in
    mul_fmp fe res
  end m (`Monom (Monom.one, 1.))

and poly_subst id p p' =
  Poly.fold begin fun m k res ->
    match monom_subst id p m with
    | `Poly p -> Poly.add_scale k p res
    | `Monom (m', k') -> Poly.add_monom m' k' res
    | `Factor (f, e) -> Poly.add_monom (Monom.of_factor f e) 1. res
  end p' (Poly.zero ())
