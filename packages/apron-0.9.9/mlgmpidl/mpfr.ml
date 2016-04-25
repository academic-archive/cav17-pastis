(* File generated from mpfr.idl *)

(* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type t
and round = 
  | Near
  | Zero
  | Up
  | Down

(** MPFR multiprecision floating-point numbers *)


(** The following operations are mapped as much as possible to their C counterpart. In case of imperative functions (like [set], [add], ...) the first parameter of type [t] is an out-parameter and holds the result when the function returns. For instance, [add x y z] adds the values of [y] and [z] and stores the result in [x].

 These functions are as efficient as their C counterpart: they do not imply additional memory allocation. *)



(** {2 Rounding Modes} *)

external set_default_rounding_mode : round -> unit
	= "camlidl_mpfr_mpfr_set_default_rounding_mode"

external get_default_rounding_mode : unit -> round
	= "camlidl_mpfr_mpfr_get_default_rounding_mode"

external round_prec : t -> round -> int -> int
	= "camlidl_mpfr_mpfr_round_prec"


(** {2 Exceptions} *)

external get_emin : unit -> int
	= "camlidl_mpfr_mpfr_get_emin"

external get_emax : unit -> int
	= "camlidl_mpfr_mpfr_get_emax"

external set_emin : int -> unit
	= "camlidl_mpfr_mpfr_set_emin"

external set_emax : int -> unit
	= "camlidl_mpfr_mpfr_set_emax"

external check_range : t -> int -> round -> int
	= "camlidl_mpfr_mpfr_check_range"

external clear_underflow : unit -> unit
	= "camlidl_mpfr_mpfr_clear_underflow"

external clear_overflow : unit -> unit
	= "camlidl_mpfr_mpfr_clear_overflow"

external clear_nanflag : unit -> unit
	= "camlidl_mpfr_mpfr_clear_nanflag"

external clear_inexflag : unit -> unit
	= "camlidl_mpfr_mpfr_clear_inexflag"

external clear_flags : unit -> unit
	= "camlidl_mpfr_mpfr_clear_flags"

external underflow_p : unit -> bool
	= "camlidl_mpfr_mpfr_underflow_p"

external overflow_p : unit -> bool
	= "camlidl_mpfr_mpfr_overflow_p"

external nanflag_p : unit -> bool
	= "camlidl_mpfr_mpfr_nanflag_p"

external inexflag_p : unit -> bool
	= "camlidl_mpfr_mpfr_inexflag_p"


(** {2 Initialization and Assignment Functions} *)

external set_default_prec : int -> unit
	= "camlidl_mpfr_mpfr_set_default_prec"

external get_default_prec : unit -> int
	= "camlidl_mpfr_mpfr_get_default_prec"

external init : unit -> t
	= "camlidl_mpfr_mpfr_init"

external init2 : int -> t
	= "camlidl_mpfr_mpfr_init2"

external get_prec : t -> int
	= "camlidl_mpfr_mpfr_get_prec"

external set_prec : t -> int -> unit
	= "camlidl_mpfr_mpfr_set_prec"

external set_prec_raw : t -> int -> unit
	= "camlidl_mpfr_mpfr_set_prec_raw"

external set : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_set"

external set_si : t -> int -> round -> int
	= "camlidl_mpfr_mpfr_set_si"

external set_d : t -> float -> round -> int
	= "camlidl_mpfr_mpfr_set_d"

external set_z : t -> Mpz.t -> round -> int
	= "camlidl_mpfr_mpfr_set_z"

external set_q : t -> Mpq.t -> round -> int
	= "camlidl_mpfr_mpfr_set_q"

external _set_str : t -> string -> int -> round -> unit
	= "camlidl_mpfr_mpfr__set_str"

let set_str a b ~base round = _set_str a b base round
external set_f : t -> Mpf.t -> round -> int
	= "camlidl_mpfr_mpfr_set_f"

external set_inf : t -> int -> unit
	= "camlidl_mpfr_mpfr_set_inf"

external set_nan : t -> unit
	= "camlidl_mpfr_mpfr_set_nan"

external swap : t -> t -> unit
	= "camlidl_mpfr_mpfr_swap"

external init_set : t -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set"

external init_set_si : int -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set_si"

external init_set_d : float -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set_d"

external init_set_f : Mpf.t -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set_f"

external init_set_z : Mpz.t -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set_z"

external init_set_q : Mpq.t -> round -> int * t
	= "camlidl_mpfr_mpfr_init_set_q"

external _init_set_str : string -> int -> round -> t
	= "camlidl_mpfr_mpfr__init_set_str"

let init_set_str a ~base round = _init_set_str a base round

(** {2 Conversion Functions} *)

external get_d : t -> round -> float
	= "camlidl_mpfr_mpfr_get_d"

external get_d1 : t -> float
	= "camlidl_mpfr_mpfr_get_d1"

external get_z_exp : Mpz.t -> t -> int
	= "camlidl_mpfr_mpfr_get_z_exp"

external get_z : Mpz.t -> t -> round -> unit
	= "camlidl_mpfr_mpfr_get_z"

external _get_str : int -> int -> t -> round -> string * int
	= "camlidl_mpfr_mpfr__get_str"

let get_str ~base ~digits a round = _get_str base digits a round

(** {2 Basic Arithmetic Functions} *)

external add : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_add"

external add_ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_add_ui"

external add_z : t -> t -> Mpz.t -> round -> int
	= "camlidl_mpfr_mpfr_add_z"

external add_q : t -> t -> Mpq.t -> round -> int
	= "camlidl_mpfr_mpfr_add_q"

external sub : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_sub"

external ui_sub : t -> int -> t -> round -> int
	= "camlidl_mpfr_mpfr_ui_sub"

external sub_ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_sub_ui"

external sub_z : t -> t -> Mpz.t -> round -> int
	= "camlidl_mpfr_mpfr_sub_z"

external sub_q : t -> t -> Mpq.t -> round -> int
	= "camlidl_mpfr_mpfr_sub_q"

external mul : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_mul"

external mul_ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_mul_ui"

external mul_z : t -> t -> Mpz.t -> round -> int
	= "camlidl_mpfr_mpfr_mul_z"

external mul_q : t -> t -> Mpq.t -> round -> int
	= "camlidl_mpfr_mpfr_mul_q"

external mul_2ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_mul_2ui"

external mul_2si : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_mul_2si"

external mul_2exp : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_mul_2exp"

external div : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_div"

external ui_div : t -> int -> t -> round -> int
	= "camlidl_mpfr_mpfr_ui_div"

external div_ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_div_ui"

external div_z : t -> t -> Mpz.t -> round -> int
	= "camlidl_mpfr_mpfr_div_z"

external div_q : t -> t -> Mpq.t -> round -> int
	= "camlidl_mpfr_mpfr_div_q"

external div_2ui : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_div_2ui"

external div_2si : t -> t -> int -> round -> int
	= "camlidl_mpfr_mpfr_div_2si"

let div_2exp = div_2ui
external sqrt : t -> t -> round -> bool
	= "camlidl_mpfr_mpfr_sqrt"

external sqrt_ui : t -> int -> round -> bool
	= "camlidl_mpfr_mpfr_sqrt_ui"

external pow_ui : t -> t -> int -> round -> bool
	= "camlidl_mpfr_mpfr_pow_ui"

external pow_si : t -> t -> int -> round -> bool
	= "camlidl_mpfr_mpfr_pow_si"

external ui_pow_ui : t -> int -> int -> round -> bool
	= "camlidl_mpfr_mpfr_ui_pow_ui"

external ui_pow : t -> int -> t -> round -> bool
	= "camlidl_mpfr_mpfr_ui_pow"

external pow : t -> t -> t -> round -> bool
	= "camlidl_mpfr_mpfr_pow"

external neg : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_neg"

external abs : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_abs"


(** {2 Comparison Functions} *)

external cmp : t -> t -> int
	= "camlidl_mpfr_mpfr_cmp"

external cmp_si : t -> int -> int
	= "camlidl_mpfr_mpfr_cmp_si"

external cmp_si_2exp : t -> int -> int -> int
	= "camlidl_mpfr_mpfr_cmp_si_2exp"

external sgn : t -> int
	= "camlidl_mpfr_mpfr_sgn"

external _equal : t -> t -> int -> bool
	= "camlidl_mpfr_mpfr__equal"

let equal a b ~bits = _equal a b bits
external nan_p : t -> bool
	= "camlidl_mpfr_mpfr_nan_p"

external inf_p : t -> bool
	= "camlidl_mpfr_mpfr_inf_p"

external number_p : t -> bool
	= "camlidl_mpfr_mpfr_number_p"

external reldiff : t -> t -> t -> round -> unit
	= "camlidl_mpfr_mpfr_reldiff"


(** {2 Special Functions} *)

external log : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_log"

external log2 : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_log2"

external log10 : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_log10"

external exp : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_exp"

external exp2 : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_exp2"

external exp10 : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_exp10"

external cos : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_cos"

external sin : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_sin"

external tan : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_tan"

external sec : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_sec"

external csc : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_csc"

external cot : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_cot"

external sin_cos : t -> t -> t -> round -> bool
	= "camlidl_mpfr_mpfr_sin_cos"

external acos : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_acos"

external asin : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_asin"

external atan : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_atan"

external atan2 : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_atan2"

external cosh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_cosh"

external sinh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_sinh"

external tanh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_tanh"

external sech : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_sech"

external csch : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_csch"

external coth : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_coth"

external acosh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_acosh"

external asinh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_asinh"

external atanh : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_atanh"

external fac_ui : t -> int -> round -> int
	= "camlidl_mpfr_mpfr_fac_ui"

external log1p : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_log1p"

external expm1 : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_expm1"

external eint : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_eint"

external gamma : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_gamma"

external lngamma : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_lngamma"

external zeta : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_zeta"

external erf : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_erf"

external erfc : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_erfc"

external fma : t -> t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_fma"

external agm : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_agm"

external hypot : t -> t -> t -> round -> int
	= "camlidl_mpfr_mpfr_hypot"

external const_log2 : t -> round -> int
	= "camlidl_mpfr_mpfr_const_log2"

external const_pi : t -> round -> int
	= "camlidl_mpfr_mpfr_const_pi"

external const_euler : t -> round -> int
	= "camlidl_mpfr_mpfr_const_euler"

external const_catalan : t -> round -> int
	= "camlidl_mpfr_mpfr_const_catalan"


(** {2 Input and Output Functions: not interfaced} *)


(** {2 Miscellaneous Float Functions} *)

external rint : t -> t -> round -> int
	= "camlidl_mpfr_mpfr_rint"

external ceil : t -> t -> int
	= "camlidl_mpfr_mpfr_ceil"

external floor : t -> t -> int
	= "camlidl_mpfr_mpfr_floor"

external round : t -> t -> int
	= "camlidl_mpfr_mpfr_round"

external trunc : t -> t -> int
	= "camlidl_mpfr_mpfr_trunc"

external integer_p : t -> bool
	= "camlidl_mpfr_mpfr_integer_p"


(** {2 Additional functions} *)

let to_string x = 
let (s,e) = get_str ~base:10 ~digits:0 x Near in 
if s="" 
then "0.0" 
else Format.sprintf "0.%sE%i" s e 


(** {2 Pretty printing} *)

let print fmt x = 
let (s,e) = get_str ~base:10 ~digits:0 x Near in 
if s="" 
then Format.pp_print_string fmt "0.0" 
else Format.fprintf fmt "0.%sE%i" s e 

let string_of_round = function
| Near -> "Near"
| Zero -> "Zero"
| Up -> "Up"
| Down -> "Down"
let print_round fmt x = Format.pp_print_string fmt (string_of_round x)

let to_float ?round x = 
  match round with
  | None -> get_d1 x
  | Some r -> get_d x r

let to_mpq x = 
   let num,den = Mpz.init(), Mpz.init_set_si 1 in
   let e = get_z_exp num x in
   Mpz.mul_2exp den den e;
   Mpq.of_mpz2 num den

let of_int x r = snd (init_set_si x r)
let of_float x r = snd (init_set_d x r)
let of_mpz x r = snd (init_set_z x r)
let of_mpq x r = snd (init_set_q x r)
let of_string x r = init_set_str x ~base:10 r
let of_frac n d r = let mpq = Mpq.of_frac n d in of_mpq mpq r
let of_mpz2 n d r = let mpq = Mpq.of_mpz2 n d in of_mpq mpq r
