open Types
open Focus_Types
open Graph_Types
open Presburger
open Polynom

(* todo: "dump" is probably not quite the right word, what should it be called? *)

let mkvarname funname x = "ID" ^ funname ^ "_" ^ x
let statevar s varname x = "(" ^ s ^ " " ^ varname x ^ ")"

(* Assumes that we have a state s in the context. *)
let rec dump_expr varname fmt  = function
  | ERandom ->
    Utils._TODO "coqgen random expressions"
  | EVar x ->
    Format.fprintf fmt "(EVar %s)" (varname x)
  | ENum n ->
    Format.fprintf fmt "(ENum %d)" n
  | EAdd (e1, e2) ->
     Format.fprintf fmt "(EAdd %a@ %a)"
       (dump_expr varname) e1
       (dump_expr varname) e2
  | ESub (e1, e2) ->
    Format.fprintf fmt "(ESub %a@ %a)"
      (dump_expr varname) e1
      (dump_expr varname) e2
  | EMul (e1, e2) ->
    Format.fprintf fmt "(EMul %a@ %a)"
      (dump_expr varname) e1
      (dump_expr varname) e2

(* Assumes that we have a state s in the context. *)
let rec dump_logic varname fmt = function
  | LRandom | LTrue ->
    Format.fprintf fmt "True"
  | LFalse ->
    Format.fprintf fmt "False"
  | LCmp (e1,cmp,e2) ->
    let cmp = match cmp with
      | Le -> "<="
      | Lt -> "<"
      | Ge -> ">="
      | Gt -> ">"
      | Eq -> "="
      | Ne -> "<>"
    in
    Format.fprintf fmt "((eval %a@ s) %s@ (eval %a@ s))%%Z"
      (dump_expr varname) e1
      cmp
      (dump_expr varname) e2
  | LAnd (l1,l2) ->
    Format.fprintf fmt "(%a@ /\\ %a)"
      (dump_logic varname) l1
      (dump_logic varname) l2
  | LOr (l1,l2) ->
    Format.fprintf fmt "(%a@ \\/ %a)"
      (dump_logic varname) l1
      (dump_logic varname) l2
  | LNot l ->
     Format.fprintf fmt "(~ %a)"
       (dump_logic varname) l

let dump_action varname fmt = function
  | ANone ->
    Format.fprintf fmt "ANone"
  | AWeaken ->
    Format.fprintf fmt "AWeaken"
  | AGuard a ->
    Format.fprintf fmt "(AGuard@ (fun s => %a))"
      (dump_logic varname) a
  | AAssign (x, e) ->
    Format.fprintf fmt "(AAssign@ %s@ %a)"
      (varname x)
      (dump_expr varname) e
  | ACall (xs,y,es) ->
    Utils._TODO "coqgen calls"

let dump_edges' varname fmt s es =
  List.iter (fun (a,s') ->
    Format.fprintf fmt "@[<hov>(%d,@,%a,@,%d)@]::@,"
      s (dump_action varname) a s'
  ) es

let dump_edges varname fmt es =
  Format.fprintf fmt "@[<hov>";
  Array.iteri (dump_edges' varname fmt) es;
  Format.fprintf fmt "nil";
  Format.fprintf fmt "@]"

let dump_func fmt f =
  let varname = (mkvarname f.fun_name) in
  List.iteri (fun i x ->
    Format.fprintf fmt "Notation %s := %d.@," (varname x) i
  ) (f.fun_vars @ f.fun_args);

  Format.fprintf fmt
    "Definition %s : graph := {|@   @[<v>\
         g_start := %d;@,\
         g_end := %d;@,\
         g_edges := %a\
       @]@,|}.@,"
    f.fun_name f.fun_body.g_start f.fun_body.g_end
    (dump_edges varname) f.fun_body.g_edges;

  Format.fprintf fmt "@]"

let dump_ai print_bound fn fmt ai_annots =
  (* TODO: print the correct function name *)
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@,Definition %s_ai (p: node) (s: state) := @," fn;
  Format.fprintf fmt "  match p with@,";
  let varname = statevar "s" (mkvarname fn) in
  let print_bound = print_bound varname in
  Format.fprintf fmt "    @[<v>";
  Array.iteri (fun i v ->
    Format.fprintf fmt "| %d => (%a)%%Z@," i print_bound v
  ) ai_annots;
  Format.fprintf fmt "| _ => False@]@,  end.@,";
  Format.fprintf fmt "@]"

let eps = 1e-4

let int_of_flt x =
  let c = ceil x and f = floor x in
  let dc = c -. x and df = x -. f in
  let check_ratios d1 d2 y =
    if (abs_float d1 >= eps || abs_float d2 >= eps)
    && d1 /. eps >= d2 then
      Printf.eprintf "Coq Generation: Warning trying \
                      to convert %g to an integer!\n" x;
    int_of_float y;
  in
  if dc < df
  then (check_ratios dc df c)
  else (check_ratios df dc f)

let rat_of_flt x =
  let sign, x = if x < 0. then -1, -. x else +1, +. x in
  let intp = floor x in
  let x = x -. intp in
  let farey n x =
    let rec go (a, b) (c, d) =
      if b > n then
        (c, d)
      else if d > n then
        (a, b)
      else
        let mediant =
          float_of_int (a+c) /.
          float_of_int (b+d) in
        if abs_float (x -. mediant) < eps then
          if b + d <= n then
            (a+c, b+d)
          else if d > b then
            (c, d)
          else
            (a, b)
        else if x > mediant then
          go (a+c, b+d) (c, d)
        else
          go (a, b) (a+c, b+d)
    in
    go (0, 1) (1, 1)
  in
  let (n, d) = farey 200 x in
  (sign * (n + int_of_float intp * d), d)

let rec dump_poly ring varname fmt pol =
  let print_coeff =
    match ring with
    | `Q -> begin fun fmt c ->
        let n, d = rat_of_flt c in
        Format.fprintf fmt "(%d # %d)" n d
      end
    | `Z -> begin fun fmt c ->
        Format.fprintf fmt "%d" (int_of_flt c)
      end
  in
  Format.fprintf fmt "@[<hov>";
  let is_zero = Poly.fold begin fun monom k first ->
      let pref, k =
        if k < 0.
        then "-", (-. k)
        else (if first then "" else "+"), k
      in
      if Monom.is_one monom then
        Format.fprintf fmt
          (if first then "%s%a" else "@ %s %a")
          pref print_coeff k
      else if abs_float (k -. 1.) < eps then
        Format.fprintf fmt
          (if first then "%s%a" else "@ %s %a")
          pref (dump_monom ring varname) monom
      else
        Format.fprintf fmt
          (if first then "%s@[<h>%a * %a@]" else "@ %s @[<h>%a * %a@]")
          pref print_coeff k (dump_monom ring varname) monom;
        false
    end pol true in
  if is_zero then Format.fprintf fmt "0";
  Format.fprintf fmt "@]"

and dump_monom ring varname fmt m =
  Format.fprintf fmt "@[<h>";
  let is_one = Monom.fold begin fun f e first ->
      if e = 0 then first else begin
      if e = 1 then
        Format.fprintf fmt
          (if first then "%a" else "@ *@ %a")
          (dump_factor ring varname) f
      else
        Format.fprintf fmt
          (if first then "%a^%d" else "@ *@ %a^%d")
          (dump_factor ring varname) f e;
        false
      end
    end m true in
  if is_one then Format.fprintf fmt "1";
  Format.fprintf fmt "@]"

and dump_factor ring varname fmt = function
  | Factor.Var v ->
    Format.fprintf fmt "%s" (varname v)
  | Factor.Max p ->
    Format.fprintf fmt "max0(%a)"
      (dump_poly `Z varname) p

let dump_annot fn fmt annot =
  Format.fprintf fmt "@[<v>@,Definition %s_pot (p : node) (s : state): Q := @," fn;
  Format.fprintf fmt "  match p with@,";
  let varname = statevar "s" (mkvarname fn) in
  Format.fprintf fmt "    @[<v>";
  Array.iteri (fun i v ->
    Format.fprintf fmt "| %d => (%a)%%Q@," i (dump_poly `Q varname) v
  ) annot;
  Format.fprintf fmt "| _ => (0 # 1)%%Q@]@,  end.@,";
  Format.fprintf fmt "@]";
  ()

let dump_hints fn fmt fannot =

  let varname = statevar "s" (mkvarname fn) in
  let rec dump_ast fmt =
    let print s = Format.fprintf fmt s in
    let poly = dump_poly `Z varname in
    function
    | F_one -> print "F_one"
    | F_check_ge (a, b) ->
      print "F_check_ge (%a) (%a)" poly a poly b
    | F_max0_pre_decrement (a, b) ->
      print "F_max0_pre_decrement (%a) (%a)" poly a poly b
    | F_max0_pre_increment (a, b) ->
      print "F_max0_pre_increment (%a) (%a)" poly a poly b
    | F_max0_ge_0 a ->
      print "F_max0_ge_0 (%a)" poly a
    | F_max0_ge_arg a ->
      print "F_max0_ge_arg (%a)" poly a
    | F_max0_le_arg f ->
      print "F_max0_le_arg (%a)" dump_ast f
    | F_max0_monotonic f ->
      print "F_max0_monotonic (%a)" dump_ast f
    | F_max0_sublinear f ->
      print "F_max0_sublinear (%a)" dump_ast f
    | F_binom_monotonic (k, f, g) ->
      print "F_binom_monotonic %d (%a) (%a)" k dump_ast f dump_ast g
    | F_product (f, g) ->
      print "F_product (%a) (%a)" dump_ast f dump_ast g
  in
  let dump_hint fmt ((ka, kb), f) =
    Format.fprintf fmt "@[<h>(*%g %g*) %a@]" ka kb dump_ast f.ast
  in

  Format.fprintf fmt "@[<v>@,Definition %s_hints (p : node) (s : state) := @," fn;
  Format.fprintf fmt "  match p with@,";
  Format.fprintf fmt "    @[<v>";
  Array.iteri (fun i l ->
    List.filter (fun ((ka, kb), _) -> abs_float (ka -. kb) > fsmall) l |>
    Format.fprintf fmt "| %d => %a@,"
      i (Print.list dump_hint)
  ) fannot;
  Format.fprintf fmt "| _ => []@]@,  end.@,";
  Format.fprintf fmt "@,@,@]";
  ()

let dump_theorems fn fmt =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt
    "Theorem %s_ai_correct:@   \
       forall s p' s', steps (g_start %s) s (g_edges %s) p' s' -> \
       %s_ai p' s'.@,\
     Proof.@,  check_ai.@,Qed.@,@,"
    fn fn fn fn;
  Format.fprintf fmt
    "Theorem %s_pot_correct:@   \
       forall s p' s',@,    \
         steps (g_start %s) s (g_edges %s) p' s' ->@,    \
         (%s_pot (g_start %s) s >= %s_pot p' s')%%Q.@,\
     Proof.@,  check_lp %s_ai_correct %s_hints.@,Qed.@,"
    fn fn fn fn fn fn fn fn;
  Format.fprintf fmt "@]";
  ()

let dump fstart fs print_bound ai_results annot fannot =
  let oc = open_out "generated_coq.v" in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt
    "@[<v>Add LoadPath \"coq\".@,\
     Require Import Pasta.@,@,";
  dump_func fmt (List.find (fun f -> f.fun_name = fstart) fs);
  let ai_annots = Hashtbl.find ai_results fstart in
  dump_ai print_bound fstart fmt ai_annots;
  dump_annot fstart fmt annot;
  dump_hints fstart fmt fannot;
  dump_theorems fstart fmt;
  Format.fprintf fmt "@.";
  close_out oc
