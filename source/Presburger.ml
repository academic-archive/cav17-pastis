(* Quentin Carbonneaux - 2016 *)

let debug = false

module Id = struct
  type t = Types.id
  let compare = compare
end

module S = Set.Make(Id)

module L = struct

  (* Linear sums are represented as a map
     from variable to their coefficient in
     the linear sum and a constant k.
     They are used as constraints below and
     we interpret them as:

        sum_i k_i v_i + k <= 0

  *)

  include Map.Make(Id)
  type sum = {m: int t; k: int}

  let const k = {m = empty; k}

  let eq a b =
    compare ( - ) a.m b.m = 0 && a.k = b.k

  let coeff id {m;_} =
    try find id m with Not_found -> 0

  let set x c {m;k} =
    {m = if c = 0 then remove x m else add x c m; k}

  let addl id n m = set id (coeff id m + n) m

  let addk k' {m; k} = {m; k = k + k'}

  let mult c {m; k} =
    if c = 0 then {m = empty; k = 0} else
    {m = map (fun n -> c * n) m; k = c * k}

  let vars vs s =
    fold (fun k c vs -> S.add k vs) s.m vs

  let plus c {m = m1; k = k1} {m = m2; k = k2} =
    let h = function Some n -> n | None -> 0 in
    let f _ a b =
      let o = c * h a + h b in
      if o = 0 then None else Some o in
    {m = merge f m1 m2; k = c * k1 + k2}

  let print fmt {m; k} =
    let open Format in
    let sign first c =
      if first then
        if c < 0 then "-" else ""
      else
        if c < 0 then " -" else " +"
    in
    let rec p first = function
      | (x, c) :: tl when c <> 0 ->
        if abs c <> 1 then
          fprintf fmt (if first then "%s%d%s" else "%s@ %d%s")
            (sign first c) (abs c) x
        else
          fprintf fmt (if first then "%s%s" else "%s@ %s")
            (sign first c) x;
        p false tl
      | _ :: tl -> p first tl
      | [] -> first
    in
    let first = p true (bindings m) in
    if k <> 0 then
      fprintf fmt (if first then "%s%d" else "%s@ %d")
        (sign first k) (abs k)
    else if first then
      fprintf fmt "0"
end

(* The Presburger decision procedure. *)

(* Presburger decision procedure uses
   divisibility constraints, the meaning
   of the record below is "k divides s".
*)
type div = {k : int; s : L.sum}

let lcm a b =
  let rec gcd a b =
    if a = 0 || b = 0 then a + b else
    if a < b
      then gcd a (b mod a)
      else gcd (a mod b) b in
  let a = abs a and b = abs b in
  assert (a * b <> 0); (a * b) / gcd a b

let normi id ps =
  (* Makes sure id has the same coefficient everywhere. *)
  let l = List.fold_left (fun l i -> lcm l (L.coeff id i)) 1 ps in
  let f m = L.mult (l / abs (L.coeff id m)) m in
  (List.map f ps, l)

let dmsg x =
  if debug
    then Printf.printf x
    else Printf.ifprintf stdout x

let uid = ref 0

let rec elim x (ps, ds) vars =
  let c = L.coeff x in
  let ps, irest = List.partition (fun i -> c i <> 0) ps in
  let ps, l = normi x ps in
  let ubs, lbs = List.partition (fun i -> c i > 0) ps in
  let ds, drest = List.partition (fun d -> c d.s <> 0) ds in
  let w = List.fold_left (fun w d -> lcm w d.k) 1 ds in
  List.exists begin fun glb ->
    let lbs' =  List.map (L.plus (-1) glb) lbs in
    let rec loop i =
      if i < 0 then false else
      let xeq = L.addk i glb in
      let ubs' = List.map (L.plus 1 xeq) ubs in
      let ds' =
        let trd {k; s} =
          { k = k * l
          ; s = L.plus 1 (L.mult (c s) xeq) (L.mult l s)
          } in
        let s = L.set x 0 xeq in
        {k = l; s} :: List.map trd ds in
      assert (List.for_all (fun i -> L.coeff x i = 0) (lbs' @ ubs')); (* XXX *)
      assert (List.for_all (fun d -> L.coeff x d.s = 0) ds');
      let id = !uid in incr uid;
      dmsg ">> (%d) attempt with i=%d\n" id i;
      let sb = sat (lbs' @ ubs' @ irest, ds' @ drest) vars in
      dmsg "<< (%d) end of attempt with i=%d\n" id i;
      sb || loop (i-1)
    in loop (l * w - 1)
  end (
    if lbs <> [] then lbs else
    [L.set x (-l) (L.const (-100_000_000_000))]
  )

and sat (ps, divs) = function
  | id :: vars -> elim id (ps, divs) vars
  | [] ->
    let zero {L.m;_} = assert (L.for_all (fun _ c -> c = 0) m) in (* XXX *)
    let deci s = zero s; dmsg "  %d <= 0\n" s.L.k; s.L.k <= 0 in
    let decd {k; s} = zero s; dmsg "  %d | %d\n" k s.L.k; s.L.k mod k = 0 in
    List.for_all deci ps && List.for_all decd divs

let sat (ps, divs) vars =
  uid := 0; sat (ps, divs) vars

let sat ps =
  let vars = List.fold_left L.vars S.empty ps
  in sat (ps, []) (S.elements vars)

(* Applications. *)

let bottom =
  [L.const 1]

let implies ps a =
  let nega = L.addk 1 (L.mult (-1) a) in
  not (sat (nega :: ps))

let minimize =
  let rec f ps = function
    | a :: ps' ->
      if implies ps a then f ps ps' else
      f (a :: List.filter (fun b -> not (implies [a] b)) ps) ps'
    | [] -> ps
  in f []

let meet ps1 ps2 =
  minimize (List.rev_append ps1 ps2)

let join ps1 ps2 =
  List.rev_append ps1 ps2
    |> List.filter (implies ps1)
    |> List.filter (implies ps2)
    |> minimize

let print fmt ps =
  if ps = [] then Format.pp_print_string fmt "Top" else
  if not (sat ps) then Format.pp_print_string fmt "Bot" else
  Print.list ~first:"@[<h>" ~sep:" &&@ " ~last:"@]"
    (fun fmt -> Format.fprintf fmt "%a ≤ 0" L.print)
    fmt ps
