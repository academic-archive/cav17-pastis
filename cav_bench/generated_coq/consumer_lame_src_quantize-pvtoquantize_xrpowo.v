Require Import pasta.Pasta.

Notation IDquantize_xrpow_z := 1%positive.
Notation IDquantize_xrpow_j := 2%positive.
Notation IDquantize_xrpow_cod_info := 3%positive.
Notation IDquantize_xrpow_ix := 4%positive.
Notation IDquantize_xrpow_xr := 5%positive.
Definition quantize_xrpow : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDquantize_xrpow_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDquantize_xrpow_j (Some (ENum (144)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDquantize_xrpow_j)
             s) > (eval (ENum (0)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDquantize_xrpow_j)
             s) <= (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDquantize_xrpow_j
             (Some (EAdd (EVar IDquantize_xrpow_j) (ENum (-1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDquantize_xrpow_z (Some (EAdd (ENum (1))
             (EVar IDquantize_xrpow_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition quantize_xrpow_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDquantize_xrpow_z) <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0)%Z
    | 3%positive => (-1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -144 <= 0 /\ -1 * (s IDquantize_xrpow_j) + 144 <= 0)%Z
    | 4%positive => (-1 * (s IDquantize_xrpow_j) + 144 <= 0 /\ 1 * (s IDquantize_xrpow_j) + -144 <= 0 /\ 1 * (s IDquantize_xrpow_z) <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0)%Z
    | 5%positive => (-1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -144 <= 0 /\ -1 * (s IDquantize_xrpow_j) <= 0)%Z
    | 6%positive => (-1 * (s IDquantize_xrpow_j) <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) <= 0)%Z
    | 7%positive => (1 * (s IDquantize_xrpow_j) <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0 /\ -1 * (s IDquantize_xrpow_j) <= 0)%Z
    | 8%positive => (1 * (s IDquantize_xrpow_j) + -144 <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0 /\ -1 * (s IDquantize_xrpow_j) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDquantize_xrpow_j) + 1 <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -144 <= 0)%Z
    | 10%positive => (1 * (s IDquantize_xrpow_j) + -144 <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0 /\ -1 * (s IDquantize_xrpow_j) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -143 <= 0 /\ -1 * (s IDquantize_xrpow_j) <= 0)%Z
    | 12%positive => (-1 * (s IDquantize_xrpow_j) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -143 <= 0 /\ -1 * (s IDquantize_xrpow_z) <= 0)%Z
    | 13%positive => (-1 * (s IDquantize_xrpow_z) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -143 <= 0 /\ -1 * (s IDquantize_xrpow_j) <= 0)%Z
    | 14%positive => (-1 * (s IDquantize_xrpow_j) <= 0 /\ 1 * (s IDquantize_xrpow_j) + -143 <= 0 /\ -1 * (s IDquantize_xrpow_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition quantize_xrpow_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((144 # 1))%Q
    | 2%positive => ((144 # 1) + (s IDquantize_xrpow_z))%Q
    | 3%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | 4%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | 5%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | 6%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | 7%positive => ((s IDquantize_xrpow_z))%Q
    | 8%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | 9%positive => ((1 # 1) + (s IDquantize_xrpow_z)
                     + max0(-1 + (s IDquantize_xrpow_j)))%Q
    | 10%positive => ((1 # 1) + (s IDquantize_xrpow_z)
                      + max0(-1 + (s IDquantize_xrpow_j)))%Q
    | 11%positive => ((1 # 1) + (s IDquantize_xrpow_z)
                      + max0((s IDquantize_xrpow_j)))%Q
    | 12%positive => ((1 # 1) + (s IDquantize_xrpow_z)
                      + max0((s IDquantize_xrpow_j)))%Q
    | 13%positive => ((1 # 1) + (s IDquantize_xrpow_z)
                      + max0((s IDquantize_xrpow_j)))%Q
    | 14%positive => ((s IDquantize_xrpow_z) + max0((s IDquantize_xrpow_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition quantize_xrpow_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDquantize_xrpow_j)) (-1
                                                                    + (s IDquantize_xrpow_j)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDquantize_xrpow_j))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement ((s IDquantize_xrpow_j)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem quantize_xrpow_ai_correct:
  forall s p' s', steps (g_start quantize_xrpow) s (g_edges quantize_xrpow) p' s' -> quantize_xrpow_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem quantize_xrpow_pot_correct:
  forall s p' s',
    steps (g_start quantize_xrpow) s (g_edges quantize_xrpow) p' s' ->
    (quantize_xrpow_pot (g_start quantize_xrpow) s >= quantize_xrpow_pot p' s')%Q.
Proof.
  check_lp quantize_xrpow_ai_correct quantize_xrpow_hints.
Qed.

