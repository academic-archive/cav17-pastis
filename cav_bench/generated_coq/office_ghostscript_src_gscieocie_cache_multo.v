Require Import pasta.Pasta.

Notation IDcie_cache_mult_z := 1%positive.
Notation IDcie_cache_mult_i := 2%positive.
Notation IDcie_cache_mult_pcache := 3%positive.
Notation IDcie_cache_mult_pcf := 4%positive.
Notation IDcie_cache_mult_pvec := 5%positive.
Definition cie_cache_mult : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDcie_cache_mult_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcie_cache_mult_i (Some (ENum (512)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDcie_cache_mult_i
             (Some (EAdd (EVar IDcie_cache_mult_i) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcie_cache_mult_i) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcie_cache_mult_i) (ENum (-1)))
             s) < (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDcie_cache_mult_z (Some (EAdd (ENum (1))
             (EVar IDcie_cache_mult_z)))),4%positive)::nil
|}.

Definition cie_cache_mult_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_cache_mult_z) <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_cache_mult_z) <= 0 /\ 1 * (s IDcie_cache_mult_z) <= 0 /\ 1 * (s IDcie_cache_mult_i) + -512 <= 0 /\ -1 * (s IDcie_cache_mult_i) + 512 <= 0)%Z
    | 4%positive => (1 * (s IDcie_cache_mult_i) + -512 <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0 /\ -1 * (s IDcie_cache_mult_i) + 1 <= 0)%Z
    | 5%positive => (-1 * (s IDcie_cache_mult_z) <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_cache_mult_i) <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_cache_mult_z) <= 0 /\ -1 * (s IDcie_cache_mult_i) <= 0 /\ 1 * (s IDcie_cache_mult_i) <= 0)%Z
    | 8%positive => (1 * (s IDcie_cache_mult_i) <= 0 /\ -1 * (s IDcie_cache_mult_i) <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_cache_mult_z) <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDcie_cache_mult_i) + 1 <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_cache_mult_z) <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDcie_cache_mult_i) + 1 <= 0 /\ 1 * (s IDcie_cache_mult_i) + -511 <= 0 /\ -1 * (s IDcie_cache_mult_z) <= 0)%Z
    | _ => False
  end.

Definition cie_cache_mult_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((511 # 1))%Q
    | 2%positive => ((511 # 1) + (s IDcie_cache_mult_z))%Q
    | 3%positive => (-(1 # 1) + (s IDcie_cache_mult_i)
                     + (s IDcie_cache_mult_z))%Q
    | 4%positive => (-(1 # 1) + (s IDcie_cache_mult_i)
                     + (s IDcie_cache_mult_z))%Q
    | 5%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 6%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 7%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 8%positive => ((s IDcie_cache_mult_z))%Q
    | 9%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 10%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 11%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | 12%positive => ((s IDcie_cache_mult_i) + (s IDcie_cache_mult_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_cache_mult_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcie_cache_mult_i)) (-1
                                                                    + (s IDcie_cache_mult_i)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDcie_cache_mult_i));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcie_cache_mult_i)) (0))) (F_max0_ge_0 ((s IDcie_cache_mult_i)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem cie_cache_mult_ai_correct:
  forall s p' s', steps (g_start cie_cache_mult) s (g_edges cie_cache_mult) p' s' -> cie_cache_mult_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_cache_mult_pot_correct:
  forall s p' s',
    steps (g_start cie_cache_mult) s (g_edges cie_cache_mult) p' s' ->
    (cie_cache_mult_pot (g_start cie_cache_mult) s >= cie_cache_mult_pot p' s')%Q.
Proof.
  check_lp cie_cache_mult_ai_correct cie_cache_mult_hints.
Qed.

