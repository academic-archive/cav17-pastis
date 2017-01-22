Require Import pasta.Pasta.

Notation IDcie_cache_restrict_z := 1%positive.
Notation IDcie_cache_restrict_i := 2%positive.
Notation IDcie_cache_restrict_pcache := 3%positive.
Notation IDcie_cache_restrict_prange := 4%positive.
Definition cie_cache_restrict : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDcie_cache_restrict_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcie_cache_restrict_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_restrict_i) s) <
             (eval (ENum (512)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_restrict_i) s) >=
             (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,14%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (11%positive,ANone,13%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,15%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcie_cache_restrict_i
             (Some (EAdd (EVar IDcie_cache_restrict_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcie_cache_restrict_z
             (Some (EAdd (ENum (1)) (EVar IDcie_cache_restrict_z)))),
             20%positive)::(20%positive,AWeaken,5%positive)::nil
|}.

Definition cie_cache_restrict_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ 1 * (s IDcie_cache_restrict_i) <= 0 /\ 1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -512 <= 0)%Z
    | 6%positive => (1 * (s IDcie_cache_restrict_i) + -512 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDcie_cache_restrict_i) + 512 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -512 <= 0)%Z
    | 8%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -511 <= 0)%Z
    | 9%positive => (1 * (s IDcie_cache_restrict_i) + -511 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -511 <= 0)%Z
    | 11%positive => (1 * (s IDcie_cache_restrict_i) + -511 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -511 <= 0)%Z
    | 13%positive => (1 * (s IDcie_cache_restrict_i) + -511 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -511 <= 0)%Z
    | 15%positive => (1 * (s IDcie_cache_restrict_i) + -511 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcie_cache_restrict_i) <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -511 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) + 1 <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -512 <= 0)%Z
    | 18%positive => (1 * (s IDcie_cache_restrict_i) + -512 <= 0 /\ -1 * (s IDcie_cache_restrict_i) + 1 <= 0 /\ -1 * (s IDcie_cache_restrict_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcie_cache_restrict_z) <= 0 /\ -1 * (s IDcie_cache_restrict_i) + 1 <= 0 /\ 1 * (s IDcie_cache_restrict_i) + -512 <= 0)%Z
    | 20%positive => (1 * (s IDcie_cache_restrict_i) + -512 <= 0 /\ -1 * (s IDcie_cache_restrict_i) + 1 <= 0 /\ -1 * (s IDcie_cache_restrict_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cie_cache_restrict_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1))%Q
    | 2%positive => ((512 # 1) + (s IDcie_cache_restrict_z))%Q
    | 3%positive => ((s IDcie_cache_restrict_z)
                     + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 4%positive => ((s IDcie_cache_restrict_z)
                     + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 5%positive => ((s IDcie_cache_restrict_z)
                     + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 6%positive => ((s IDcie_cache_restrict_z)
                     + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 7%positive => ((s IDcie_cache_restrict_z))%Q
    | 8%positive => ((s IDcie_cache_restrict_z)
                     + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 9%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                     + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 10%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 11%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 12%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 13%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 14%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 15%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 16%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(511 - (s IDcie_cache_restrict_i)))%Q
    | 17%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 18%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 19%positive => ((1 # 1) + (s IDcie_cache_restrict_z)
                      + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | 20%positive => ((s IDcie_cache_restrict_z)
                      + max0(512 - (s IDcie_cache_restrict_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_cache_restrict_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                            - (s IDcie_cache_restrict_i)) (511
                                                                    - (s IDcie_cache_restrict_i)));
                     (*-1 0*) F_max0_ge_0 (511 - (s IDcie_cache_restrict_i))]
    | 7%positive => []
    | 8%positive => [(*0 1*) F_max0_pre_decrement (512
                                                   - (s IDcie_cache_restrict_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem cie_cache_restrict_ai_correct:
  forall s p' s', steps (g_start cie_cache_restrict) s (g_edges cie_cache_restrict) p' s' -> cie_cache_restrict_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_cache_restrict_pot_correct:
  forall s p' s',
    steps (g_start cie_cache_restrict) s (g_edges cie_cache_restrict) p' s' ->
    (cie_cache_restrict_pot (g_start cie_cache_restrict) s >= cie_cache_restrict_pot p' s')%Q.
Proof.
  check_lp cie_cache_restrict_ai_correct cie_cache_restrict_hints.
Qed.

