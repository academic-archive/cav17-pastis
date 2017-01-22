Require Import pasta.Pasta.

Notation IDgs_cie_cache_to_fracs_z := 1%positive.
Notation IDgs_cie_cache_to_fracs_i := 2%positive.
Notation IDgs_cie_cache_to_fracs_pcache := 3%positive.
Definition gs_cie_cache_to_fracs : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDgs_cie_cache_to_fracs_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgs_cie_cache_to_fracs_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgs_cie_cache_to_fracs_i) s) <
             (eval (ENum (512)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgs_cie_cache_to_fracs_i) s) >=
             (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDgs_cie_cache_to_fracs_i
             (Some (EAdd (EVar IDgs_cie_cache_to_fracs_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDgs_cie_cache_to_fracs_z
             (Some (EAdd (ENum (1)) (EVar IDgs_cie_cache_to_fracs_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition gs_cie_cache_to_fracs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0)%Z
    | 6%positive => (1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDgs_cie_cache_to_fracs_i) + 512 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0)%Z
    | 8%positive => (-1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -511 <= 0)%Z
    | 9%positive => (1 * (s IDgs_cie_cache_to_fracs_i) + -511 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgs_cie_cache_to_fracs_i) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -511 <= 0)%Z
    | 11%positive => (-1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) + 1 <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0)%Z
    | 12%positive => (1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) + 1 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgs_cie_cache_to_fracs_z) <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) + 1 <= 0 /\ 1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0)%Z
    | 14%positive => (1 * (s IDgs_cie_cache_to_fracs_i) + -512 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_i) + 1 <= 0 /\ -1 * (s IDgs_cie_cache_to_fracs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gs_cie_cache_to_fracs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1))%Q
    | 2%positive => ((512 # 1) + (s IDgs_cie_cache_to_fracs_z))%Q
    | 3%positive => ((s IDgs_cie_cache_to_fracs_z)
                     + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 4%positive => ((s IDgs_cie_cache_to_fracs_z)
                     + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 5%positive => ((s IDgs_cie_cache_to_fracs_z)
                     + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 6%positive => ((s IDgs_cie_cache_to_fracs_z)
                     + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 7%positive => ((s IDgs_cie_cache_to_fracs_z))%Q
    | 8%positive => ((s IDgs_cie_cache_to_fracs_z)
                     + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 9%positive => ((1 # 1) + (s IDgs_cie_cache_to_fracs_z)
                     + max0(511 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 10%positive => ((1 # 1) + (s IDgs_cie_cache_to_fracs_z)
                      + max0(511 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 11%positive => ((1 # 1) + (s IDgs_cie_cache_to_fracs_z)
                      + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 12%positive => ((1 # 1) + (s IDgs_cie_cache_to_fracs_z)
                      + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 13%positive => ((1 # 1) + (s IDgs_cie_cache_to_fracs_z)
                      + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | 14%positive => ((s IDgs_cie_cache_to_fracs_z)
                      + max0(512 - (s IDgs_cie_cache_to_fracs_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_cie_cache_to_fracs_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                            - (s IDgs_cie_cache_to_fracs_i)) (511
                                                                    - (s IDgs_cie_cache_to_fracs_i)));
                     (*-1 0*) F_max0_ge_0 (511
                                           - (s IDgs_cie_cache_to_fracs_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (512
                                                    - (s IDgs_cie_cache_to_fracs_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem gs_cie_cache_to_fracs_ai_correct:
  forall s p' s', steps (g_start gs_cie_cache_to_fracs) s (g_edges gs_cie_cache_to_fracs) p' s' -> gs_cie_cache_to_fracs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_cie_cache_to_fracs_pot_correct:
  forall s p' s',
    steps (g_start gs_cie_cache_to_fracs) s (g_edges gs_cie_cache_to_fracs) p' s' ->
    (gs_cie_cache_to_fracs_pot (g_start gs_cie_cache_to_fracs) s >= gs_cie_cache_to_fracs_pot p' s')%Q.
Proof.
  check_lp gs_cie_cache_to_fracs_ai_correct gs_cie_cache_to_fracs_hints.
Qed.

