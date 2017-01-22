Require Import pasta.Pasta.

Notation IDcie_joint_caches_init_z := 1%positive.
Notation IDcie_joint_caches_init_i := 2%positive.
Notation IDcie_joint_caches_init_j := 3%positive.
Notation IDcie_joint_caches_init_pcie := 4%positive.
Notation IDcie_joint_caches_init_pcier := 5%positive.
Notation IDcie_joint_caches_init_pjc := 6%positive.
Definition cie_joint_caches_init : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDcie_joint_caches_init_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcie_joint_caches_init_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_init_i) s) <
             (eval (ENum (512)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_init_i) s) >=
             (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDcie_joint_caches_init_j
             (Some (ENum (0)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_init_j) s) <
             (eval (ENum (3)) s))%Z)),20%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_init_j) s) >=
             (eval (ENum (3)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDcie_joint_caches_init_i
             (Some (EAdd (EVar IDcie_joint_caches_init_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDcie_joint_caches_init_z
             (Some (EAdd (ENum (1)) (EVar IDcie_joint_caches_init_z)))),
             19%positive)::(19%positive,AWeaken,5%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDcie_joint_caches_init_j
             (Some (EAdd (EVar IDcie_joint_caches_init_j) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDcie_joint_caches_init_z
             (Some (EAdd (ENum (1)) (EVar IDcie_joint_caches_init_z)))),
             26%positive)::(26%positive,AWeaken,12%positive)::nil
|}.

Definition cie_joint_caches_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_joint_caches_init_i) <= 0 /\ 1 * (s IDcie_joint_caches_init_i) <= 0 /\ 1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDcie_joint_caches_init_i) + 512 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_i) + -511 <= 0)%Z
    | 9%positive => (1 * (s IDcie_joint_caches_init_i) + -511 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_i) + -511 <= 0 /\ 1 * (s IDcie_joint_caches_init_j) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_joint_caches_init_j) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) <= 0 /\ 1 * (s IDcie_joint_caches_init_i) + -511 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 3 <= 0)%Z
    | 14%positive => (-1 * (s IDcie_joint_caches_init_j) + 3 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 3 <= 0)%Z
    | 16%positive => (-1 * (s IDcie_joint_caches_init_j) + 3 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_joint_caches_init_i) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 3 <= 0)%Z
    | 18%positive => (-1 * (s IDcie_joint_caches_init_j) + 3 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcie_joint_caches_init_i) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 3 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -2 <= 0)%Z
    | 21%positive => (1 * (s IDcie_joint_caches_init_j) + -2 <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0)%Z
    | 22%positive => (-1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -2 <= 0)%Z
    | 23%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0)%Z
    | 24%positive => (1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 1 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) <= 0)%Z
    | 25%positive => (-1 * (s IDcie_joint_caches_init_z) <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_init_j) + -3 <= 0)%Z
    | 26%positive => (1 * (s IDcie_joint_caches_init_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_init_j) + 1 <= 0 /\ -1 * (s IDcie_joint_caches_init_i) <= 0 /\ -1 * (s IDcie_joint_caches_init_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cie_joint_caches_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2048 # 1))%Q
    | 2%positive => ((2048 # 1) + (s IDcie_joint_caches_init_z))%Q
    | 3%positive => ((s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 4%positive => ((s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 5%positive => ((s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 6%positive => ((s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 7%positive => ((s IDcie_joint_caches_init_z))%Q
    | 8%positive => ((s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 9%positive => ((4 # 1) + (s IDcie_joint_caches_init_z)
                     + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 10%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 11%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 12%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 13%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 14%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 15%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 16%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 17%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 18%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 19%positive => ((s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(512 - (s IDcie_joint_caches_init_i)))%Q
    | 20%positive => ((1 # 1) + (s IDcie_joint_caches_init_z)
                      + max0(3 - (s IDcie_joint_caches_init_j))
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 21%positive => ((4 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 22%positive => ((4 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 23%positive => ((5 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 24%positive => ((5 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 25%positive => ((5 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | 26%positive => ((4 # 1) - (s IDcie_joint_caches_init_j)
                      + (s IDcie_joint_caches_init_z)
                      + (4 # 1) * max0(511 - (s IDcie_joint_caches_init_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_joint_caches_init_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (512
                                                            - (s IDcie_joint_caches_init_i)) (511
                                                                    - (s IDcie_joint_caches_init_i)));
                     (*-4 0*) F_max0_ge_0 (511
                                           - (s IDcie_joint_caches_init_i))]
    | 7%positive => []
    | 8%positive => [(*-4 0*) F_max0_pre_decrement (512
                                                    - (s IDcie_joint_caches_init_i)) (1)]
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
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                 - (s IDcie_joint_caches_init_j))) (F_check_ge (0) (0))]
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                   - 
                                                                   (s IDcie_joint_caches_init_j))) (F_check_ge (3
                                                                    - (s IDcie_joint_caches_init_j)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDcie_joint_caches_init_j)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDcie_joint_caches_init_j)))]
    | _ => []
  end.


Theorem cie_joint_caches_init_ai_correct:
  forall s p' s', steps (g_start cie_joint_caches_init) s (g_edges cie_joint_caches_init) p' s' -> cie_joint_caches_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_joint_caches_init_pot_correct:
  forall s p' s',
    steps (g_start cie_joint_caches_init) s (g_edges cie_joint_caches_init) p' s' ->
    (cie_joint_caches_init_pot (g_start cie_joint_caches_init) s >= cie_joint_caches_init_pot p' s')%Q.
Proof.
  check_lp cie_joint_caches_init_ai_correct cie_joint_caches_init_hints.
Qed.

