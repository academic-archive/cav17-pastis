Require Import pasta.Pasta.

Notation IDgx_install_CIEABC_z := 1%positive.
Notation IDgx_install_CIEABC_i := 2%positive.
Notation IDgx_install_CIEABC_j := 3%positive.
Notation IDgx_install_CIEABC_pcs := 4%positive.
Notation IDgx_install_CIEABC_pgs := 5%positive.
Definition gx_install_CIEABC : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDgx_install_CIEABC_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgx_install_CIEABC_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgx_install_CIEABC_i) s) <
             (eval (ENum (512)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgx_install_CIEABC_i) s) >=
             (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDgx_install_CIEABC_j (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDgx_install_CIEABC_j) s) <
             (eval (ENum (3)) s))%Z)),20%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDgx_install_CIEABC_j) s) >=
             (eval (ENum (3)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDgx_install_CIEABC_i
             (Some (EAdd (EVar IDgx_install_CIEABC_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDgx_install_CIEABC_z
             (Some (EAdd (ENum (1)) (EVar IDgx_install_CIEABC_z)))),
             19%positive)::(19%positive,AWeaken,5%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDgx_install_CIEABC_j
             (Some (EAdd (EVar IDgx_install_CIEABC_j) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDgx_install_CIEABC_z
             (Some (EAdd (ENum (1)) (EVar IDgx_install_CIEABC_z)))),
             26%positive)::(26%positive,AWeaken,12%positive)::nil
|}.

Definition gx_install_CIEABC_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgx_install_CIEABC_i) <= 0 /\ 1 * (s IDgx_install_CIEABC_i) <= 0 /\ 1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDgx_install_CIEABC_i) + 512 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_i) + -511 <= 0)%Z
    | 9%positive => (1 * (s IDgx_install_CIEABC_i) + -511 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_i) + -511 <= 0 /\ 1 * (s IDgx_install_CIEABC_j) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) <= 0)%Z
    | 11%positive => (-1 * (s IDgx_install_CIEABC_j) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) <= 0 /\ 1 * (s IDgx_install_CIEABC_i) + -511 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0)%Z
    | 12%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 3 <= 0)%Z
    | 14%positive => (-1 * (s IDgx_install_CIEABC_j) + 3 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 3 <= 0)%Z
    | 16%positive => (-1 * (s IDgx_install_CIEABC_j) + 3 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDgx_install_CIEABC_i) + 1 <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 3 <= 0)%Z
    | 18%positive => (-1 * (s IDgx_install_CIEABC_j) + 3 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDgx_install_CIEABC_i) + 1 <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 3 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -2 <= 0)%Z
    | 21%positive => (1 * (s IDgx_install_CIEABC_j) + -2 <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0)%Z
    | 22%positive => (-1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -2 <= 0)%Z
    | 23%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 1 <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0)%Z
    | 24%positive => (1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 1 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) <= 0)%Z
    | 25%positive => (-1 * (s IDgx_install_CIEABC_z) <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 1 <= 0 /\ 1 * (s IDgx_install_CIEABC_j) + -3 <= 0)%Z
    | 26%positive => (1 * (s IDgx_install_CIEABC_j) + -3 <= 0 /\ -1 * (s IDgx_install_CIEABC_j) + 1 <= 0 /\ -1 * (s IDgx_install_CIEABC_i) <= 0 /\ -1 * (s IDgx_install_CIEABC_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gx_install_CIEABC_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2048 # 1))%Q
    | 2%positive => ((2048 # 1) + (s IDgx_install_CIEABC_z))%Q
    | 3%positive => ((s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 4%positive => ((s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 5%positive => ((s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 6%positive => ((s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 7%positive => ((s IDgx_install_CIEABC_z))%Q
    | 8%positive => ((s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 9%positive => ((4 # 1) + (s IDgx_install_CIEABC_z)
                     + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 10%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 11%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 12%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 13%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 14%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 15%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 16%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 17%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 18%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 19%positive => ((s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(512 - (s IDgx_install_CIEABC_i)))%Q
    | 20%positive => ((1 # 1) + (s IDgx_install_CIEABC_z)
                      + max0(3 - (s IDgx_install_CIEABC_j))
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 21%positive => ((4 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 22%positive => ((4 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 23%positive => ((5 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 24%positive => ((5 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 25%positive => ((5 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | 26%positive => ((4 # 1) - (s IDgx_install_CIEABC_j)
                      + (s IDgx_install_CIEABC_z)
                      + (4 # 1) * max0(511 - (s IDgx_install_CIEABC_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_install_CIEABC_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (512
                                                            - (s IDgx_install_CIEABC_i)) (511
                                                                    - (s IDgx_install_CIEABC_i)));
                     (*-4 0*) F_max0_ge_0 (511 - (s IDgx_install_CIEABC_i))]
    | 7%positive => []
    | 8%positive => [(*-4 0*) F_max0_pre_decrement (512
                                                    - (s IDgx_install_CIEABC_i)) (1)]
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
                                                                 - (s IDgx_install_CIEABC_j))) (F_check_ge (0) (0))]
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                   - 
                                                                   (s IDgx_install_CIEABC_j))) (F_check_ge (3
                                                                    - (s IDgx_install_CIEABC_j)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDgx_install_CIEABC_j)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDgx_install_CIEABC_j)))]
    | _ => []
  end.


Theorem gx_install_CIEABC_ai_correct:
  forall s p' s', steps (g_start gx_install_CIEABC) s (g_edges gx_install_CIEABC) p' s' -> gx_install_CIEABC_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_install_CIEABC_pot_correct:
  forall s p' s',
    steps (g_start gx_install_CIEABC) s (g_edges gx_install_CIEABC) p' s' ->
    (gx_install_CIEABC_pot (g_start gx_install_CIEABC) s >= gx_install_CIEABC_pot p' s')%Q.
Proof.
  check_lp gx_install_CIEABC_ai_correct gx_install_CIEABC_hints.
Qed.

