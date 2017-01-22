Require Import pasta.Pasta.

Notation IDcie_joint_caches_complete_z := 1%positive.
Notation IDcie_joint_caches_complete_j := 2%positive.
Notation IDcie_joint_caches_complete_pcie := 3%positive.
Notation IDcie_joint_caches_complete_pcier := 4%positive.
Notation IDcie_joint_caches_complete_pjc := 5%positive.
Definition cie_joint_caches_complete : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDcie_joint_caches_complete_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcie_joint_caches_complete_j
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_complete_j) s) <
             (eval (ENum (3)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_joint_caches_complete_j) s) >=
             (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDcie_joint_caches_complete_j
             (Some (EAdd (EVar IDcie_joint_caches_complete_j) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDcie_joint_caches_complete_z
             (Some (EAdd (ENum (1)) (EVar IDcie_joint_caches_complete_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition cie_joint_caches_complete_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_joint_caches_complete_z) <= 0 /\ 1 * (s IDcie_joint_caches_complete_z) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_joint_caches_complete_j) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) <= 0 /\ 1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -3 <= 0)%Z
    | 6%positive => (1 * (s IDcie_joint_caches_complete_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDcie_joint_caches_complete_j) + 3 <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -3 <= 0)%Z
    | 8%positive => (-1 * (s IDcie_joint_caches_complete_j) <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -2 <= 0)%Z
    | 9%positive => (1 * (s IDcie_joint_caches_complete_j) + -2 <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_joint_caches_complete_j) <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -2 <= 0)%Z
    | 11%positive => (-1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -3 <= 0)%Z
    | 12%positive => (1 * (s IDcie_joint_caches_complete_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) + 1 <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcie_joint_caches_complete_z) <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) + 1 <= 0 /\ 1 * (s IDcie_joint_caches_complete_j) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDcie_joint_caches_complete_j) + -3 <= 0 /\ -1 * (s IDcie_joint_caches_complete_j) + 1 <= 0 /\ -1 * (s IDcie_joint_caches_complete_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cie_joint_caches_complete_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((3 # 1))%Q
    | 2%positive => ((3 # 1) + (s IDcie_joint_caches_complete_z))%Q
    | 3%positive => ((s IDcie_joint_caches_complete_z)
                     + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 4%positive => ((s IDcie_joint_caches_complete_z)
                     + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 5%positive => ((s IDcie_joint_caches_complete_z)
                     + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 6%positive => ((s IDcie_joint_caches_complete_z)
                     + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 7%positive => ((s IDcie_joint_caches_complete_z))%Q
    | 8%positive => ((s IDcie_joint_caches_complete_z)
                     + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 9%positive => ((1 # 1) + (s IDcie_joint_caches_complete_z)
                     + max0(2 - (s IDcie_joint_caches_complete_j)))%Q
    | 10%positive => ((1 # 1) + (s IDcie_joint_caches_complete_z)
                      + max0(2 - (s IDcie_joint_caches_complete_j)))%Q
    | 11%positive => ((1 # 1) + (s IDcie_joint_caches_complete_z)
                      + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 12%positive => ((1 # 1) + (s IDcie_joint_caches_complete_z)
                      + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 13%positive => ((1 # 1) + (s IDcie_joint_caches_complete_z)
                      + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | 14%positive => ((s IDcie_joint_caches_complete_z)
                      + max0(3 - (s IDcie_joint_caches_complete_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_joint_caches_complete_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                            - (s IDcie_joint_caches_complete_j)) (2
                                                                    - (s IDcie_joint_caches_complete_j)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                - (s IDcie_joint_caches_complete_j))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                 - (s IDcie_joint_caches_complete_j))) (F_check_ge (3
                                                                    - (s IDcie_joint_caches_complete_j)) (0));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDcie_joint_caches_complete_j)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDcie_joint_caches_complete_j)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem cie_joint_caches_complete_ai_correct:
  forall s p' s', steps (g_start cie_joint_caches_complete) s (g_edges cie_joint_caches_complete) p' s' -> cie_joint_caches_complete_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_joint_caches_complete_pot_correct:
  forall s p' s',
    steps (g_start cie_joint_caches_complete) s (g_edges cie_joint_caches_complete) p' s' ->
    (cie_joint_caches_complete_pot (g_start cie_joint_caches_complete) s >= cie_joint_caches_complete_pot p' s')%Q.
Proof.
  check_lp cie_joint_caches_complete_ai_correct cie_joint_caches_complete_hints.
Qed.

