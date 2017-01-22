Require Import pasta.Pasta.

Notation IDrandomunit_z := 1%positive.
Notation IDrandomunit_i := 2%positive.
Notation IDrandomunit_u := 3%positive.
Definition randomunit : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDrandomunit_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDrandomunit_u (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDrandomunit_i (Some (ENum (2)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDrandomunit_u None),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDrandomunit_i
             (Some (EAdd (EVar IDrandomunit_i) (ENum (-1))))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EAdd (EVar IDrandomunit_i)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EAdd (EVar IDrandomunit_i)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDrandomunit_z (Some (EAdd (ENum (1))
             (EVar IDrandomunit_z)))),5%positive)::nil
|}.

Definition randomunit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_u) <= 0 /\ -1 * (s IDrandomunit_u) <= 0)%Z
    | 4%positive => (-1 * (s IDrandomunit_u) <= 0 /\ 1 * (s IDrandomunit_u) <= 0 /\ 1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_i) + -2 <= 0 /\ -1 * (s IDrandomunit_i) + 2 <= 0)%Z
    | 5%positive => (-1 * (s IDrandomunit_i) + 2 <= 0 /\ 1 * (s IDrandomunit_i) + -2 <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_u) <= 0 /\ -1 * (s IDrandomunit_u) <= 0)%Z
    | 6%positive => (1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_i) + -2 <= 0 /\ -1 * (s IDrandomunit_i) + 2 <= 0)%Z
    | 7%positive => (-1 * (s IDrandomunit_i) + 2 <= 0 /\ 1 * (s IDrandomunit_i) + -2 <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_z) <= 0)%Z
    | 8%positive => (1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_i) + 1 <= 0 /\ 1 * (s IDrandomunit_i) + -1 <= 0)%Z
    | 9%positive => (1 * (s IDrandomunit_i) + -1 <= 0 /\ -1 * (s IDrandomunit_i) + 1 <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_z) <= 0)%Z
    | 10%positive => (1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ -1 * (s IDrandomunit_i) + 1 <= 0 /\ 1 * (s IDrandomunit_i) + -1 <= 0)%Z
    | 11%positive => (1 * (s IDrandomunit_i) + -1 <= 0 /\ -1 * (s IDrandomunit_i) + 1 <= 0 /\ -1 * (s IDrandomunit_z) <= 0 /\ 1 * (s IDrandomunit_z) <= 0)%Z
    | 12%positive => (False)%Z
    | 13%positive => (False)%Z
    | 14%positive => (False)%Z
    | _ => False
  end.

Definition randomunit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (0)%Q
    | 2%positive => (0)%Q
    | 3%positive => (0)%Q
    | 4%positive => (0)%Q
    | 5%positive => (0)%Q
    | 6%positive => (0)%Q
    | 7%positive => (0)%Q
    | 8%positive => (0)%Q
    | 9%positive => ((s IDrandomunit_z) + max0(-(s IDrandomunit_z)))%Q
    | 10%positive => ((s IDrandomunit_z) + max0(-(s IDrandomunit_z)))%Q
    | 11%positive => ((s IDrandomunit_z))%Q
    | 12%positive => ((s IDrandomunit_z) + max0(-(s IDrandomunit_z)))%Q
    | 13%positive => (0)%Q
    | 14%positive => (0)%Q
    | _ => (0 # 1)%Q
  end.

Definition randomunit_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDrandomunit_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDrandomunit_z)))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDrandomunit_z))) (F_check_ge (0) (0))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDrandomunit_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDrandomunit_z)) (0))) (F_max0_ge_0 ((s IDrandomunit_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDrandomunit_z))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem randomunit_ai_correct:
  forall s p' s', steps (g_start randomunit) s (g_edges randomunit) p' s' -> randomunit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem randomunit_pot_correct:
  forall s p' s',
    steps (g_start randomunit) s (g_edges randomunit) p' s' ->
    (randomunit_pot (g_start randomunit) s >= randomunit_pot p' s')%Q.
Proof.
  check_lp randomunit_ai_correct randomunit_hints.
Qed.

