Require Import pasta.Pasta.

Notation IDslowtest_z := 1%positive.
Notation IDslowtest__tmp := 2%positive.
Notation IDslowtest_i := 3%positive.
Notation IDslowtest_p := 4%positive.
Definition slowtest : graph := {|
  g_start := 1%positive;
  g_end := 28%positive;
  g_edges := (1%positive,(AAssign IDslowtest_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDslowtest_i (Some (ENum (0)))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDslowtest_i) s) <
             (eval (ENum (4)) s))%Z)),10%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDslowtest_i) s) >=
             (eval (ENum (4)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDslowtest__tmp (Some (ENum (1)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,28%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,25%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,22%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,22%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDslowtest_i
             (Some (EAdd (EVar IDslowtest_i) (ENum (1))))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDslowtest_z (Some (EAdd (ENum (1))
             (EVar IDslowtest_z)))),21%positive)::
             (21%positive,AWeaken,5%positive)::
             (22%positive,(AAssign IDslowtest__tmp (Some (ENum (0)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,28%positive)::
             (25%positive,(AAssign IDslowtest__tmp (Some (ENum (0)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::nil
|}.

Definition slowtest_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_z) <= 0)%Z
    | 3%positive => (-1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_i) <= 0)%Z
    | 4%positive => (-1 * (s IDslowtest_i) <= 0 /\ 1 * (s IDslowtest_i) <= 0 /\ 1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_z) <= 0)%Z
    | 5%positive => (-1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0)%Z
    | 6%positive => (1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) + 4 <= 0)%Z
    | 7%positive => (-1 * (s IDslowtest_i) + 4 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0)%Z
    | 8%positive => (1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) + 4 <= 0 /\ 1 * (s IDslowtest__tmp) + -1 <= 0 /\ -1 * (s IDslowtest__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDslowtest__tmp) + 1 <= 0 /\ 1 * (s IDslowtest__tmp) + -1 <= 0 /\ -1 * (s IDslowtest_i) + 4 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0)%Z
    | 10%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 11%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0)%Z
    | 12%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0)%Z
    | 14%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0)%Z
    | 16%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 17%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0)%Z
    | 18%positive => (-1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDslowtest_i) + 1 <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_z) <= 0)%Z
    | 20%positive => (-1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDslowtest_i) + 1 <= 0 /\ 1 * (s IDslowtest_i) + -4 <= 0 /\ -1 * (s IDslowtest_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 23%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ 1 * (s IDslowtest__tmp) <= 0 /\ -1 * (s IDslowtest__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDslowtest__tmp) <= 0 /\ 1 * (s IDslowtest__tmp) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 25%positive => (-1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 26%positive => (1 * (s IDslowtest_i) + -3 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ 1 * (s IDslowtest__tmp) <= 0 /\ -1 * (s IDslowtest__tmp) <= 0)%Z
    | 27%positive => (-1 * (s IDslowtest__tmp) <= 0 /\ 1 * (s IDslowtest__tmp) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ 1 * (s IDslowtest_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDslowtest_i) + -4 <= 0 /\ 1 * (s IDslowtest__tmp) + -1 <= 0 /\ -1 * (s IDslowtest_z) <= 0 /\ -1 * (s IDslowtest_i) <= 0 /\ -1 * (s IDslowtest__tmp) <= 0)%Z
    | _ => False
  end.

Definition slowtest_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDslowtest_z))%Q
    | 3%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 4%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 5%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 6%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 7%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 8%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 9%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 10%positive => ((s IDslowtest_z) + max0(4 - (s IDslowtest_i)))%Q
    | 11%positive => ((1 # 1) + max0(3 - (s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 12%positive => ((1 # 1) + max0(3 - (s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 13%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 14%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 15%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 16%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 17%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 18%positive => ((4 # 3) - (1 # 3) * (s IDslowtest_i)
                      + (1 # 3) * max0(-1 + (s IDslowtest_i))
                      + max0(4 - (s IDslowtest_i)) + max0((s IDslowtest_z)))%Q
    | 19%positive => ((4 # 3) - (1 # 3) * (s IDslowtest_i)
                      + (1 # 3) * max0(-1 + (s IDslowtest_i))
                      + max0(4 - (s IDslowtest_i)) + max0((s IDslowtest_z)))%Q
    | 20%positive => ((4 # 3) - (1 # 3) * (s IDslowtest_i)
                      + (1 # 3) * max0(-1 + (s IDslowtest_i))
                      + max0(4 - (s IDslowtest_i)) + max0((s IDslowtest_z)))%Q
    | 21%positive => ((4 # 3) - (1 # 3) * (s IDslowtest_i)
                      + (1 # 3) * max0(-1 + (s IDslowtest_i))
                      + max0(-1 + (s IDslowtest_z))
                      + max0(4 - (s IDslowtest_i)))%Q
    | 22%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 23%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 24%positive => ((1 # 1) - (1 # 3) * (s IDslowtest_i)
                      + max0(3 - (s IDslowtest_i))
                      + (1 # 3) * max0((s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 25%positive => ((1 # 1) + max0(3 - (s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 26%positive => ((1 # 1) + max0(3 - (s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 27%positive => ((1 # 1) + max0(3 - (s IDslowtest_i))
                      + max0((s IDslowtest_z)))%Q
    | 28%positive => ((s IDslowtest_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition slowtest_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDslowtest_i)) (3
                                                                    - (s IDslowtest_i)));
                     (*-1 0*) F_max0_ge_0 (3 - (s IDslowtest_i))]
    | 10%positive => [(*0 1*) F_max0_pre_decrement (4 - (s IDslowtest_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDslowtest_z)) (0))) (F_max0_ge_0 ((s IDslowtest_z)))]
    | 11%positive => []
    | 12%positive => [(*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDslowtest_i)) (0))) (F_max0_ge_0 ((s IDslowtest_i)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDslowtest_z))) (F_check_ge (-1
                                                                    + (s IDslowtest_z)) (0));
                      (*0 0.333333*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDslowtest_i))) (F_check_ge (-1
                                                                    + (s IDslowtest_i)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-0.333333 0*) F_max0_pre_decrement (4
                                                            - (s IDslowtest_i)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDslowtest_z))) (F_check_ge ((s IDslowtest_z)) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDslowtest_i))) (F_check_ge (0) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDslowtest_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDslowtest_i)));
                      (*-1.33333 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDslowtest_i))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (3 - (s IDslowtest_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDslowtest_z))) (F_check_ge ((s IDslowtest_z)) (0))]
    | 28%positive => []
    | _ => []
  end.


Theorem slowtest_ai_correct:
  forall s p' s', steps (g_start slowtest) s (g_edges slowtest) p' s' -> slowtest_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem slowtest_pot_correct:
  forall s p' s',
    steps (g_start slowtest) s (g_edges slowtest) p' s' ->
    (slowtest_pot (g_start slowtest) s >= slowtest_pot p' s')%Q.
Proof.
  check_lp slowtest_ai_correct slowtest_hints.
Qed.

