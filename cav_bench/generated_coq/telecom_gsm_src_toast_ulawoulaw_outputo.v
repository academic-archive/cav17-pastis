Require Import pasta.Pasta.

Notation IDulaw_output_z := 1%positive.
Notation IDulaw_output__tmp := 2%positive.
Notation IDulaw_output_i := 3%positive.
Notation IDulaw_output_buf := 4%positive.
Definition ulaw_output : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDulaw_output_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDulaw_output_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDulaw_output_i) s) <
             (eval (ENum (160)) s))%Z)),10%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDulaw_output_i)
             s) >= (eval (ENum (160)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDulaw_output__tmp (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,21%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,18%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDulaw_output_i
             (Some (EAdd (EVar IDulaw_output_i) (ENum (1))))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDulaw_output_z (Some (EAdd (ENum (1))
             (EVar IDulaw_output_z)))),17%positive)::
             (17%positive,AWeaken,5%positive)::
             (18%positive,(AAssign IDulaw_output__tmp (Some (ENum (-1)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::nil
|}.

Definition ulaw_output_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_z) <= 0)%Z
    | 3%positive => (-1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output_i) <= 0)%Z
    | 4%positive => (-1 * (s IDulaw_output_i) <= 0 /\ 1 * (s IDulaw_output_i) <= 0 /\ 1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_z) <= 0)%Z
    | 5%positive => (-1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0)%Z
    | 6%positive => (1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) + 160 <= 0)%Z
    | 7%positive => (-1 * (s IDulaw_output_i) + 160 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0)%Z
    | 8%positive => (1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) + 160 <= 0 /\ 1 * (s IDulaw_output__tmp) <= 0 /\ -1 * (s IDulaw_output__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDulaw_output__tmp) <= 0 /\ 1 * (s IDulaw_output__tmp) <= 0 /\ -1 * (s IDulaw_output_i) + 160 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0)%Z
    | 10%positive => (-1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -159 <= 0)%Z
    | 11%positive => (1 * (s IDulaw_output_i) + -159 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) <= 0)%Z
    | 12%positive => (-1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -159 <= 0)%Z
    | 13%positive => (1 * (s IDulaw_output_i) + -159 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) <= 0)%Z
    | 14%positive => (-1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDulaw_output_i) + 1 <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_z) <= 0)%Z
    | 16%positive => (-1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDulaw_output_i) + 1 <= 0 /\ 1 * (s IDulaw_output_i) + -160 <= 0 /\ -1 * (s IDulaw_output_z) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -159 <= 0)%Z
    | 19%positive => (1 * (s IDulaw_output_i) + -159 <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) <= 0 /\ 1 * (s IDulaw_output__tmp) + 1 <= 0 /\ -1 * (s IDulaw_output__tmp) + -1 <= 0)%Z
    | 20%positive => (-1 * (s IDulaw_output__tmp) + -1 <= 0 /\ 1 * (s IDulaw_output__tmp) + 1 <= 0 /\ -1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ 1 * (s IDulaw_output_i) + -159 <= 0)%Z
    | 21%positive => (1 * (s IDulaw_output_i) + -160 <= 0 /\ 1 * (s IDulaw_output__tmp) <= 0 /\ -1 * (s IDulaw_output_z) <= 0 /\ -1 * (s IDulaw_output_i) <= 0 /\ -1 * (s IDulaw_output__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition ulaw_output_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((160 # 1))%Q
    | 2%positive => ((160 # 1) + (s IDulaw_output_z))%Q
    | 3%positive => ((160 # 1) - (158 # 157) * (s IDulaw_output_i)
                     + (s IDulaw_output_z)
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 4%positive => ((160 # 1) - (158 # 157) * (s IDulaw_output_i)
                     + (s IDulaw_output_z)
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 5%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                     + max0(160 - (s IDulaw_output_i))
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 6%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                     + max0(160 - (s IDulaw_output_i))
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 7%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                     + max0(160 - (s IDulaw_output_i))
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 8%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                     + max0(160 - (s IDulaw_output_i))
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 9%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                     + max0(160 - (s IDulaw_output_i))
                     + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 10%positive => (-(1 # 157) * (s IDulaw_output_i) + (s IDulaw_output_z)
                      + max0(160 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 11%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 12%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 13%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 14%positive => ((s IDulaw_output_z)
                      + (1 # 157) * max0(-1 + (s IDulaw_output_i))
                      + (158 # 157) * max0(160 - (s IDulaw_output_i)))%Q
    | 15%positive => ((s IDulaw_output_z)
                      + (1 # 157) * max0(-1 + (s IDulaw_output_i))
                      + (158 # 157) * max0(160 - (s IDulaw_output_i)))%Q
    | 16%positive => ((s IDulaw_output_z)
                      + (1 # 157) * max0(-1 + (s IDulaw_output_i))
                      + (158 # 157) * max0(160 - (s IDulaw_output_i)))%Q
    | 17%positive => (-(1 # 1) + (s IDulaw_output_z)
                      + (1 # 157) * max0(-1 + (s IDulaw_output_i))
                      + (158 # 157) * max0(160 - (s IDulaw_output_i)))%Q
    | 18%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 19%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 20%positive => ((s IDulaw_output_z)
                      + (158 # 157) * max0(159 - (s IDulaw_output_i))
                      + (1 # 157) * max0((s IDulaw_output_i)))%Q
    | 21%positive => ((s IDulaw_output_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition ulaw_output_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (160
                                                                    - (s IDulaw_output_i)) (0))) (F_max0_ge_0 (160
                                                                    - (s IDulaw_output_i)))]
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (160
                                                            - (s IDulaw_output_i)) (159
                                                                    - (s IDulaw_output_i)));
                     (*-1 0*) F_max0_ge_0 (159 - (s IDulaw_output_i));
                     (*-0.00628931 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDulaw_output_i))) (F_check_ge ((s IDulaw_output_i)) (0))]
    | 10%positive => [(*0 1*) F_max0_pre_decrement (160 - (s IDulaw_output_i)) (1);
                      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (159
                                                                    - (s IDulaw_output_i)) (0))) (F_max0_ge_0 (159
                                                                    - (s IDulaw_output_i)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-0.00628931 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDulaw_output_i)) (0))) (F_max0_ge_0 ((s IDulaw_output_i)));
                      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_ge_arg (160
                                                                    - (s IDulaw_output_i))) (F_check_ge (160
                                                                    - (s IDulaw_output_i)) (0));
                      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDulaw_output_i))) (F_check_ge (-1
                                                                    + (s IDulaw_output_i)) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1.00629 0*) F_max0_ge_0 (159 - (s IDulaw_output_i));
                      (*-0.00628931 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDulaw_output_i))) (F_check_ge (0) (0))]
    | 21%positive => []
    | _ => []
  end.


Theorem ulaw_output_ai_correct:
  forall s p' s', steps (g_start ulaw_output) s (g_edges ulaw_output) p' s' -> ulaw_output_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ulaw_output_pot_correct:
  forall s p' s',
    steps (g_start ulaw_output) s (g_edges ulaw_output) p' s' ->
    (ulaw_output_pot (g_start ulaw_output) s >= ulaw_output_pot p' s')%Q.
Proof.
  check_lp ulaw_output_ai_correct ulaw_output_hints.
Qed.

