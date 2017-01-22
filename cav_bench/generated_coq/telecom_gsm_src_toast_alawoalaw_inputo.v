Require Import pasta.Pasta.

Notation IDalaw_input_z := 1%positive.
Notation IDalaw_input__tmp := 2%positive.
Notation IDalaw_input_c := 3%positive.
Notation IDalaw_input_i := 4%positive.
Notation IDalaw_input_buf := 5%positive.
Definition alaw_input : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign IDalaw_input_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDalaw_input_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDalaw_input_i) s) <
             (eval (ENum (160)) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDalaw_input_i) s) >=
             (eval (ENum (160)) s))%Z)),6%positive)::
             (6%positive,AWeaken,13%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDalaw_input_c None),9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => True)),24%positive)::
             (11%positive,(AGuard (fun s => True)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDalaw_input_c) s) =
             (eval (ENum (-1)) s))%Z)),15%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDalaw_input_c)
             s) <> (eval (ENum (-1)) s))%Z)),14%positive)::
             (14%positive,AWeaken,17%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDalaw_input__tmp
             (Some (EVar IDalaw_input_i))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,23%positive)::
             (20%positive,(AAssign IDalaw_input__tmp (Some (ENum (-1)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDalaw_input_i
             (Some (EAdd (EVar IDalaw_input_i) (ENum (1))))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDalaw_input_z (Some (EAdd (ENum (1))
             (EVar IDalaw_input_z)))),30%positive)::
             (30%positive,AWeaken,5%positive)::nil
|}.

Definition alaw_input_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_z) <= 0)%Z
    | 3%positive => (-1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 4%positive => (-1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_z) <= 0)%Z
    | 5%positive => (-1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0)%Z
    | 6%positive => (1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) + 160 <= 0)%Z
    | 7%positive => (-1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -159 <= 0)%Z
    | 8%positive => (1 * (s IDalaw_input_i) + -159 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 9%positive => (-1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -159 <= 0)%Z
    | 10%positive => (1 * (s IDalaw_input_i) + -159 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 11%positive => (-1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -159 <= 0)%Z
    | 12%positive => (1 * (s IDalaw_input_i) + -159 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 13%positive => (1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0)%Z
    | 14%positive => (-1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0)%Z
    | 15%positive => (-1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ 1 * (s IDalaw_input_c) + 1 <= 0 /\ -1 * (s IDalaw_input_c) + -1 <= 0)%Z
    | 16%positive => (-1 * (s IDalaw_input_c) + -1 <= 0 /\ 1 * (s IDalaw_input_c) + 1 <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0)%Z
    | 17%positive => (-1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0)%Z
    | 18%positive => (1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input__tmp) + -160 <= 0 /\ -1 * (s IDalaw_input__tmp) <= 0)%Z
    | 19%positive => (-1 * (s IDalaw_input__tmp) <= 0 /\ 1 * (s IDalaw_input__tmp) + -160 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0)%Z
    | 20%positive => (-1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ 1 * (s IDalaw_input_c) + 1 <= 0 /\ -1 * (s IDalaw_input_c) + -1 <= 0)%Z
    | 21%positive => (-1 * (s IDalaw_input_c) + -1 <= 0 /\ 1 * (s IDalaw_input_c) + 1 <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input__tmp) + 1 <= 0 /\ -1 * (s IDalaw_input__tmp) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDalaw_input__tmp) + -1 <= 0 /\ 1 * (s IDalaw_input__tmp) + 1 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ 1 * (s IDalaw_input_c) + 1 <= 0 /\ -1 * (s IDalaw_input_c) + -1 <= 0)%Z
    | 23%positive => (1 * (s IDalaw_input__tmp) + -160 <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input__tmp) + -1 <= 0)%Z
    | 24%positive => (1 * (s IDalaw_input_i) + -159 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 25%positive => (-1 * (s IDalaw_input_i) <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -159 <= 0)%Z
    | 26%positive => (1 * (s IDalaw_input_i) + -159 <= 0 /\ -1 * (s IDalaw_input_z) <= 0 /\ -1 * (s IDalaw_input_i) <= 0)%Z
    | 27%positive => (-1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDalaw_input_i) + 1 <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_z) <= 0)%Z
    | 29%positive => (-1 * (s IDalaw_input_z) <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDalaw_input_i) + 1 <= 0 /\ 1 * (s IDalaw_input_i) + -160 <= 0 /\ -1 * (s IDalaw_input_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition alaw_input_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((160 # 1))%Q
    | 2%positive => ((160 # 1) + (s IDalaw_input_z))%Q
    | 3%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 4%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 5%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 6%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 7%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 8%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 9%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 10%positive => ((s IDalaw_input_z) + max0(160 - (s IDalaw_input_i)))%Q
    | 11%positive => (-(1 # 1) + (s IDalaw_input_z)
                      + (1 # 158) * max0(1 + (s IDalaw_input_i))
                      + (1 # 158) * max0(159 - (s IDalaw_input_i))
                      + max0(160 - (s IDalaw_input_i)))%Q
    | 12%positive => (-(1 # 1) + (s IDalaw_input_z)
                      + (1 # 158) * max0(1 + (s IDalaw_input_i))
                      + (1 # 158) * max0(159 - (s IDalaw_input_i))
                      + max0(160 - (s IDalaw_input_i)))%Q
    | 13%positive => ((s IDalaw_input_z))%Q
    | 14%positive => ((s IDalaw_input_z))%Q
    | 15%positive => ((s IDalaw_input_z))%Q
    | 16%positive => ((s IDalaw_input_z))%Q
    | 17%positive => ((s IDalaw_input_z))%Q
    | 18%positive => ((s IDalaw_input_z))%Q
    | 19%positive => ((s IDalaw_input_z))%Q
    | 20%positive => ((s IDalaw_input_z))%Q
    | 21%positive => ((s IDalaw_input_z))%Q
    | 22%positive => ((s IDalaw_input_z))%Q
    | 23%positive => ((s IDalaw_input_z))%Q
    | 24%positive => (-(1 # 1) + (s IDalaw_input_z)
                      + (1 # 158) * max0(1 + (s IDalaw_input_i))
                      + (1 # 158) * max0(159 - (s IDalaw_input_i))
                      + max0(160 - (s IDalaw_input_i)))%Q
    | 25%positive => ((157 # 158) - (1 # 158) * (s IDalaw_input_i)
                      + (s IDalaw_input_z)
                      + (1 # 158) * max0(1 + (s IDalaw_input_i))
                      + max0(159 - (s IDalaw_input_i)))%Q
    | 26%positive => ((157 # 158) - (1 # 158) * (s IDalaw_input_i)
                      + (s IDalaw_input_z)
                      + (1 # 158) * max0(1 + (s IDalaw_input_i))
                      + max0(159 - (s IDalaw_input_i)))%Q
    | 27%positive => ((1 # 1) - (1 # 158) * (s IDalaw_input_i)
                      + (s IDalaw_input_z) + max0(160 - (s IDalaw_input_i))
                      + (1 # 158) * max0((s IDalaw_input_i)))%Q
    | 28%positive => ((1 # 1) - (1 # 158) * (s IDalaw_input_i)
                      + (s IDalaw_input_z) + max0(160 - (s IDalaw_input_i))
                      + (1 # 158) * max0((s IDalaw_input_i)))%Q
    | 29%positive => ((1 # 1) - (1 # 158) * (s IDalaw_input_i)
                      + (s IDalaw_input_z) + max0(160 - (s IDalaw_input_i))
                      + (1 # 158) * max0((s IDalaw_input_i)))%Q
    | 30%positive => (-(1 # 158) * (s IDalaw_input_i) + (s IDalaw_input_z)
                      + max0(160 - (s IDalaw_input_i))
                      + (1 # 158) * max0((s IDalaw_input_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition alaw_input_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (160
                                                            - (s IDalaw_input_i)) (159
                                                                    - (s IDalaw_input_i)));
                     (*-1 0*) F_max0_ge_0 (159 - (s IDalaw_input_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-0.00625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (159
                                                                    - (s IDalaw_input_i)) (0))) (F_max0_ge_0 (159
                                                                    - (s IDalaw_input_i)));
                      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDalaw_input_i)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDalaw_input_i)))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement (160 - (s IDalaw_input_i)) (1);
                      (*-1.00625 0*) F_max0_ge_0 (159 - (s IDalaw_input_i));
                      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDalaw_input_i))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_pre_decrement (160 - (s IDalaw_input_i)) (1);
                      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_arg (159
                                                                    - (s IDalaw_input_i))) (F_check_ge (159
                                                                    - (s IDalaw_input_i)) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDalaw_input_i))) (F_check_ge ((s IDalaw_input_i)) (0))]
    | _ => []
  end.


Theorem alaw_input_ai_correct:
  forall s p' s', steps (g_start alaw_input) s (g_edges alaw_input) p' s' -> alaw_input_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem alaw_input_pot_correct:
  forall s p' s',
    steps (g_start alaw_input) s (g_edges alaw_input) p' s' ->
    (alaw_input_pot (g_start alaw_input) s >= alaw_input_pot p' s')%Q.
Proof.
  check_lp alaw_input_ai_correct alaw_input_hints.
Qed.

