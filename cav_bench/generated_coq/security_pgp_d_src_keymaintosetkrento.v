Require Import pasta.Pasta.

Notation IDsetkrent_z := 1%positive.
Notation IDsetkrent__tmp := 2%positive.
Notation IDsetkrent_i := 3%positive.
Notation IDsetkrent_nkr := 4%positive.
Notation IDsetkrent_keyring := 5%positive.
Definition setkrent : graph := {|
  g_start := 1%positive;
  g_end := 32%positive;
  g_edges := (1%positive,(AAssign IDsetkrent_z (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDsetkrent_nkr) s) <
             (eval (ENum (8)) s))%Z)),7%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDsetkrent_nkr) s) >=
             (eval (ENum (8)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,32%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDsetkrent_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsetkrent_i) s) <
             (eval (EVar IDsetkrent_nkr) s))%Z)),21%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsetkrent_i) s) >=
             (eval (EVar IDsetkrent_nkr) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDsetkrent_nkr
             (Some (EAdd (EVar IDsetkrent_nkr) (ENum (1))))),18%positive)::
             (18%positive,(AAssign IDsetkrent__tmp (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,32%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,29%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDsetkrent_i
             (Some (EAdd (EVar IDsetkrent_i) (ENum (1))))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDsetkrent_z (Some (EAdd (ENum (1))
             (EVar IDsetkrent_z)))),28%positive)::
             (28%positive,AWeaken,15%positive)::
             (29%positive,(AAssign IDsetkrent__tmp (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::nil
|}.

Definition setkrent_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 4%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_nkr) + 8 <= 0)%Z
    | 5%positive => (-1 * (s IDsetkrent_nkr) + 8 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 6%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_nkr) + 8 <= 0)%Z
    | 7%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 8%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 9%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 10%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 11%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 12%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 13%positive => (1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0 /\ 1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_i) <= 0)%Z
    | 14%positive => (-1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_z) <= 0)%Z
    | 15%positive => (-1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 16%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i)+ 1 * (s IDsetkrent_nkr) <= 0)%Z
    | 17%positive => (-1 * (s IDsetkrent_i)+ 1 * (s IDsetkrent_nkr) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 18%positive => (-1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i)+ 1 * (s IDsetkrent_nkr) + -1 <= 0 /\ 1 * (s IDsetkrent_nkr) + -8 <= 0)%Z
    | 19%positive => (1 * (s IDsetkrent_nkr) + -8 <= 0 /\ -1 * (s IDsetkrent_i)+ 1 * (s IDsetkrent_nkr) + -1 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent__tmp) <= 0 /\ -1 * (s IDsetkrent__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDsetkrent__tmp) <= 0 /\ 1 * (s IDsetkrent__tmp) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i)+ 1 * (s IDsetkrent_nkr) + -1 <= 0 /\ 1 * (s IDsetkrent_nkr) + -8 <= 0)%Z
    | 21%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 25%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) <= 0 /\ -1 * (s IDsetkrent_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDsetkrent_i) + 1 <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) <= 0 /\ -1 * (s IDsetkrent_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDsetkrent_i) + 1 <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_z) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0 /\ 1 * (s IDsetkrent__tmp) <= 0 /\ -1 * (s IDsetkrent__tmp) <= 0)%Z
    | 31%positive => (-1 * (s IDsetkrent__tmp) <= 0 /\ 1 * (s IDsetkrent__tmp) <= 0 /\ 1 * (s IDsetkrent_nkr) + -7 <= 0 /\ -1 * (s IDsetkrent_i) <= 0 /\ -1 * (s IDsetkrent_z) <= 0 /\ 1 * (s IDsetkrent_i)+ -1 * (s IDsetkrent_nkr) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDsetkrent_z) <= 0)%Z
    | _ => False
  end.

Definition setkrent_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDsetkrent_nkr)))%Q
    | 2%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 3%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 4%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 5%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 6%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 7%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 8%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 9%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 10%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 11%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 12%positive => ((s IDsetkrent_z) + max0((s IDsetkrent_nkr)))%Q
    | 13%positive => ((s IDsetkrent_z)
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 14%positive => ((s IDsetkrent_z)
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 15%positive => ((s IDsetkrent_z)
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 16%positive => ((s IDsetkrent_z)
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 17%positive => ((s IDsetkrent_z))%Q
    | 18%positive => ((s IDsetkrent_z))%Q
    | 19%positive => ((s IDsetkrent_z))%Q
    | 20%positive => ((s IDsetkrent_z))%Q
    | 21%positive => ((s IDsetkrent_z)
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 22%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 23%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 24%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 25%positive => ((s IDsetkrent_z)
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr))
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 26%positive => ((s IDsetkrent_z)
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr))
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 27%positive => ((s IDsetkrent_z)
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr))
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 28%positive => (-(1 # 1) + (s IDsetkrent_z)
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr))
                      + max0(-(s IDsetkrent_i) + (s IDsetkrent_nkr)))%Q
    | 29%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 30%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 31%positive => ((s IDsetkrent_z)
                      + max0(-1 - (s IDsetkrent_i) + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(-1 + (s IDsetkrent_nkr))
                      + (1 # 7) * max0(8 - (s IDsetkrent_nkr)))%Q
    | 32%positive => ((s IDsetkrent_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition setkrent_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_ge_0 ((s IDsetkrent_nkr))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDsetkrent_i)
                                                             + (s IDsetkrent_nkr)) (-1
                                                                    - (s IDsetkrent_i)
                                                                    + (s IDsetkrent_nkr)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDsetkrent_i)
                                                                 + (s IDsetkrent_nkr))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_max0_pre_decrement (-(s IDsetkrent_i)
                                                    + (s IDsetkrent_nkr)) (1);
                      (*0 0.142857*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDsetkrent_nkr)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDsetkrent_nkr)));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsetkrent_nkr)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsetkrent_nkr)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDsetkrent_nkr))) (F_check_ge (8
                                                                    - (s IDsetkrent_nkr)) (0));
                      (*0 0.142857*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsetkrent_nkr))) (F_check_ge (-1
                                                                    + (s IDsetkrent_nkr)) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_ge_0 (-1 - (s IDsetkrent_i)
                                            + (s IDsetkrent_nkr));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    - (s IDsetkrent_nkr))) (F_check_ge (0) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsetkrent_nkr))) (F_check_ge (0) (0))]
    | 32%positive => []
    | _ => []
  end.


Theorem setkrent_ai_correct:
  forall s p' s', steps (g_start setkrent) s (g_edges setkrent) p' s' -> setkrent_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem setkrent_pot_correct:
  forall s p' s',
    steps (g_start setkrent) s (g_edges setkrent) p' s' ->
    (setkrent_pot (g_start setkrent) s >= setkrent_pot p' s')%Q.
Proof.
  check_lp setkrent_ai_correct setkrent_hints.
Qed.

