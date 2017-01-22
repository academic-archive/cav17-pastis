Require Import pasta.Pasta.

Notation IDmain1_z := 1%positive.
Notation IDmain1__tmp := 2%positive.
Notation IDmain1__tmp1 := 3%positive.
Notation IDmain1_argc := 4%positive.
Notation IDmain1_argv := 5%positive.
Notation IDmain1_print := 6%positive.
Definition main1 : graph := {|
  g_start := 1%positive;
  g_end := 34%positive;
  g_edges := (1%positive,(AAssign IDmain1_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmain1__tmp (Some (EVar IDmain1_argc))),
             3%positive)::
             (3%positive,(AAssign IDmain1__tmp1 (Some (EVar IDmain1_print))),
             4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) <
             (eval (ENum (2)) s))%Z)),27%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) >=
             (eval (ENum (2)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,(AAssign IDmain1__tmp
             (Some (EAdd (EVar IDmain1__tmp) (ENum (-1))))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmain1__tmp)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),14%positive)::
             (10%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmain1__tmp)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,34%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,25%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),19%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) =
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,21%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDmain1_z (Some (EAdd (ENum (1))
             (EVar IDmain1_z)))),8%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,34%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),30%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) =
             (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,32%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::nil
|}.

Definition main1_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 4%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 6%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 7%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 9%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDmain1__tmp) + 1 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 1 <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 12%positive => (1 * (s IDmain1__tmp) + -1 <= 0 /\ -1 * (s IDmain1__tmp) + 1 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 1 <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 14%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 15%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 17%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0 /\ 1 * (s IDmain1__tmp1) <= 0 /\ -1 * (s IDmain1__tmp1) <= 0)%Z
    | 19%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 20%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 21%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 22%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 23%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 24%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 25%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 26%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 27%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 28%positive => (1 * (s IDmain1__tmp) + -1 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 29%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0 /\ 1 * (s IDmain1__tmp1) <= 0 /\ -1 * (s IDmain1__tmp1) <= 0)%Z
    | 30%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 31%positive => (1 * (s IDmain1__tmp) + -1 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 32%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 33%positive => (1 * (s IDmain1__tmp) + -1 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 34%positive => (-1 * (s IDmain1_z) <= 0)%Z
    | _ => False
  end.

Definition main1_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDmain1_argc)))%Q
    | 2%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1_argc)))%Q
    | 3%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 4%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 5%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 6%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 7%positive => (-(1 # 1) + (s IDmain1__tmp) + (s IDmain1_z))%Q
    | 8%positive => (-(1 # 1) + (s IDmain1__tmp) + (s IDmain1_z))%Q
    | 9%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 10%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 11%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 12%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 13%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 14%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 15%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 16%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 17%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 18%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 19%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 20%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 21%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 22%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 23%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 24%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 25%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 26%positive => ((s IDmain1__tmp) + (s IDmain1_z))%Q
    | 27%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 28%positive => ((s IDmain1_z))%Q
    | 29%positive => ((s IDmain1_z))%Q
    | 30%positive => ((s IDmain1_z))%Q
    | 31%positive => ((s IDmain1_z))%Q
    | 32%positive => ((s IDmain1_z))%Q
    | 33%positive => ((s IDmain1_z))%Q
    | 34%positive => ((s IDmain1_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition main1_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDmain1__tmp))) (F_check_ge (-1
                                                                    + (s IDmain1__tmp)) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDmain1__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmain1__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmain1__tmp)))]
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
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDmain1__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmain1__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmain1__tmp)))]
    | 27%positive => [(*5e-12 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    (s IDmain1__tmp))) (F_check_ge (0) (0))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | _ => []
  end.


Theorem main1_ai_correct:
  forall s p' s', steps (g_start main1) s (g_edges main1) p' s' -> main1_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem main1_pot_correct:
  forall s p' s',
    steps (g_start main1) s (g_edges main1) p' s' ->
    (main1_pot (g_start main1) s >= main1_pot p' s')%Q.
Proof.
  check_lp main1_ai_correct main1_hints.
Qed.

