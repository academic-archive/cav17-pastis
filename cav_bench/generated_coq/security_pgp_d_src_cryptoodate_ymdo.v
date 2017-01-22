Require Import pasta.Pasta.

Notation IDdate_ymd_z := 1%positive.
Notation IDdate_ymd_d := 2%positive.
Notation IDdate_ymd_day_dref := 3%positive.
Notation IDdate_ymd_days := 4%positive.
Notation IDdate_ymd_i := 5%positive.
Notation IDdate_ymd_m := 6%positive.
Notation IDdate_ymd_month_dref := 7%positive.
Notation IDdate_ymd_y := 8%positive.
Notation IDdate_ymd_year_dref := 9%positive.
Notation IDdate_ymd_day := 10%positive.
Notation IDdate_ymd_month := 11%positive.
Notation IDdate_ymd_tstamp := 12%positive.
Notation IDdate_ymd_year := 13%positive.
Definition date_ymd : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDdate_ymd_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDdate_ymd_days None),3%positive)::
             (3%positive,(AAssign IDdate_ymd_days
             (Some (ESub (EVar IDdate_ymd_days) (ENum (730))))),4%positive)::
             (4%positive,(AAssign IDdate_ymd_y None),5%positive)::
             (5%positive,(AAssign IDdate_ymd_d None),6%positive)::
             (6%positive,(AAssign IDdate_ymd_year_dref
             (Some (EAdd (EVar IDdate_ymd_y) (ENum (1972))))),7%positive)::
             (7%positive,(AAssign IDdate_ymd_i (Some (ENum (0)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDdate_ymd_i) s) <
             (eval (ENum (48)) s))%Z)),12%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDdate_ymd_i) s) >=
             (eval (ENum (48)) s))%Z)),11%positive)::
             (11%positive,AWeaken,27%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDdate_ymd_m None),14%positive)::
             (14%positive,(AAssign IDdate_ymd_d None),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDdate_ymd_d) s) <
             (eval (ENum (0)) s))%Z)),24%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDdate_ymd_d) s) >=
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDdate_ymd_i
             (Some (EAdd (EVar IDdate_ymd_i) (ENum (1))))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDdate_ymd_z (Some (EAdd (ENum (1))
             (EVar IDdate_ymd_z)))),23%positive)::
             (23%positive,AWeaken,10%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDdate_ymd_d None),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDdate_ymd_month_dref
             (Some (EAdd (EVar IDdate_ymd_m) (ENum (1))))),28%positive)::
             (28%positive,(AAssign IDdate_ymd_day_dref
             (Some (EAdd (EVar IDdate_ymd_d) (ENum (1))))),29%positive)::
             (29%positive,(AAssign IDdate_ymd_i None),30%positive)::
             (30%positive,AWeaken,31%positive)::nil
|}.

Definition date_ymd_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_z) <= 0)%Z
    | 4%positive => (1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0)%Z
    | 5%positive => (-1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_z) <= 0)%Z
    | 6%positive => (1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0)%Z
    | 7%positive => (-1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_z) <= 0)%Z
    | 8%positive => (1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0)%Z
    | 9%positive => (-1 * (s IDdate_ymd_i) <= 0 /\ 1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_z) <= 0)%Z
    | 10%positive => (-1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0)%Z
    | 11%positive => (1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) + 48 <= 0)%Z
    | 12%positive => (-1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -47 <= 0)%Z
    | 13%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0)%Z
    | 14%positive => (-1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -47 <= 0)%Z
    | 15%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0)%Z
    | 16%positive => (-1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -47 <= 0)%Z
    | 17%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_d) <= 0)%Z
    | 18%positive => (-1 * (s IDdate_ymd_d) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -47 <= 0)%Z
    | 19%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_d) <= 0)%Z
    | 20%positive => (-1 * (s IDdate_ymd_d) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDdate_ymd_i) + 1 <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_d) <= 0)%Z
    | 22%positive => (-1 * (s IDdate_ymd_d) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDdate_ymd_i) + 1 <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_d) <= 0 /\ -1 * (s IDdate_ymd_z) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ 1 * (s IDdate_ymd_d) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDdate_ymd_d) + 1 <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ 1 * (s IDdate_ymd_i) + -47 <= 0)%Z
    | 26%positive => (1 * (s IDdate_ymd_i) + -47 <= 0 /\ -1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0)%Z
    | 27%positive => (1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0)%Z
    | 28%positive => (-1 * (s IDdate_ymd_z) <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ 1 * (s IDdate_ymd_i) + -48 <= 0)%Z
    | 29%positive => (1 * (s IDdate_ymd_i) + -48 <= 0 /\ -1 * (s IDdate_ymd_i) <= 0 /\ -1 * (s IDdate_ymd_z) <= 0)%Z
    | 30%positive => (-1 * (s IDdate_ymd_z) <= 0)%Z
    | 31%positive => (-1 * (s IDdate_ymd_z) <= 0)%Z
    | _ => False
  end.

Definition date_ymd_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((48 # 1))%Q
    | 2%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 3%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 4%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 5%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 6%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 7%positive => ((48 # 1) + (s IDdate_ymd_z))%Q
    | 8%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 9%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 10%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 11%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 12%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 13%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 14%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 15%positive => ((48 # 1) - (s IDdate_ymd_i) + (s IDdate_ymd_z))%Q
    | 16%positive => (-(48 # 47) + (s IDdate_ymd_z)
                      + (48 # 47) * max0(48 - (s IDdate_ymd_i))
                      + (1 # 47) * max0((s IDdate_ymd_i)))%Q
    | 17%positive => (-(48 # 47) + (s IDdate_ymd_z)
                      + (48 # 47) * max0(48 - (s IDdate_ymd_i))
                      + (1 # 47) * max0((s IDdate_ymd_i)))%Q
    | 18%positive => ((48 # 1) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z) + (1 # 47) * max0((s IDdate_ymd_i)))%Q
    | 19%positive => ((48 # 1) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z) + (1 # 47) * max0((s IDdate_ymd_i)))%Q
    | 20%positive => ((2304 # 47) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z)
                      + (1 # 47) * max0(-1 + (s IDdate_ymd_i)))%Q
    | 21%positive => ((2304 # 47) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z)
                      + (1 # 47) * max0(-1 + (s IDdate_ymd_i)))%Q
    | 22%positive => ((2304 # 47) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z)
                      + (1 # 47) * max0(-1 + (s IDdate_ymd_i)))%Q
    | 23%positive => ((2257 # 47) - (48 # 47) * (s IDdate_ymd_i)
                      + (s IDdate_ymd_z)
                      + (1 # 47) * max0(-1 + (s IDdate_ymd_i)))%Q
    | 24%positive => (-(48 # 47) + (s IDdate_ymd_z)
                      + (48 # 47) * max0(48 - (s IDdate_ymd_i))
                      + (1 # 47) * max0((s IDdate_ymd_i)))%Q
    | 25%positive => ((s IDdate_ymd_z))%Q
    | 26%positive => ((s IDdate_ymd_z))%Q
    | 27%positive => ((s IDdate_ymd_z))%Q
    | 28%positive => ((s IDdate_ymd_z))%Q
    | 29%positive => ((s IDdate_ymd_z))%Q
    | 30%positive => ((s IDdate_ymd_z))%Q
    | 31%positive => ((s IDdate_ymd_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition date_ymd_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (48
                                                             - (s IDdate_ymd_i)) (47
                                                                    - (s IDdate_ymd_i)));
                      (*-1 0*) F_max0_ge_0 (47 - (s IDdate_ymd_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (48
                                                                    - (s IDdate_ymd_i)) (0))) (F_max0_ge_0 (48
                                                                    - (s IDdate_ymd_i)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*0 0.0212766*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdate_ymd_i)) (0))) (F_max0_ge_0 ((s IDdate_ymd_i)));
                      (*-1.02128 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (48
                                                                    - (s IDdate_ymd_i)) (0))) (F_max0_ge_0 (48
                                                                    - (s IDdate_ymd_i)))]
    | 16%positive => []
    | 17%positive => [(*-1.02128 0*) F_binom_monotonic 1 (F_max0_ge_arg (48
                                                                    - (s IDdate_ymd_i))) (F_check_ge (48
                                                                    - (s IDdate_ymd_i)) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-0.0212766 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDdate_ymd_i))) (F_check_ge (-1
                                                                    + (s IDdate_ymd_i)) (0))]
    | 24%positive => [(*-1.02128 0*) F_max0_pre_decrement (48
                                                           - (s IDdate_ymd_i)) (1);
                      (*-1.02128 0*) F_max0_ge_0 (47 - (s IDdate_ymd_i));
                      (*-0.0212766 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDdate_ymd_i))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | _ => []
  end.


Theorem date_ymd_ai_correct:
  forall s p' s', steps (g_start date_ymd) s (g_edges date_ymd) p' s' -> date_ymd_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem date_ymd_pot_correct:
  forall s p' s',
    steps (g_start date_ymd) s (g_edges date_ymd) p' s' ->
    (date_ymd_pot (g_start date_ymd) s >= date_ymd_pot p' s')%Q.
Proof.
  check_lp date_ymd_ai_correct date_ymd_hints.
Qed.

