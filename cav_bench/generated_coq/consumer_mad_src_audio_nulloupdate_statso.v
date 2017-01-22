Require Import pasta.Pasta.

Notation IDupdate_stats_z := 1%positive.
Notation IDupdate_stats__tmp := 2%positive.
Notation IDupdate_stats_stats_dref_off12 := 3%positive.
Notation IDupdate_stats_stats_dref_off8 := 4%positive.
Notation IDupdate_stats_nsamples := 5%positive.
Notation IDupdate_stats_sample := 6%positive.
Notation IDupdate_stats_stats := 7%positive.
Definition update_stats : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDupdate_stats_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDupdate_stats__tmp
             (Some (EVar IDupdate_stats_nsamples))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDupdate_stats__tmp
             (Some (EAdd (EVar IDupdate_stats__tmp) (ENum (-1))))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDupdate_stats__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDupdate_stats__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,22%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,21%positive)::
             (13%positive,(AAssign IDupdate_stats_stats_dref_off12 None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (15%positive,ANone,20%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (17%positive,ANone,20%positive)::
             (18%positive,(AAssign IDupdate_stats_stats_dref_off8 None),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,30%positive)::
             (22%positive,(AAssign IDupdate_stats_stats_dref_off12 None),
             23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (24%positive,ANone,29%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,29%positive)::
             (27%positive,(AAssign IDupdate_stats_stats_dref_off8 None),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDupdate_stats_z (Some (EAdd (ENum (1))
             (EVar IDupdate_stats_z)))),4%positive)::nil
|}.

Definition update_stats_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDupdate_stats_z) <= 0 /\ -1 * (s IDupdate_stats_z) <= 0)%Z
    | 3%positive => (-1 * (s IDupdate_stats_z) <= 0 /\ 1 * (s IDupdate_stats_z) <= 0)%Z
    | 4%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 5%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 6%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 7%positive => (-1 * (s IDupdate_stats_z) <= 0 /\ 1 * (s IDupdate_stats__tmp) <= 0 /\ -1 * (s IDupdate_stats__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDupdate_stats__tmp) <= 0 /\ 1 * (s IDupdate_stats__tmp) <= 0 /\ -1 * (s IDupdate_stats_z) <= 0)%Z
    | 9%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 10%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 11%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 12%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 13%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 14%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 15%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 16%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 17%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 18%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 19%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 20%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 21%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 22%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 23%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 24%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 25%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 26%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 27%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 28%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 29%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 30%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 31%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | 32%positive => (-1 * (s IDupdate_stats_z) <= 0)%Z
    | _ => False
  end.

Definition update_stats_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDupdate_stats_nsamples))%Q
    | 2%positive => ((s IDupdate_stats_nsamples) + (s IDupdate_stats_z))%Q
    | 3%positive => ((s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 4%positive => ((s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 5%positive => ((1 # 1) + (s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 6%positive => ((1 # 1) + (s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 7%positive => ((1 # 1) + (s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 8%positive => ((s IDupdate_stats_z))%Q
    | 9%positive => ((1 # 1) + (s IDupdate_stats__tmp) + (s IDupdate_stats_z))%Q
    | 10%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 11%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 12%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 13%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 14%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 15%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 16%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 17%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 18%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 19%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 20%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 21%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 22%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 23%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 24%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 25%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 26%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 27%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 28%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 29%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 30%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 31%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | 32%positive => ((1 # 1) + (s IDupdate_stats__tmp)
                      + (s IDupdate_stats_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition update_stats_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDupdate_stats__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDupdate_stats__tmp)) (0))) (F_max0_ge_0 ((s IDupdate_stats__tmp)))]
    | 8%positive => []
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
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | _ => []
  end.


Theorem update_stats_ai_correct:
  forall s p' s', steps (g_start update_stats) s (g_edges update_stats) p' s' -> update_stats_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem update_stats_pot_correct:
  forall s p' s',
    steps (g_start update_stats) s (g_edges update_stats) p' s' ->
    (update_stats_pot (g_start update_stats) s >= update_stats_pot p' s')%Q.
Proof.
  check_lp update_stats_ai_correct update_stats_hints.
Qed.

