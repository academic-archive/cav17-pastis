Require Import pasta.Pasta.

Notation IDbsW_z := 1%positive.
Notation IDbsW__tmp := 2%positive.
Notation IDbsW__tmp1 := 3%positive.
Notation IDbsW_s_dref_off116 := 4%positive.
Notation IDbsW_s_dref_off640 := 5%positive.
Notation IDbsW_s_dref_off644 := 6%positive.
Notation IDbsW_n := 7%positive.
Notation IDbsW_s := 8%positive.
Notation IDbsW_v := 9%positive.
Definition bsW : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDbsW_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDbsW__tmp (Some (EVar IDbsW_n))),
             3%positive)::
             (3%positive,(AAssign IDbsW__tmp1 (Some (EVar IDbsW_v))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDbsW_s_dref_off644)
             s) >= (eval (ENum (8)) s))%Z)),12%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDbsW_s_dref_off644)
             s) < (eval (ENum (8)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDbsW_s_dref_off640 None),9%positive)::
             (9%positive,(AAssign IDbsW_s_dref_off644
             (Some (EAdd (EVar IDbsW_s_dref_off644) (EVar IDbsW__tmp)))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDbsW_s_dref_off116
             (Some (EAdd (EVar IDbsW_s_dref_off116) (ENum (1))))),
             14%positive)::
             (14%positive,(AAssign IDbsW_s_dref_off640 None),15%positive)::
             (15%positive,(AAssign IDbsW_s_dref_off644
             (Some (ESub (EVar IDbsW_s_dref_off644) (ENum (8))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDbsW_z (Some (EAdd (ENum (1))
             (EVar IDbsW_z)))),19%positive)::
             (19%positive,AWeaken,6%positive)::nil
|}.

Definition bsW_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbsW_z) <= 0 /\ 1 * (s IDbsW_z) <= 0)%Z
    | 4%positive => (1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbsW_z) <= 0 /\ 1 * (s IDbsW_z) <= 0)%Z
    | 6%positive => (-1 * (s IDbsW_z) <= 0)%Z
    | 7%positive => (-1 * (s IDbsW_z) <= 0 /\ 1 * (s IDbsW_s_dref_off644) + -7 <= 0)%Z
    | 8%positive => (1 * (s IDbsW_s_dref_off644) + -7 <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 9%positive => (-1 * (s IDbsW_z) <= 0 /\ 1 * (s IDbsW_s_dref_off644) + -7 <= 0)%Z
    | 10%positive => (-1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW__tmp)+ 1 * (s IDbsW_s_dref_off644) + -7 <= 0)%Z
    | 11%positive => (-1 * (s IDbsW__tmp)+ 1 * (s IDbsW_s_dref_off644) + -7 <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 12%positive => (-1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_s_dref_off644) + 8 <= 0)%Z
    | 13%positive => (-1 * (s IDbsW_s_dref_off644) + 8 <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 14%positive => (-1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_s_dref_off644) + 8 <= 0)%Z
    | 15%positive => (-1 * (s IDbsW_s_dref_off644) + 8 <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 16%positive => (-1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_s_dref_off644) <= 0)%Z
    | 17%positive => (-1 * (s IDbsW_s_dref_off644) <= 0 /\ -1 * (s IDbsW_z) <= 0)%Z
    | 18%positive => (-1 * (s IDbsW_z) <= 0 /\ -1 * (s IDbsW_s_dref_off644) <= 0)%Z
    | 19%positive => (-1 * (s IDbsW_s_dref_off644) <= 0 /\ -1 * (s IDbsW_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition bsW_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644)))%Q
    | 2%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 3%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 4%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 5%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 6%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 7%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                     + max0((s IDbsW_z)))%Q
    | 8%positive => (max0((s IDbsW_z)))%Q
    | 9%positive => (max0((s IDbsW_z)))%Q
    | 10%positive => (max0((s IDbsW_z)))%Q
    | 11%positive => ((s IDbsW_z))%Q
    | 12%positive => ((1 # 8) * max0((s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 13%positive => ((1 # 1) + (1 # 8) * max0(-8 + (s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 14%positive => ((1 # 1) + (1 # 8) * max0(-8 + (s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 15%positive => ((1 # 1) + (1 # 8) * max0(-8 + (s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 16%positive => ((1 # 1) + (1 # 8) * max0((s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 17%positive => ((1 # 1) + (1 # 8) * max0((s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 18%positive => ((1 # 1) + (1 # 8) * max0((s IDbsW_s_dref_off644))
                      + max0((s IDbsW_z)))%Q
    | 19%positive => ((1 # 1) + max0(-1 + (s IDbsW_z))
                      + (1 # 8) * max0((s IDbsW_s_dref_off644)))%Q
    | _ => (0 # 1)%Q
  end.

Definition bsW_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDbsW_s_dref_off644)) (-8
                                                                    + (s IDbsW_s_dref_off644)));
                     (*-0.125 0*) F_max0_ge_0 (-8 + (s IDbsW_s_dref_off644))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDbsW_z))) (F_check_ge ((s IDbsW_z)) (0))]
    | 11%positive => []
    | 12%positive => [(*-0.125 0*) F_max0_pre_decrement ((s IDbsW_s_dref_off644)) (8)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDbsW_z)) (0))) (F_max0_ge_0 ((s IDbsW_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDbsW_z))) (F_check_ge (-1
                                                                    + (s IDbsW_z)) (0))]
    | _ => []
  end.


Theorem bsW_ai_correct:
  forall s p' s', steps (g_start bsW) s (g_edges bsW) p' s' -> bsW_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bsW_pot_correct:
  forall s p' s',
    steps (g_start bsW) s (g_edges bsW) p' s' ->
    (bsW_pot (g_start bsW) s >= bsW_pot p' s')%Q.
Proof.
  check_lp bsW_ai_correct bsW_hints.
Qed.

