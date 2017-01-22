Require Import pasta.Pasta.

Notation IDbsFinishWrite_z := 1%positive.
Notation IDbsFinishWrite_s_dref_off116 := 2%positive.
Notation IDbsFinishWrite_s_dref_off640 := 3%positive.
Notation IDbsFinishWrite_s_dref_off644 := 4%positive.
Notation IDbsFinishWrite_s := 5%positive.
Definition bsFinishWrite : graph := {|
  g_start := 1%positive;
  g_end := 6%positive;
  g_edges := (1%positive,(AAssign IDbsFinishWrite_z (Some (ENum (0)))),
             2%positive)::(2%positive,ANone,3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDbsFinishWrite_s_dref_off644) s) >
             (eval (ENum (0)) s))%Z)),7%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDbsFinishWrite_s_dref_off644) s) <=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDbsFinishWrite_s_dref_off116
             (Some (EAdd (EVar IDbsFinishWrite_s_dref_off116) (ENum (1))))),
             9%positive)::
             (9%positive,(AAssign IDbsFinishWrite_s_dref_off640 None),
             10%positive)::
             (10%positive,(AAssign IDbsFinishWrite_s_dref_off644
             (Some (ESub (EVar IDbsFinishWrite_s_dref_off644) (ENum (8))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDbsFinishWrite_z (Some (EAdd (ENum (1))
             (EVar IDbsFinishWrite_z)))),14%positive)::
             (14%positive,AWeaken,4%positive)::nil
|}.

Definition bsFinishWrite_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbsFinishWrite_z) <= 0 /\ -1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ 1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 4%positive => (-1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ 1 * (s IDbsFinishWrite_s_dref_off644) <= 0)%Z
    | 6%positive => (1 * (s IDbsFinishWrite_s_dref_off644) <= 0 /\ -1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 7%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ -1 * (s IDbsFinishWrite_s_dref_off644) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDbsFinishWrite_s_dref_off644) + 1 <= 0 /\ -1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 9%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ -1 * (s IDbsFinishWrite_s_dref_off644) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDbsFinishWrite_s_dref_off644) + 1 <= 0 /\ -1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 11%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ -1 * (s IDbsFinishWrite_s_dref_off644) + -7 <= 0)%Z
    | 12%positive => (-1 * (s IDbsFinishWrite_s_dref_off644) + -7 <= 0 /\ -1 * (s IDbsFinishWrite_z) <= 0)%Z
    | 13%positive => (-1 * (s IDbsFinishWrite_z) <= 0 /\ -1 * (s IDbsFinishWrite_s_dref_off644) + -7 <= 0)%Z
    | 14%positive => (-1 * (s IDbsFinishWrite_s_dref_off644) + -7 <= 0 /\ -1 * (s IDbsFinishWrite_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition bsFinishWrite_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 2%positive => ((s IDbsFinishWrite_z)
                     + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 3%positive => ((s IDbsFinishWrite_z)
                     + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 4%positive => ((s IDbsFinishWrite_z)
                     + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 5%positive => ((s IDbsFinishWrite_z)
                     + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 6%positive => ((s IDbsFinishWrite_z))%Q
    | 7%positive => ((s IDbsFinishWrite_z)
                     + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 8%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                     + (1 # 8) * max0(-1 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 9%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                     + (1 # 8) * max0(-1 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 10%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                      + (1 # 8) * max0(-1 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 11%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                      + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 12%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                      + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 13%positive => ((1 # 1) + (s IDbsFinishWrite_z)
                      + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | 14%positive => ((s IDbsFinishWrite_z)
                      + (1 # 8) * max0(7 + (s IDbsFinishWrite_s_dref_off644)))%Q
    | _ => (0 # 1)%Q
  end.

Definition bsFinishWrite_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge (7
                                                                + (s IDbsFinishWrite_s_dref_off644)) (-1
                                                                    + (s IDbsFinishWrite_s_dref_off644)));
                     (*-0.125 0*) F_max0_ge_0 (-1
                                               + (s IDbsFinishWrite_s_dref_off644))]
    | 6%positive => []
    | 7%positive => [(*-0.125 0*) F_max0_pre_decrement (7
                                                        + (s IDbsFinishWrite_s_dref_off644)) (8)]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem bsFinishWrite_ai_correct:
  forall s p' s', steps (g_start bsFinishWrite) s (g_edges bsFinishWrite) p' s' -> bsFinishWrite_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bsFinishWrite_pot_correct:
  forall s p' s',
    steps (g_start bsFinishWrite) s (g_edges bsFinishWrite) p' s' ->
    (bsFinishWrite_pot (g_start bsFinishWrite) s >= bsFinishWrite_pot p' s')%Q.
Proof.
  check_lp bsFinishWrite_ai_correct bsFinishWrite_hints.
Qed.

