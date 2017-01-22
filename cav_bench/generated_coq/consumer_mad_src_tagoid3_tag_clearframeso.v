Require Import pasta.Pasta.

Notation IDid3_tag_clearframes_z := 1%positive.
Notation IDid3_tag_clearframes_i := 2%positive.
Notation IDid3_tag_clearframes_tag_dref_off24 := 3%positive.
Notation IDid3_tag_clearframes_tag := 4%positive.
Definition id3_tag_clearframes : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDid3_tag_clearframes_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_clearframes_tag_dref_off24)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_clearframes_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDid3_tag_clearframes_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_clearframes_i) s) <
             (eval (EVar IDid3_tag_clearframes_tag_dref_off24) s))%Z)),
             13%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_clearframes_i) s) >=
             (eval (EVar IDid3_tag_clearframes_tag_dref_off24) s))%Z)),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDid3_tag_clearframes_tag_dref_off24
             (Some (ENum (0)))),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDid3_tag_clearframes_i
             (Some (EAdd (EVar IDid3_tag_clearframes_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDid3_tag_clearframes_z
             (Some (EAdd (ENum (1)) (EVar IDid3_tag_clearframes_z)))),
             19%positive)::(19%positive,AWeaken,8%positive)::nil
|}.

Definition id3_tag_clearframes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 4%positive => (-1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0)%Z
    | 5%positive => (-1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 6%positive => (-1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0)%Z
    | 7%positive => (-1 * (s IDid3_tag_clearframes_i) <= 0 /\ 1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 8%positive => (-1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 9%positive => (1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i)+ 1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 10%positive => (-1 * (s IDid3_tag_clearframes_i)+ 1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 11%positive => (-1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 12%positive => (-1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0)%Z
    | 13%positive => (-1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) <= 0)%Z
    | 15%positive => (-1 * (s IDid3_tag_clearframes_i) <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) + 1 <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 17%positive => (1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) + 1 <= 0 /\ -1 * (s IDid3_tag_clearframes_z) <= 0)%Z
    | 18%positive => (-1 * (s IDid3_tag_clearframes_z) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) + 1 <= 0 /\ 1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0)%Z
    | 19%positive => (1 * (s IDid3_tag_clearframes_i)+ -1 * (s IDid3_tag_clearframes_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_clearframes_i) + 1 <= 0 /\ -1 * (s IDid3_tag_clearframes_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition id3_tag_clearframes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDid3_tag_clearframes_tag_dref_off24)))%Q
    | 2%positive => (max0((s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 3%positive => (max0((s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 4%positive => (max0((s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 5%positive => (max0((s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 6%positive => (max0(-(s IDid3_tag_clearframes_i)
                          + (s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 7%positive => (max0(-(s IDid3_tag_clearframes_i)
                          + (s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 8%positive => (max0(-(s IDid3_tag_clearframes_i)
                          + (s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 9%positive => (max0(-(s IDid3_tag_clearframes_i)
                          + (s IDid3_tag_clearframes_tag_dref_off24))
                     + max0((s IDid3_tag_clearframes_z)))%Q
    | 10%positive => (max0((s IDid3_tag_clearframes_z)))%Q
    | 11%positive => (max0((s IDid3_tag_clearframes_z)))%Q
    | 12%positive => ((s IDid3_tag_clearframes_z))%Q
    | 13%positive => (max0(-(s IDid3_tag_clearframes_i)
                           + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 14%positive => ((1 # 1)
                      + max0(-1 - (s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 15%positive => ((1 # 1)
                      + max0(-1 - (s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 16%positive => ((1 # 1)
                      + max0(-(s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 17%positive => ((1 # 1)
                      + max0(-(s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 18%positive => ((1 # 1)
                      + max0(-(s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24))
                      + max0((s IDid3_tag_clearframes_z)))%Q
    | 19%positive => ((1 # 1) + max0(-1 + (s IDid3_tag_clearframes_z))
                      + max0(-(s IDid3_tag_clearframes_i)
                             + (s IDid3_tag_clearframes_tag_dref_off24)))%Q
    | _ => (0 # 1)%Q
  end.

Definition id3_tag_clearframes_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDid3_tag_clearframes_i)
                                                            + (s IDid3_tag_clearframes_tag_dref_off24)) (-1
                                                                    - (s IDid3_tag_clearframes_i)
                                                                    + (s IDid3_tag_clearframes_tag_dref_off24)));
                     (*-1 0*) F_max0_ge_0 (-1 - (s IDid3_tag_clearframes_i)
                                           + (s IDid3_tag_clearframes_tag_dref_off24))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDid3_tag_clearframes_z))) (F_check_ge ((s IDid3_tag_clearframes_z)) (0))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDid3_tag_clearframes_i)
                                                     + (s IDid3_tag_clearframes_tag_dref_off24)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_clearframes_z)) (0))) (F_max0_ge_0 ((s IDid3_tag_clearframes_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDid3_tag_clearframes_z))) (F_check_ge (-1
                                                                    + (s IDid3_tag_clearframes_z)) (0))]
    | _ => []
  end.


Theorem id3_tag_clearframes_ai_correct:
  forall s p' s', steps (g_start id3_tag_clearframes) s (g_edges id3_tag_clearframes) p' s' -> id3_tag_clearframes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem id3_tag_clearframes_pot_correct:
  forall s p' s',
    steps (g_start id3_tag_clearframes) s (g_edges id3_tag_clearframes) p' s' ->
    (id3_tag_clearframes_pot (g_start id3_tag_clearframes) s >= id3_tag_clearframes_pot p' s')%Q.
Proof.
  check_lp id3_tag_clearframes_ai_correct id3_tag_clearframes_hints.
Qed.

