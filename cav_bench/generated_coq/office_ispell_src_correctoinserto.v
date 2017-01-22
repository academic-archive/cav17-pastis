Require Import pasta.Pasta.

Notation IDinsert_z := 1%positive.
Notation IDinsert__tmp := 2%positive.
Notation IDinsert_i := 3%positive.
Notation IDinsert_maxposslen := 4%positive.
Notation IDinsert_pcount := 5%positive.
Notation IDinsert_word := 6%positive.
Definition insert : graph := {|
  g_start := 1%positive;
  g_end := 36%positive;
  g_edges := (1%positive,(AAssign IDinsert_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDinsert_i (Some (ENum (0)))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinsert_i) s) <
             (eval (EVar IDinsert_pcount) s))%Z)),25%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinsert_i) s) >=
             (eval (EVar IDinsert_pcount) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDinsert_pcount
             (Some (EAdd (EVar IDinsert_pcount) (ENum (1))))),8%positive)::
             (8%positive,(AAssign IDinsert_i None),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinsert_i) s) >
             (eval (EVar IDinsert_maxposslen) s))%Z)),12%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinsert_i) s) <=
             (eval (EVar IDinsert_maxposslen) s))%Z)),11%positive)::
             (11%positive,AWeaken,16%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDinsert_maxposslen
             (Some (EVar IDinsert_i))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDinsert_pcount)
             s) >= (eval (ENum (100)) s))%Z)),21%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDinsert_pcount)
             s) < (eval (ENum (100)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDinsert__tmp (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,36%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDinsert__tmp (Some (ENum (-1)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,36%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,33%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDinsert_i (Some (EAdd (EVar IDinsert_i)
             (ENum (1))))),29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDinsert_z (Some (EAdd (ENum (1))
             (EVar IDinsert_z)))),32%positive)::
             (32%positive,AWeaken,5%positive)::
             (33%positive,(AAssign IDinsert__tmp (Some (ENum (0)))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::nil
|}.

Definition insert_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_i) <= 0)%Z
    | 4%positive => (-1 * (s IDinsert_i) <= 0 /\ 1 * (s IDinsert_i) <= 0 /\ 1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 5%positive => (-1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i) <= 0)%Z
    | 6%positive => (-1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i)+ 1 * (s IDinsert_pcount) <= 0)%Z
    | 7%positive => (-1 * (s IDinsert_i)+ 1 * (s IDinsert_pcount) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i) <= 0)%Z
    | 8%positive => (-1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i)+ 1 * (s IDinsert_pcount) + -1 <= 0)%Z
    | 9%positive => (-1 * (s IDinsert_z) <= 0)%Z
    | 10%positive => (-1 * (s IDinsert_z) <= 0)%Z
    | 11%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_maxposslen) <= 0)%Z
    | 12%positive => (-1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i)+ 1 * (s IDinsert_maxposslen) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDinsert_i)+ 1 * (s IDinsert_maxposslen) + 1 <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 14%positive => (-1 * (s IDinsert_z) <= 0)%Z
    | 15%positive => (-1 * (s IDinsert_z) <= 0)%Z
    | 16%positive => (-1 * (s IDinsert_z) <= 0)%Z
    | 17%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_pcount) + -99 <= 0)%Z
    | 18%positive => (1 * (s IDinsert_pcount) + -99 <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 19%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_pcount) + -99 <= 0 /\ 1 * (s IDinsert__tmp) <= 0 /\ -1 * (s IDinsert__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDinsert__tmp) <= 0 /\ 1 * (s IDinsert__tmp) <= 0 /\ 1 * (s IDinsert_pcount) + -99 <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 21%positive => (-1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_pcount) + 100 <= 0)%Z
    | 22%positive => (-1 * (s IDinsert_pcount) + 100 <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 23%positive => (-1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_pcount) + 100 <= 0 /\ 1 * (s IDinsert__tmp) + 1 <= 0 /\ -1 * (s IDinsert__tmp) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDinsert__tmp) + -1 <= 0 /\ 1 * (s IDinsert__tmp) + 1 <= 0 /\ -1 * (s IDinsert_pcount) + 100 <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 25%positive => (-1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i) <= 0)%Z
    | 27%positive => (-1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i) <= 0)%Z
    | 29%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) <= 0 /\ -1 * (s IDinsert_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDinsert_i) + 1 <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) <= 0 /\ -1 * (s IDinsert_z) <= 0)%Z
    | 31%positive => (-1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) <= 0 /\ -1 * (s IDinsert_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDinsert_i) + 1 <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) <= 0 /\ -1 * (s IDinsert_z) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ -1 * (s IDinsert_i) <= 0 /\ 1 * (s IDinsert__tmp) <= 0 /\ -1 * (s IDinsert__tmp) <= 0)%Z
    | 35%positive => (-1 * (s IDinsert__tmp) <= 0 /\ 1 * (s IDinsert__tmp) <= 0 /\ -1 * (s IDinsert_i) <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert_i)+ -1 * (s IDinsert_pcount) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDinsert__tmp) + -1 <= 0 /\ -1 * (s IDinsert_z) <= 0 /\ 1 * (s IDinsert__tmp) <= 0)%Z
    | _ => False
  end.

Definition insert_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDinsert_pcount)))%Q
    | 2%positive => (max0((s IDinsert_pcount)) + max0((s IDinsert_z)))%Q
    | 3%positive => (max0(-(s IDinsert_i) + (s IDinsert_pcount))
                     + max0((s IDinsert_z)))%Q
    | 4%positive => (max0(-(s IDinsert_i) + (s IDinsert_pcount))
                     + max0((s IDinsert_z)))%Q
    | 5%positive => (max0(-(s IDinsert_i) + (s IDinsert_pcount))
                     + max0((s IDinsert_z)))%Q
    | 6%positive => (max0(-(s IDinsert_i) + (s IDinsert_pcount))
                     + max0((s IDinsert_z)))%Q
    | 7%positive => (max0((s IDinsert_z)))%Q
    | 8%positive => (max0((s IDinsert_z)))%Q
    | 9%positive => (max0((s IDinsert_z)))%Q
    | 10%positive => ((s IDinsert_z))%Q
    | 11%positive => ((s IDinsert_z))%Q
    | 12%positive => ((s IDinsert_z))%Q
    | 13%positive => ((s IDinsert_z))%Q
    | 14%positive => ((s IDinsert_z))%Q
    | 15%positive => ((s IDinsert_z))%Q
    | 16%positive => ((s IDinsert_z))%Q
    | 17%positive => ((s IDinsert_z))%Q
    | 18%positive => ((s IDinsert_z))%Q
    | 19%positive => ((s IDinsert_z))%Q
    | 20%positive => ((s IDinsert_z))%Q
    | 21%positive => ((s IDinsert_z))%Q
    | 22%positive => ((s IDinsert_z))%Q
    | 23%positive => ((s IDinsert_z))%Q
    | 24%positive => ((s IDinsert_z))%Q
    | 25%positive => (max0(-(s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 26%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 27%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 28%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 29%positive => ((1 # 1) + max0(-(s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 30%positive => ((1 # 1) + max0(-(s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 31%positive => ((1 # 1) + max0(-(s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 32%positive => ((1 # 1) + max0(-1 + (s IDinsert_z))
                      + max0(-(s IDinsert_i) + (s IDinsert_pcount)))%Q
    | 33%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 34%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 35%positive => ((1 # 1)
                      + max0(-1 - (s IDinsert_i) + (s IDinsert_pcount))
                      + max0((s IDinsert_z)))%Q
    | 36%positive => ((s IDinsert_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition insert_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDinsert_i)
                                                            + (s IDinsert_pcount)) (-1
                                                                    - (s IDinsert_i)
                                                                    + (s IDinsert_pcount)));
                     (*0 1*) F_max0_ge_0 (-1 - (s IDinsert_i)
                                          + (s IDinsert_pcount))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDinsert_z))) (F_check_ge ((s IDinsert_z)) (0))]
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
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDinsert_i)
                                                     + (s IDinsert_pcount)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDinsert_z)) (0))) (F_max0_ge_0 ((s IDinsert_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDinsert_z))) (F_check_ge (-1
                                                                    + (s IDinsert_z)) (0))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDinsert_i)
                                            + (s IDinsert_pcount));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDinsert_z))) (F_check_ge ((s IDinsert_z)) (0))]
    | 36%positive => []
    | _ => []
  end.


Theorem insert_ai_correct:
  forall s p' s', steps (g_start insert) s (g_edges insert) p' s' -> insert_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem insert_pot_correct:
  forall s p' s',
    steps (g_start insert) s (g_edges insert) p' s' ->
    (insert_pot (g_start insert) s >= insert_pot p' s')%Q.
Proof.
  check_lp insert_ai_correct insert_hints.
Qed.

