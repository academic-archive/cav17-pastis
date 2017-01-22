Require Import pasta.Pasta.

Notation IDmake_random_ideakey_z := 1%positive.
Notation IDmake_random_ideakey__tmp := 2%positive.
Notation IDmake_random_ideakey_count := 3%positive.
Notation IDmake_random_ideakey_key := 4%positive.
Notation IDmake_random_ideakey_skip := 5%positive.
Definition make_random_ideakey : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDmake_random_ideakey_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmake_random_ideakey__tmp
             (Some (EVar IDmake_random_ideakey_skip))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,5%positive)::
             (4%positive,ANone,6%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDmake_random_ideakey_count
             (Some (ENum (24)))),7%positive)::
             (7%positive,(AAssign IDmake_random_ideakey_count
             (Some (EVar IDmake_random_ideakey__tmp))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDmake_random_ideakey_count) s) <
             (eval (ENum (24)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDmake_random_ideakey_count) s) >=
             (eval (ENum (24)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDmake_random_ideakey_count
             (Some (EAdd (EVar IDmake_random_ideakey_count) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDmake_random_ideakey_z
             (Some (EAdd (ENum (1)) (EVar IDmake_random_ideakey_z)))),
             19%positive)::(19%positive,AWeaken,10%positive)::nil
|}.

Definition make_random_ideakey_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmake_random_ideakey_z) <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 4%positive => (1 * (s IDmake_random_ideakey_z) <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 6%positive => (1 * (s IDmake_random_ideakey_z) <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_count) + -24 <= 0 /\ -1 * (s IDmake_random_ideakey_count) + 24 <= 0)%Z
    | 8%positive => (1 * (s IDmake_random_ideakey_z) <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ -1 * (s IDmake_random_ideakey_count) + 24 <= 0)%Z
    | 12%positive => (-1 * (s IDmake_random_ideakey_count) + 24 <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_count) + -23 <= 0)%Z
    | 14%positive => (1 * (s IDmake_random_ideakey_count) + -23 <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 15%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_count) + -23 <= 0)%Z
    | 16%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_count) + -24 <= 0)%Z
    | 17%positive => (1 * (s IDmake_random_ideakey_count) + -24 <= 0 /\ -1 * (s IDmake_random_ideakey_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmake_random_ideakey_z) <= 0 /\ 1 * (s IDmake_random_ideakey_count) + -24 <= 0)%Z
    | 19%positive => (1 * (s IDmake_random_ideakey_count) + -24 <= 0 /\ -1 * (s IDmake_random_ideakey_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition make_random_ideakey_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(24 - (s IDmake_random_ideakey_skip)))%Q
    | 2%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey_skip)))%Q
    | 3%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey__tmp)))%Q
    | 4%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey__tmp)))%Q
    | 5%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey__tmp)))%Q
    | 6%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey__tmp)))%Q
    | 7%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey__tmp)))%Q
    | 8%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 9%positive => ((s IDmake_random_ideakey_z)
                     + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 10%positive => ((s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 11%positive => ((s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 12%positive => ((s IDmake_random_ideakey_z))%Q
    | 13%positive => ((s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 14%positive => ((1 # 1) + (s IDmake_random_ideakey_z)
                      + max0(23 - (s IDmake_random_ideakey_count)))%Q
    | 15%positive => ((1 # 1) + (s IDmake_random_ideakey_z)
                      + max0(23 - (s IDmake_random_ideakey_count)))%Q
    | 16%positive => ((1 # 1) + (s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 17%positive => ((1 # 1) + (s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 18%positive => ((1 # 1) + (s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | 19%positive => ((s IDmake_random_ideakey_z)
                      + max0(24 - (s IDmake_random_ideakey_count)))%Q
    | _ => (0 # 1)%Q
  end.

Definition make_random_ideakey_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (24
                                                             - (s IDmake_random_ideakey_count)) (23
                                                                    - (s IDmake_random_ideakey_count)));
                      (*-1 0*) F_max0_ge_0 (23
                                            - (s IDmake_random_ideakey_count))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (24
                                                     - (s IDmake_random_ideakey_count)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem make_random_ideakey_ai_correct:
  forall s p' s', steps (g_start make_random_ideakey) s (g_edges make_random_ideakey) p' s' -> make_random_ideakey_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem make_random_ideakey_pot_correct:
  forall s p' s',
    steps (g_start make_random_ideakey) s (g_edges make_random_ideakey) p' s' ->
    (make_random_ideakey_pot (g_start make_random_ideakey) s >= make_random_ideakey_pot p' s')%Q.
Proof.
  check_lp make_random_ideakey_ai_correct make_random_ideakey_hints.
Qed.

