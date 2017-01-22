Require Import pasta.Pasta.

Notation IDideaRandByte_z := 1%positive.
Notation IDideaRandByte_i := 2%positive.
Notation IDideaRandByte_c := 3%positive.
Definition ideaRandByte : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDideaRandByte_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,30%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDideaRandByte_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDideaRandByte_i)
             s) < (eval (ENum (8)) s))%Z)),23%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDideaRandByte_i)
             s) >= (eval (ENum (8)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDideaRandByte_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDideaRandByte_i)
             s) < (eval (ENum (8)) s))%Z)),16%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDideaRandByte_i)
             s) >= (eval (ENum (8)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,31%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDideaRandByte_i
             (Some (EAdd (EVar IDideaRandByte_i) (ENum (1))))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDideaRandByte_z (Some (EAdd (ENum (1))
             (EVar IDideaRandByte_z)))),22%positive)::
             (22%positive,AWeaken,12%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDideaRandByte_i
             (Some (EAdd (EVar IDideaRandByte_i) (ENum (1))))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDideaRandByte_z (Some (EAdd (ENum (1))
             (EVar IDideaRandByte_z)))),29%positive)::
             (29%positive,AWeaken,7%positive)::
             (30%positive,AWeaken,31%positive)::nil
|}.

Definition ideaRandByte_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_z) <= 0)%Z
    | 4%positive => (1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0)%Z
    | 6%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ 1 * (s IDideaRandByte_i) <= 0 /\ 1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 7%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 8 <= 0)%Z
    | 9%positive => (-1 * (s IDideaRandByte_i) + 8 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 10%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0)%Z
    | 11%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ 1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 12%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 13%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 8 <= 0)%Z
    | 14%positive => (-1 * (s IDideaRandByte_i) + 8 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 15%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 8 <= 0)%Z
    | 16%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -7 <= 0)%Z
    | 17%positive => (1 * (s IDideaRandByte_i) + -7 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0)%Z
    | 18%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -7 <= 0)%Z
    | 19%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 20%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 21%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 22%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ -1 * (s IDideaRandByte_z) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDideaRandByte_i) + -7 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) <= 0)%Z
    | 25%positive => (-1 * (s IDideaRandByte_i) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0 /\ 1 * (s IDideaRandByte_i) + -7 <= 0)%Z
    | 26%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 27%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 28%positive => (-1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ 1 * (s IDideaRandByte_i) + -8 <= 0)%Z
    | 29%positive => (1 * (s IDideaRandByte_i) + -8 <= 0 /\ -1 * (s IDideaRandByte_i) + 1 <= 0 /\ -1 * (s IDideaRandByte_z) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDideaRandByte_z) <= 0 /\ -1 * (s IDideaRandByte_z) <= 0)%Z
    | 31%positive => (-1 * (s IDideaRandByte_z) <= 0)%Z
    | _ => False
  end.

Definition ideaRandByte_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDideaRandByte_z))%Q
    | 3%positive => ((16 # 1) + (s IDideaRandByte_z))%Q
    | 4%positive => ((16 # 1) + (s IDideaRandByte_z))%Q
    | 5%positive => ((16 # 1) - (s IDideaRandByte_i) + (s IDideaRandByte_z))%Q
    | 6%positive => ((16 # 1) - (s IDideaRandByte_i) + (s IDideaRandByte_z))%Q
    | 7%positive => ((8 # 1) + (s IDideaRandByte_z)
                     + max0(8 - (s IDideaRandByte_i)))%Q
    | 8%positive => ((8 # 1) + (s IDideaRandByte_z)
                     + max0(8 - (s IDideaRandByte_i)))%Q
    | 9%positive => ((8 # 1) + (s IDideaRandByte_z))%Q
    | 10%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 11%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 12%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 13%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 14%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 15%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 16%positive => (-(1 # 2) * (s IDideaRandByte_i) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 17%positive => ((1 # 1) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z) + max0(7 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 18%positive => ((1 # 1) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z) + max0(7 - (s IDideaRandByte_i))
                      + (1 # 2) * max0((s IDideaRandByte_i)))%Q
    | 19%positive => ((3 # 2) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z)
                      + (1 # 2) * max0(-1 + (s IDideaRandByte_i))
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 20%positive => ((3 # 2) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z)
                      + (1 # 2) * max0(-1 + (s IDideaRandByte_i))
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 21%positive => ((3 # 2) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z)
                      + (1 # 2) * max0(-1 + (s IDideaRandByte_i))
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 22%positive => ((1 # 2) - (1 # 2) * (s IDideaRandByte_i)
                      + (s IDideaRandByte_z)
                      + (1 # 2) * max0(-1 + (s IDideaRandByte_i))
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 23%positive => ((8 # 1) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 24%positive => ((9 # 1) + (s IDideaRandByte_z)
                      + max0(7 - (s IDideaRandByte_i)))%Q
    | 25%positive => ((9 # 1) + (s IDideaRandByte_z)
                      + max0(7 - (s IDideaRandByte_i)))%Q
    | 26%positive => ((9 # 1) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 27%positive => ((9 # 1) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 28%positive => ((9 # 1) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 29%positive => ((8 # 1) + (s IDideaRandByte_z)
                      + max0(8 - (s IDideaRandByte_i)))%Q
    | 30%positive => ((16 # 1) + (s IDideaRandByte_z))%Q
    | 31%positive => ((s IDideaRandByte_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaRandByte_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDideaRandByte_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDideaRandByte_i)))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDideaRandByte_i)) (7
                                                                    - (s IDideaRandByte_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDideaRandByte_i))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDideaRandByte_i)) (7
                                                                    - (s IDideaRandByte_i)));
                      (*-1 0*) F_max0_ge_0 (7 - (s IDideaRandByte_i));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDideaRandByte_i))) (F_check_ge ((s IDideaRandByte_i)) (0))]
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                   - 
                                                                   (s IDideaRandByte_i))) (F_check_ge (8
                                                                    - (s IDideaRandByte_i)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaRandByte_i)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaRandByte_i)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaRandByte_i)) (0))) (F_max0_ge_0 ((s IDideaRandByte_i)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDideaRandByte_i))) (F_check_ge (-1
                                                                    + (s IDideaRandByte_i)) (0))]
    | 23%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                  - (s IDideaRandByte_i))) (F_check_ge (8
                                                                    - (s IDideaRandByte_i)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaRandByte_i)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaRandByte_i)))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-16 0*) F_one]
    | 31%positive => []
    | _ => []
  end.


Theorem ideaRandByte_ai_correct:
  forall s p' s', steps (g_start ideaRandByte) s (g_edges ideaRandByte) p' s' -> ideaRandByte_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaRandByte_pot_correct:
  forall s p' s',
    steps (g_start ideaRandByte) s (g_edges ideaRandByte) p' s' ->
    (ideaRandByte_pot (g_start ideaRandByte) s >= ideaRandByte_pot p' s')%Q.
Proof.
  check_lp ideaRandByte_ai_correct ideaRandByte_hints.
Qed.

