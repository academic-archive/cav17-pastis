Require Import pasta.Pasta.

Notation IDwrite_tables_only_z := 1%positive.
Notation IDwrite_tables_only_i := 2%positive.
Notation IDwrite_tables_only_cinfo := 3%positive.
Definition write_tables_only : graph := {|
  g_start := 1%positive;
  g_end := 29%positive;
  g_edges := (1%positive,(AAssign IDwrite_tables_only_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDwrite_tables_only_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDwrite_tables_only_i) s) <
             (eval (ENum (4)) s))%Z)),30%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDwrite_tables_only_i) s) >=
             (eval (ENum (4)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,28%positive)::(7%positive,ANone,8%positive)::
             (8%positive,(AAssign IDwrite_tables_only_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDwrite_tables_only_i) s) <
             (eval (ENum (4)) s))%Z)),15%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDwrite_tables_only_i) s) >=
             (eval (ENum (4)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,29%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,20%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDwrite_tables_only_i
             (Some (EAdd (EVar IDwrite_tables_only_i) (ENum (1))))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDwrite_tables_only_z
             (Some (EAdd (ENum (1)) (EVar IDwrite_tables_only_z)))),
             27%positive)::(27%positive,AWeaken,11%positive)::
             (28%positive,AWeaken,29%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (31%positive,ANone,33%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDwrite_tables_only_i
             (Some (EAdd (EVar IDwrite_tables_only_i) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDwrite_tables_only_z
             (Some (EAdd (ENum (1)) (EVar IDwrite_tables_only_z)))),
             38%positive)::(38%positive,AWeaken,5%positive)::nil
|}.

Definition write_tables_only_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0)%Z
    | 3%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 4%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ 1 * (s IDwrite_tables_only_i) <= 0 /\ 1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0)%Z
    | 5%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 6%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 4 <= 0)%Z
    | 7%positive => (-1 * (s IDwrite_tables_only_i) + 4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 8%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 4 <= 0)%Z
    | 9%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 10%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ 1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0)%Z
    | 11%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDwrite_tables_only_i) + 4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 14%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 4 <= 0)%Z
    | 15%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 17%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 18%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 20%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 22%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 23%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 24%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0)%Z
    | 26%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 4 <= 0)%Z
    | 29%positive => (-1 * (s IDwrite_tables_only_i) + 4 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 30%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 31%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 32%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 33%positive => (1 * (s IDwrite_tables_only_i) + -3 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) <= 0)%Z
    | 34%positive => (-1 * (s IDwrite_tables_only_i) <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0 /\ 1 * (s IDwrite_tables_only_i) + -3 <= 0)%Z
    | 35%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ -1 * (s IDwrite_tables_only_z) <= 0)%Z
    | 37%positive => (-1 * (s IDwrite_tables_only_z) <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ 1 * (s IDwrite_tables_only_i) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDwrite_tables_only_i) + -4 <= 0 /\ -1 * (s IDwrite_tables_only_i) + 1 <= 0 /\ -1 * (s IDwrite_tables_only_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition write_tables_only_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDwrite_tables_only_z))%Q
    | 3%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                     + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 4%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                     + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 5%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                     + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 6%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                     + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 7%positive => ((4 # 1) + (s IDwrite_tables_only_z))%Q
    | 8%positive => ((4 # 1) + (s IDwrite_tables_only_z))%Q
    | 9%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                     + (s IDwrite_tables_only_z))%Q
    | 10%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 11%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 12%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 13%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 14%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 15%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 16%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 17%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 18%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 19%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 20%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 21%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 22%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 23%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 24%positive => ((5 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 25%positive => ((5 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 26%positive => ((5 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 27%positive => ((4 # 1) - (s IDwrite_tables_only_i)
                      + (s IDwrite_tables_only_z))%Q
    | 28%positive => ((4 # 1) + (s IDwrite_tables_only_z))%Q
    | 29%positive => ((s IDwrite_tables_only_z))%Q
    | 30%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                      + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 31%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(3 - (s IDwrite_tables_only_i)))%Q
    | 32%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(3 - (s IDwrite_tables_only_i)))%Q
    | 33%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(3 - (s IDwrite_tables_only_i)))%Q
    | 34%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(3 - (s IDwrite_tables_only_i)))%Q
    | 35%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 36%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 37%positive => ((5 # 1) + (s IDwrite_tables_only_z)
                      + max0(4 - (s IDwrite_tables_only_i)))%Q
    | 38%positive => ((4 # 1) + (s IDwrite_tables_only_z)
                      + max0(4 - (s IDwrite_tables_only_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition write_tables_only_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 1*) F_max0_monotonic (F_check_ge (4
                                                           - (s IDwrite_tables_only_i)) (3
                                                                    - (s IDwrite_tables_only_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                - (s IDwrite_tables_only_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDwrite_tables_only_i)) (3
                                                                    - (s IDwrite_tables_only_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDwrite_tables_only_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDwrite_tables_only_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDwrite_tables_only_i)))]
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
    | 28%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDwrite_tables_only_i)) (3
                                                                    - (s IDwrite_tables_only_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDwrite_tables_only_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDwrite_tables_only_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDwrite_tables_only_i)) (0))) (F_max0_ge_0 ((s IDwrite_tables_only_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDwrite_tables_only_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDwrite_tables_only_i)))]
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDwrite_tables_only_i))) (F_check_ge (4
                                                                    - (s IDwrite_tables_only_i)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDwrite_tables_only_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDwrite_tables_only_i)))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem write_tables_only_ai_correct:
  forall s p' s', steps (g_start write_tables_only) s (g_edges write_tables_only) p' s' -> write_tables_only_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem write_tables_only_pot_correct:
  forall s p' s',
    steps (g_start write_tables_only) s (g_edges write_tables_only) p' s' ->
    (write_tables_only_pot (g_start write_tables_only) s >= write_tables_only_pot p' s')%Q.
Proof.
  check_lp write_tables_only_ai_correct write_tables_only_hints.
Qed.

