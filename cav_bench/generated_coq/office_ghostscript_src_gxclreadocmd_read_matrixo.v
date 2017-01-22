Require Import pasta.Pasta.

Notation IDcmd_read_matrix_z := 1%positive.
Notation IDcmd_read_matrix_b := 2%positive.
Notation IDcmd_read_matrix_i := 3%positive.
Notation IDcmd_read_matrix_cbp := 4%positive.
Notation IDcmd_read_matrix_pmat := 5%positive.
Definition cmd_read_matrix : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDcmd_read_matrix_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcmd_read_matrix_b None),3%positive)::
             (3%positive,(AAssign IDcmd_read_matrix_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcmd_read_matrix_i)
             s) < (eval (ENum (4)) s))%Z)),24%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcmd_read_matrix_i)
             s) >= (eval (ENum (4)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDcmd_read_matrix_i)
             s) < (eval (ENum (6)) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDcmd_read_matrix_i)
             s) >= (eval (ENum (6)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,16%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,17%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDcmd_read_matrix_i
             (Some (EAdd (EVar IDcmd_read_matrix_i) (ENum (1))))),
             19%positive)::
             (19%positive,(AAssign IDcmd_read_matrix_b None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDcmd_read_matrix_z (Some (EAdd (ENum (1))
             (EVar IDcmd_read_matrix_z)))),23%positive)::
             (23%positive,AWeaken,10%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,27%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,33%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,32%positive)::
             (28%positive,ANone,31%positive)::
             (28%positive,ANone,30%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,32%positive)::
             (30%positive,ANone,32%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDcmd_read_matrix_i
             (Some (EAdd (EVar IDcmd_read_matrix_i) (ENum (2))))),
             35%positive)::
             (35%positive,(AAssign IDcmd_read_matrix_b None),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDcmd_read_matrix_z (Some (EAdd (ENum (1))
             (EVar IDcmd_read_matrix_z)))),39%positive)::
             (39%positive,AWeaken,6%positive)::nil
|}.

Definition cmd_read_matrix_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 4%positive => (1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ 1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 7%positive => (1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 9%positive => (1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -6 <= 0)%Z
    | 11%positive => (1 * (s IDcmd_read_matrix_i) + -6 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 6 <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_read_matrix_i) + 6 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -6 <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 14%positive => (1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 16%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 17%positive => (1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 18%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 4 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 5 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -6 <= 0)%Z
    | 20%positive => (1 * (s IDcmd_read_matrix_i) + -6 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 5 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -6 <= 0)%Z
    | 22%positive => (1 * (s IDcmd_read_matrix_i) + -6 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_read_matrix_i) + 5 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -6 <= 0 /\ -1 * (s IDcmd_read_matrix_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 25%positive => (1 * (s IDcmd_read_matrix_i) + -3 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0)%Z
    | 26%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDcmd_read_matrix_i) + -3 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0)%Z
    | 29%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 30%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 31%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 32%positive => (1 * (s IDcmd_read_matrix_i) + -3 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0)%Z
    | 33%positive => (-1 * (s IDcmd_read_matrix_i) <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -3 <= 0)%Z
    | 34%positive => (1 * (s IDcmd_read_matrix_i) + -3 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0 /\ -1 * (s IDcmd_read_matrix_i) <= 0)%Z
    | 35%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 2 <= 0)%Z
    | 36%positive => (-1 * (s IDcmd_read_matrix_i) + 2 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 37%positive => (-1 * (s IDcmd_read_matrix_z) <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 2 <= 0)%Z
    | 38%positive => (-1 * (s IDcmd_read_matrix_i) + 2 <= 0 /\ 1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_z) <= 0)%Z
    | 39%positive => (1 * (s IDcmd_read_matrix_i) + -5 <= 0 /\ -1 * (s IDcmd_read_matrix_i) + 2 <= 0 /\ -1 * (s IDcmd_read_matrix_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cmd_read_matrix_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDcmd_read_matrix_z))%Q
    | 3%positive => ((4 # 1) + (s IDcmd_read_matrix_z))%Q
    | 4%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z))%Q
    | 5%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z))%Q
    | 6%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z))%Q
    | 7%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z))%Q
    | 8%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z)
                     + (1 # 2) * max0(-4 + (s IDcmd_read_matrix_i)))%Q
    | 9%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                     + (s IDcmd_read_matrix_z)
                     + (1 # 2) * max0(-4 + (s IDcmd_read_matrix_i)))%Q
    | 10%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 11%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 12%positive => ((s IDcmd_read_matrix_z))%Q
    | 13%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 14%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 15%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 16%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 17%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 18%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 19%positive => ((7 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 20%positive => ((7 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 21%positive => ((7 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 22%positive => ((7 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 23%positive => ((6 # 1) - (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 24%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 25%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 26%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 27%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 28%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 29%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 30%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 31%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 32%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 33%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 34%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 35%positive => ((5 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 36%positive => ((5 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 37%positive => ((5 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 38%positive => ((5 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | 39%positive => ((4 # 1) - (1 # 2) * (s IDcmd_read_matrix_i)
                      + (s IDcmd_read_matrix_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_read_matrix_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDcmd_read_matrix_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDcmd_read_matrix_i)))]
    | 8%positive => []
    | 9%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                  + (s IDcmd_read_matrix_i))) (F_check_ge (0) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (6
                                                             - (s IDcmd_read_matrix_i)) (4
                                                                    - (s IDcmd_read_matrix_i)));
                      (*-1 0*) F_max0_ge_0 (4 - (s IDcmd_read_matrix_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - (s IDcmd_read_matrix_i)) (0))) (F_max0_ge_0 (6
                                                                    - (s IDcmd_read_matrix_i)))]
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
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | _ => []
  end.


Theorem cmd_read_matrix_ai_correct:
  forall s p' s', steps (g_start cmd_read_matrix) s (g_edges cmd_read_matrix) p' s' -> cmd_read_matrix_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_read_matrix_pot_correct:
  forall s p' s',
    steps (g_start cmd_read_matrix) s (g_edges cmd_read_matrix) p' s' ->
    (cmd_read_matrix_pot (g_start cmd_read_matrix) s >= cmd_read_matrix_pot p' s')%Q.
Proof.
  check_lp cmd_read_matrix_ai_correct cmd_read_matrix_hints.
Qed.

