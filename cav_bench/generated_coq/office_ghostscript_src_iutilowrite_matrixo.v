Require Import pasta.Pasta.

Notation IDwrite_matrix_z := 1%positive.
Notation IDwrite_matrix__tmp := 2%positive.
Notation IDwrite_matrix_i := 3%positive.
Notation IDwrite_matrix_op := 4%positive.
Notation IDwrite_matrix_pmat := 5%positive.
Definition write_matrix : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDwrite_matrix_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,12%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,10%positive)::(5%positive,ANone,6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDwrite_matrix__tmp None),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,35%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,35%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,32%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDwrite_matrix_i (Some (ENum (5)))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDwrite_matrix_i)
             s) >= (eval (ENum (0)) s))%Z)),22%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDwrite_matrix_i)
             s) < (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDwrite_matrix__tmp (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,35%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,25%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,26%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDwrite_matrix_i
             (Some (EAdd (EVar IDwrite_matrix_i) (ENum (-1))))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDwrite_matrix_z (Some (EAdd (ENum (1))
             (EVar IDwrite_matrix_z)))),31%positive)::
             (31%positive,AWeaken,17%positive)::
             (32%positive,(AAssign IDwrite_matrix__tmp (Some (ENum (-15)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::nil
|}.

Definition write_matrix_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 3%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 4%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 5%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 6%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 7%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 8%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 9%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 10%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 11%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 12%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 13%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0)%Z
    | 14%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 15%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_i) + 5 <= 0)%Z
    | 16%positive => (-1 * (s IDwrite_matrix_i) + 5 <= 0 /\ 1 * (s IDwrite_matrix_i) + -5 <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 17%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_i) + -1 <= 0)%Z
    | 18%positive => (-1 * (s IDwrite_matrix_i) + -1 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + 1 <= 0)%Z
    | 19%positive => (1 * (s IDwrite_matrix_i) + 1 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) + -1 <= 0)%Z
    | 20%positive => (-1 * (s IDwrite_matrix_i) + -1 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + 1 <= 0 /\ 1 * (s IDwrite_matrix__tmp) <= 0 /\ -1 * (s IDwrite_matrix__tmp) <= 0)%Z
    | 21%positive => (-1 * (s IDwrite_matrix__tmp) <= 0 /\ 1 * (s IDwrite_matrix__tmp) <= 0 /\ 1 * (s IDwrite_matrix_i) + 1 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) + -1 <= 0)%Z
    | 22%positive => (1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) <= 0)%Z
    | 23%positive => (-1 * (s IDwrite_matrix_i) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -5 <= 0)%Z
    | 24%positive => (1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) <= 0)%Z
    | 25%positive => (1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) <= 0)%Z
    | 26%positive => (-1 * (s IDwrite_matrix_i) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -5 <= 0)%Z
    | 27%positive => (1 * (s IDwrite_matrix_i) + -5 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_i) <= 0)%Z
    | 28%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -4 <= 0 /\ -1 * (s IDwrite_matrix_i) + -1 <= 0)%Z
    | 29%positive => (-1 * (s IDwrite_matrix_i) + -1 <= 0 /\ 1 * (s IDwrite_matrix_i) + -4 <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 30%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_i) + -4 <= 0 /\ -1 * (s IDwrite_matrix_i) + -1 <= 0)%Z
    | 31%positive => (-1 * (s IDwrite_matrix_i) + -1 <= 0 /\ 1 * (s IDwrite_matrix_i) + -4 <= 0 /\ -1 * (s IDwrite_matrix_z) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 33%positive => (-1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0 /\ 1 * (s IDwrite_matrix__tmp) + 15 <= 0 /\ -1 * (s IDwrite_matrix__tmp) + -15 <= 0)%Z
    | 34%positive => (-1 * (s IDwrite_matrix__tmp) + -15 <= 0 /\ 1 * (s IDwrite_matrix__tmp) + 15 <= 0 /\ 1 * (s IDwrite_matrix_z) <= 0 /\ -1 * (s IDwrite_matrix_z) <= 0)%Z
    | 35%positive => (-1 * (s IDwrite_matrix_z) <= 0)%Z
    | _ => False
  end.

Definition write_matrix_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((6 # 1))%Q
    | 2%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 3%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 4%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 5%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 6%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 7%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 8%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 9%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 10%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 11%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 12%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 13%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 14%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 15%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 16%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 17%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 18%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 19%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 20%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 21%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 22%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 23%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0((s IDwrite_matrix_i)))%Q
    | 24%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0((s IDwrite_matrix_i)))%Q
    | 25%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0((s IDwrite_matrix_i)))%Q
    | 26%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0((s IDwrite_matrix_i)))%Q
    | 27%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0((s IDwrite_matrix_i)))%Q
    | 28%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0(1 + (s IDwrite_matrix_i)))%Q
    | 29%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0(1 + (s IDwrite_matrix_i)))%Q
    | 30%positive => ((1 # 1) + (s IDwrite_matrix_z)
                      + max0(1 + (s IDwrite_matrix_i)))%Q
    | 31%positive => ((s IDwrite_matrix_z) + max0(1 + (s IDwrite_matrix_i)))%Q
    | 32%positive => ((6 # 1) + (s IDwrite_matrix_z))%Q
    | 33%positive => ((s IDwrite_matrix_z)
                      + (2 # 5) * max0(-(s IDwrite_matrix__tmp)))%Q
    | 34%positive => ((s IDwrite_matrix_z)
                      + (2 # 5) * max0(-(s IDwrite_matrix__tmp)))%Q
    | 35%positive => ((s IDwrite_matrix_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition write_matrix_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-6 0*) F_one]
    | 10%positive => []
    | 11%positive => [(*-6 0*) F_one]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDwrite_matrix_i)) ((s IDwrite_matrix_i)));
                      (*-1 0*) F_max0_ge_0 ((s IDwrite_matrix_i))]
    | 22%positive => [(*0 1*) F_max0_pre_decrement (1 + (s IDwrite_matrix_i)) (1)]
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
    | 34%positive => [(*-0.4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDwrite_matrix__tmp))) (F_check_ge (0) (0))]
    | 35%positive => []
    | _ => []
  end.


Theorem write_matrix_ai_correct:
  forall s p' s', steps (g_start write_matrix) s (g_edges write_matrix) p' s' -> write_matrix_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem write_matrix_pot_correct:
  forall s p' s',
    steps (g_start write_matrix) s (g_edges write_matrix) p' s' ->
    (write_matrix_pot (g_start write_matrix) s >= write_matrix_pot p' s')%Q.
Proof.
  check_lp write_matrix_ai_correct write_matrix_hints.
Qed.

