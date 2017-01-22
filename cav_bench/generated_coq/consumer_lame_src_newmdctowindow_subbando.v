Require Import pasta.Pasta.

Notation IDwindow_subband_z := 1%positive.
Notation IDwindow_subband_i := 2%positive.
Notation IDwindow_subband_j := 3%positive.
Notation IDwindow_subband_d := 4%positive.
Notation IDwindow_subband_in := 5%positive.
Notation IDwindow_subband_xk := 6%positive.
Definition window_subband : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDwindow_subband_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDwindow_subband_i (Some (ENum (14)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_i)
             s) >= (eval (ENum (0)) s))%Z)),32%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_i)
             s) < (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDwindow_subband_i (Some (ENum (15)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_i)
             s) >= (eval (ENum (0)) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_i)
             s) < (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDwindow_subband_j (Some (ENum (14)))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_j)
             s) >= (eval (ENum (0)) s))%Z)),25%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDwindow_subband_j)
             s) < (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDwindow_subband_i
             (Some (EAdd (EVar IDwindow_subband_i) (ENum (-1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDwindow_subband_z (Some (EAdd (ENum (1))
             (EVar IDwindow_subband_z)))),24%positive)::
             (24%positive,AWeaken,10%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDwindow_subband_j
             (Some (EAdd (EVar IDwindow_subband_j) (ENum (-1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDwindow_subband_z (Some (EAdd (ENum (1))
             (EVar IDwindow_subband_z)))),31%positive)::
             (31%positive,AWeaken,17%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDwindow_subband_i
             (Some (EAdd (EVar IDwindow_subband_i) (ENum (-1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDwindow_subband_z (Some (EAdd (ENum (1))
             (EVar IDwindow_subband_z)))),38%positive)::
             (38%positive,AWeaken,5%positive)::nil
|}.

Definition window_subband_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 3%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_i) + 14 <= 0)%Z
    | 4%positive => (-1 * (s IDwindow_subband_i) + 14 <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0 /\ 1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 5%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_i) + -1 <= 0)%Z
    | 6%positive => (-1 * (s IDwindow_subband_i) + -1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDwindow_subband_i) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_i) + -1 <= 0)%Z
    | 8%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_i) + 15 <= 0)%Z
    | 9%positive => (-1 * (s IDwindow_subband_i) + 15 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 10%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0)%Z
    | 11%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDwindow_subband_i) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 13%positive => (1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_i) <= 0)%Z
    | 14%positive => (-1 * (s IDwindow_subband_i) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0)%Z
    | 15%positive => (1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_i) <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ -1 * (s IDwindow_subband_j) + 14 <= 0)%Z
    | 16%positive => (-1 * (s IDwindow_subband_j) + 14 <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ -1 * (s IDwindow_subband_i) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0)%Z
    | 17%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0)%Z
    | 18%positive => (-1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_j) + 1 <= 0)%Z
    | 19%positive => (1 * (s IDwindow_subband_j) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0)%Z
    | 20%positive => (-1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_j) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDwindow_subband_j) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0)%Z
    | 22%positive => (1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_j) + 1 <= 0)%Z
    | 23%positive => (1 * (s IDwindow_subband_j) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0)%Z
    | 24%positive => (1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_j) + 1 <= 0 /\ -1 * (s IDwindow_subband_z) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDwindow_subband_i) + -15 <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_j) <= 0)%Z
    | 26%positive => (-1 * (s IDwindow_subband_j) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0)%Z
    | 27%positive => (1 * (s IDwindow_subband_i) + -15 <= 0 /\ 1 * (s IDwindow_subband_j) + -14 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_j) <= 0)%Z
    | 28%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ 1 * (s IDwindow_subband_j) + -13 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0)%Z
    | 29%positive => (-1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_j) + -13 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 30%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ 1 * (s IDwindow_subband_j) + -13 <= 0 /\ -1 * (s IDwindow_subband_j) + -1 <= 0)%Z
    | 31%positive => (-1 * (s IDwindow_subband_j) + -1 <= 0 /\ 1 * (s IDwindow_subband_j) + -13 <= 0 /\ 1 * (s IDwindow_subband_i) + -15 <= 0 /\ -1 * (s IDwindow_subband_z) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_i) <= 0)%Z
    | 33%positive => (-1 * (s IDwindow_subband_i) <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -14 <= 0)%Z
    | 34%positive => (1 * (s IDwindow_subband_i) + -14 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0 /\ -1 * (s IDwindow_subband_i) <= 0)%Z
    | 35%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -13 <= 0 /\ -1 * (s IDwindow_subband_i) + -1 <= 0)%Z
    | 36%positive => (-1 * (s IDwindow_subband_i) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -13 <= 0 /\ -1 * (s IDwindow_subband_z) <= 0)%Z
    | 37%positive => (-1 * (s IDwindow_subband_z) <= 0 /\ 1 * (s IDwindow_subband_i) + -13 <= 0 /\ -1 * (s IDwindow_subband_i) + -1 <= 0)%Z
    | 38%positive => (-1 * (s IDwindow_subband_i) + -1 <= 0 /\ 1 * (s IDwindow_subband_i) + -13 <= 0 /\ -1 * (s IDwindow_subband_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition window_subband_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((271 # 1))%Q
    | 2%positive => ((271 # 1) + (s IDwindow_subband_z))%Q
    | 3%positive => ((256 # 1) + (s IDwindow_subband_z)
                     + max0(1 + (s IDwindow_subband_i)))%Q
    | 4%positive => ((256 # 1) + (s IDwindow_subband_z)
                     + max0(1 + (s IDwindow_subband_i)))%Q
    | 5%positive => ((256 # 1) + (s IDwindow_subband_z)
                     + max0(1 + (s IDwindow_subband_i)))%Q
    | 6%positive => ((256 # 1) + (s IDwindow_subband_z)
                     + max0(1 + (s IDwindow_subband_i)))%Q
    | 7%positive => ((256 # 1) + (s IDwindow_subband_z))%Q
    | 8%positive => ((s IDwindow_subband_z)
                     + (16 # 1) * max0(1 + (s IDwindow_subband_i)))%Q
    | 9%positive => ((s IDwindow_subband_z)
                     + (16 # 1) * max0(1 + (s IDwindow_subband_i)))%Q
    | 10%positive => ((s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i)))%Q
    | 11%positive => ((s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i)))%Q
    | 12%positive => ((s IDwindow_subband_z))%Q
    | 13%positive => ((s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i)))%Q
    | 14%positive => ((16 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 15%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 16%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 17%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 18%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 19%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 20%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 21%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i))
                      + max0(1 + (s IDwindow_subband_j)))%Q
    | 22%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i))
                      + max0(1 + (s IDwindow_subband_j)))%Q
    | 23%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i))
                      + max0(1 + (s IDwindow_subband_j)))%Q
    | 24%positive => ((s IDwindow_subband_z)
                      + (16 # 1) * max0(1 + (s IDwindow_subband_i))
                      + max0(1 + (s IDwindow_subband_j)))%Q
    | 25%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 26%positive => ((2 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0((s IDwindow_subband_i))
                      + max0((s IDwindow_subband_j)))%Q
    | 27%positive => ((2 # 1) + (s IDwindow_subband_z)
                      + (16 # 1) * max0((s IDwindow_subband_i))
                      + max0((s IDwindow_subband_j)))%Q
    | 28%positive => ((2 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 29%positive => ((2 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 30%positive => ((2 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 31%positive => ((1 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_j))
                      + (16 # 1) * max0((s IDwindow_subband_i)))%Q
    | 32%positive => ((256 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_i)))%Q
    | 33%positive => ((257 # 1) + (s IDwindow_subband_z)
                      + max0((s IDwindow_subband_i)))%Q
    | 34%positive => ((257 # 1) + (s IDwindow_subband_z)
                      + max0((s IDwindow_subband_i)))%Q
    | 35%positive => ((257 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_i)))%Q
    | 36%positive => ((257 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_i)))%Q
    | 37%positive => ((257 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_i)))%Q
    | 38%positive => ((256 # 1) + (s IDwindow_subband_z)
                      + max0(1 + (s IDwindow_subband_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition window_subband_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDwindow_subband_i)) ((s IDwindow_subband_i)));
                     (*-1 0*) F_max0_ge_0 ((s IDwindow_subband_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-16 0*) F_max0_monotonic (F_check_ge (1
                                                              + (s IDwindow_subband_i)) ((s IDwindow_subband_i)));
                      (*-16 0*) F_max0_ge_0 ((s IDwindow_subband_i))]
    | 12%positive => []
    | 13%positive => [(*-1.40285e-12 16*) F_max0_pre_decrement (1
                                                                + (s IDwindow_subband_i)) (1)]
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
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 + (s IDwindow_subband_j))) (F_check_ge (0) (0))]
    | 25%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    + (s IDwindow_subband_j)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    + (s IDwindow_subband_i)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem window_subband_ai_correct:
  forall s p' s', steps (g_start window_subband) s (g_edges window_subband) p' s' -> window_subband_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem window_subband_pot_correct:
  forall s p' s',
    steps (g_start window_subband) s (g_edges window_subband) p' s' ->
    (window_subband_pot (g_start window_subband) s >= window_subband_pot p' s')%Q.
Proof.
  check_lp window_subband_ai_correct window_subband_hints.
Qed.

