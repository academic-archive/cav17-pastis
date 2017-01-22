Require Import pasta.Pasta.

Notation IDpclxl_beginpage_z := 1%positive.
Notation IDpclxl_beginpage_i := 2%positive.
Notation IDpclxl_beginpage_size := 3%positive.
Notation IDpclxl_beginpage_vdev_dref_off1248 := 4%positive.
Notation IDpclxl_beginpage_vdev := 5%positive.
Definition pclxl_beginpage : graph := {|
  g_start := 1%positive;
  g_end := 26%positive;
  g_edges := (1%positive,(AAssign IDpclxl_beginpage_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDpclxl_beginpage_i (Some (ENum (10)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDpclxl_beginpage_i)
             s) > (eval (ENum (0)) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDpclxl_beginpage_i)
             s) <= (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,18%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (8%positive,ANone,11%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,17%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDpclxl_beginpage_i
             (Some (EAdd (EVar IDpclxl_beginpage_i) (ENum (-1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDpclxl_beginpage_z (Some (EAdd (ENum (1))
             (EVar IDpclxl_beginpage_z)))),16%positive)::
             (16%positive,AWeaken,5%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDpclxl_beginpage_size None),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_beginpage_size) s) <>
             (eval (EVar IDpclxl_beginpage_vdev_dref_off1248) s))%Z)),
             22%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_beginpage_size) s) =
             (eval (EVar IDpclxl_beginpage_vdev_dref_off1248) s))%Z)),
             21%positive)::(21%positive,AWeaken,26%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDpclxl_beginpage_vdev_dref_off1248
             (Some (EVar IDpclxl_beginpage_size))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::nil
|}.

Definition pclxl_beginpage_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_i) + 10 <= 0)%Z
    | 4%positive => (-1 * (s IDpclxl_beginpage_i) + 10 <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ 1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0)%Z
    | 5%positive => (-1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | 6%positive => (-1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | 7%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDpclxl_beginpage_i) + 1 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 9%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDpclxl_beginpage_i) + 1 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 11%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDpclxl_beginpage_i) + 1 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 13%positive => (-1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -9 <= 0)%Z
    | 14%positive => (1 * (s IDpclxl_beginpage_i) + -9 <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0)%Z
    | 15%positive => (-1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -9 <= 0)%Z
    | 16%positive => (1 * (s IDpclxl_beginpage_i) + -9 <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) + 1 <= 0)%Z
    | 17%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 19%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | 20%positive => (-1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 21%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0 /\ 1 * (s IDpclxl_beginpage_size)+ -1 * (s IDpclxl_beginpage_vdev_dref_off1248) <= 0 /\ -1 * (s IDpclxl_beginpage_size)+ 1 * (s IDpclxl_beginpage_vdev_dref_off1248) <= 0)%Z
    | 22%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | 23%positive => (-1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 24%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | 25%positive => (-1 * (s IDpclxl_beginpage_i) <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ 1 * (s IDpclxl_beginpage_i) + -10 <= 0)%Z
    | 26%positive => (1 * (s IDpclxl_beginpage_i) + -10 <= 0 /\ -1 * (s IDpclxl_beginpage_z) <= 0 /\ -1 * (s IDpclxl_beginpage_i) <= 0)%Z
    | _ => False
  end.

Definition pclxl_beginpage_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((10 # 1))%Q
    | 2%positive => ((10 # 1) + (s IDpclxl_beginpage_z))%Q
    | 3%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 4%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 5%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 6%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 7%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 8%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 9%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 10%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 11%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 12%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 13%positive => ((1 # 1) + (s IDpclxl_beginpage_i)
                      + (s IDpclxl_beginpage_z))%Q
    | 14%positive => ((1 # 1) + (s IDpclxl_beginpage_i)
                      + (s IDpclxl_beginpage_z))%Q
    | 15%positive => ((1 # 1) + (s IDpclxl_beginpage_i)
                      + (s IDpclxl_beginpage_z))%Q
    | 16%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 17%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 18%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 19%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 20%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 21%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 22%positive => ((s IDpclxl_beginpage_i) + (s IDpclxl_beginpage_z))%Q
    | 23%positive => ((s IDpclxl_beginpage_z) + max0((s IDpclxl_beginpage_i)))%Q
    | 24%positive => ((s IDpclxl_beginpage_z) + max0((s IDpclxl_beginpage_i)))%Q
    | 25%positive => ((s IDpclxl_beginpage_z) + max0((s IDpclxl_beginpage_i)))%Q
    | 26%positive => ((s IDpclxl_beginpage_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pclxl_beginpage_hints (p : node) (s : state) := 
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
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpclxl_beginpage_i)) (-1
                                                                    + (s IDpclxl_beginpage_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpclxl_beginpage_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpclxl_beginpage_i)) (0))) (F_max0_ge_0 ((s IDpclxl_beginpage_i)))]
    | 22%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpclxl_beginpage_i)) (0))) (F_max0_ge_0 ((s IDpclxl_beginpage_i)))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpclxl_beginpage_i)) (-1
                                                                    + (s IDpclxl_beginpage_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpclxl_beginpage_i))]
    | 26%positive => []
    | _ => []
  end.


Theorem pclxl_beginpage_ai_correct:
  forall s p' s', steps (g_start pclxl_beginpage) s (g_edges pclxl_beginpage) p' s' -> pclxl_beginpage_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pclxl_beginpage_pot_correct:
  forall s p' s',
    steps (g_start pclxl_beginpage) s (g_edges pclxl_beginpage) p' s' ->
    (pclxl_beginpage_pot (g_start pclxl_beginpage) s >= pclxl_beginpage_pot p' s')%Q.
Proof.
  check_lp pclxl_beginpage_ai_correct pclxl_beginpage_hints.
Qed.

