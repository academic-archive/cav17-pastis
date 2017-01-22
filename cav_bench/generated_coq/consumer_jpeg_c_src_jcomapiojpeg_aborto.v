Require Import pasta.Pasta.

Notation IDjpeg_abort_z := 1%positive.
Notation IDjpeg_abort_pool := 2%positive.
Notation IDjpeg_abort_cinfo := 3%positive.
Definition jpeg_abort : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDjpeg_abort_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_abort_pool (Some (ENum (1)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDjpeg_abort_pool)
             s) > (eval (ENum (0)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDjpeg_abort_pool)
             s) <= (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDjpeg_abort_pool
             (Some (EAdd (EVar IDjpeg_abort_pool) (ENum (-1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDjpeg_abort_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_abort_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition jpeg_abort_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_abort_z) <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) + -1 <= 0 /\ -1 * (s IDjpeg_abort_pool) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDjpeg_abort_pool) + 1 <= 0 /\ 1 * (s IDjpeg_abort_pool) + -1 <= 0 /\ 1 * (s IDjpeg_abort_z) <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) + -1 <= 0 /\ -1 * (s IDjpeg_abort_pool) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0 /\ -1 * (s IDjpeg_abort_pool) <= 0)%Z
    | 8%positive => (1 * (s IDjpeg_abort_pool) + -1 <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0 /\ -1 * (s IDjpeg_abort_pool) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_abort_pool) + 1 <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) + -1 <= 0)%Z
    | 10%positive => (1 * (s IDjpeg_abort_pool) + -1 <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0 /\ -1 * (s IDjpeg_abort_pool) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_pool) <= 0)%Z
    | 12%positive => (-1 * (s IDjpeg_abort_pool) <= 0 /\ 1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_z) <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_abort_z) <= 0 /\ 1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_pool) <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_abort_pool) <= 0 /\ 1 * (s IDjpeg_abort_pool) <= 0 /\ -1 * (s IDjpeg_abort_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_abort_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 1))%Q
    | 2%positive => ((1 # 1) + (s IDjpeg_abort_z))%Q
    | 3%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z)
                     + max0(-(s IDjpeg_abort_pool)))%Q
    | 4%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z)
                     + max0(-(s IDjpeg_abort_pool)))%Q
    | 5%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z)
                     + max0(-(s IDjpeg_abort_pool)))%Q
    | 6%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z)
                     + max0(-(s IDjpeg_abort_pool)))%Q
    | 7%positive => ((s IDjpeg_abort_z))%Q
    | 8%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z)
                     + max0(-(s IDjpeg_abort_pool)))%Q
    | 9%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | 10%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | 11%positive => ((1 # 1) + (s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | 12%positive => ((1 # 1) + (s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | 13%positive => ((1 # 1) + (s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | 14%positive => ((s IDjpeg_abort_pool) + (s IDjpeg_abort_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_abort_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-(s IDjpeg_abort_pool))) (F_check_ge (-
                                                                    (s IDjpeg_abort_pool)) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDjpeg_abort_pool))) (F_check_ge (0) (0))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDjpeg_abort_pool)) (-1
                                                                    + (s IDjpeg_abort_pool)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDjpeg_abort_pool));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDjpeg_abort_pool)) (0))) (F_max0_ge_0 ((s IDjpeg_abort_pool)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDjpeg_abort_pool)) (0))) (F_max0_ge_0 (-
                                                                    (s IDjpeg_abort_pool)))]
    | _ => []
  end.


Theorem jpeg_abort_ai_correct:
  forall s p' s', steps (g_start jpeg_abort) s (g_edges jpeg_abort) p' s' -> jpeg_abort_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_abort_pot_correct:
  forall s p' s',
    steps (g_start jpeg_abort) s (g_edges jpeg_abort) p' s' ->
    (jpeg_abort_pot (g_start jpeg_abort) s >= jpeg_abort_pot p' s')%Q.
Proof.
  check_lp jpeg_abort_ai_correct jpeg_abort_hints.
Qed.

