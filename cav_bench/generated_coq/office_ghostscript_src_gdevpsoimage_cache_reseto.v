Require Import pasta.Pasta.

Notation IDimage_cache_reset_z := 1%positive.
Notation IDimage_cache_reset_i := 2%positive.
Notation IDimage_cache_reset_pdev := 3%positive.
Definition image_cache_reset : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDimage_cache_reset_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDimage_cache_reset_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDimage_cache_reset_i) s) <
             (eval (ENum (137)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDimage_cache_reset_i) s) >=
             (eval (ENum (137)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDimage_cache_reset_i
             (Some (EAdd (EVar IDimage_cache_reset_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDimage_cache_reset_z
             (Some (EAdd (ENum (1)) (EVar IDimage_cache_reset_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition image_cache_reset_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0)%Z
    | 3%positive => (-1 * (s IDimage_cache_reset_z) <= 0 /\ 1 * (s IDimage_cache_reset_z) <= 0 /\ 1 * (s IDimage_cache_reset_i) <= 0 /\ -1 * (s IDimage_cache_reset_i) <= 0)%Z
    | 4%positive => (-1 * (s IDimage_cache_reset_i) <= 0 /\ 1 * (s IDimage_cache_reset_i) <= 0 /\ 1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0)%Z
    | 5%positive => (-1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_i) <= 0 /\ 1 * (s IDimage_cache_reset_i) + -137 <= 0)%Z
    | 6%positive => (1 * (s IDimage_cache_reset_i) + -137 <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_i) + 137 <= 0)%Z
    | 7%positive => (-1 * (s IDimage_cache_reset_i) + 137 <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0 /\ 1 * (s IDimage_cache_reset_i) + -137 <= 0)%Z
    | 8%positive => (-1 * (s IDimage_cache_reset_i) <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0 /\ 1 * (s IDimage_cache_reset_i) + -136 <= 0)%Z
    | 9%positive => (1 * (s IDimage_cache_reset_i) + -136 <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_i) <= 0)%Z
    | 10%positive => (-1 * (s IDimage_cache_reset_i) <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0 /\ 1 * (s IDimage_cache_reset_i) + -136 <= 0)%Z
    | 11%positive => (-1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_i) + 1 <= 0 /\ 1 * (s IDimage_cache_reset_i) + -137 <= 0)%Z
    | 12%positive => (1 * (s IDimage_cache_reset_i) + -137 <= 0 /\ -1 * (s IDimage_cache_reset_i) + 1 <= 0 /\ -1 * (s IDimage_cache_reset_z) <= 0)%Z
    | 13%positive => (-1 * (s IDimage_cache_reset_z) <= 0 /\ -1 * (s IDimage_cache_reset_i) + 1 <= 0 /\ 1 * (s IDimage_cache_reset_i) + -137 <= 0)%Z
    | 14%positive => (1 * (s IDimage_cache_reset_i) + -137 <= 0 /\ -1 * (s IDimage_cache_reset_i) + 1 <= 0 /\ -1 * (s IDimage_cache_reset_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition image_cache_reset_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((137 # 1))%Q
    | 2%positive => ((137 # 1) + (s IDimage_cache_reset_z))%Q
    | 3%positive => ((s IDimage_cache_reset_z)
                     + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 4%positive => ((s IDimage_cache_reset_z)
                     + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 5%positive => ((s IDimage_cache_reset_z)
                     + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 6%positive => ((s IDimage_cache_reset_z)
                     + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 7%positive => ((s IDimage_cache_reset_z))%Q
    | 8%positive => ((s IDimage_cache_reset_z)
                     + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 9%positive => ((1 # 1) + (s IDimage_cache_reset_z)
                     + max0(136 - (s IDimage_cache_reset_i)))%Q
    | 10%positive => ((1 # 1) + (s IDimage_cache_reset_z)
                      + max0(136 - (s IDimage_cache_reset_i)))%Q
    | 11%positive => ((1 # 1) + (s IDimage_cache_reset_z)
                      + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 12%positive => ((1 # 1) + (s IDimage_cache_reset_z)
                      + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 13%positive => ((1 # 1) + (s IDimage_cache_reset_z)
                      + max0(137 - (s IDimage_cache_reset_i)))%Q
    | 14%positive => ((s IDimage_cache_reset_z)
                      + max0(137 - (s IDimage_cache_reset_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition image_cache_reset_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (137
                                                            - (s IDimage_cache_reset_i)) (136
                                                                    - (s IDimage_cache_reset_i)));
                     (*-1 0*) F_max0_ge_0 (136 - (s IDimage_cache_reset_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (137
                                                    - (s IDimage_cache_reset_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem image_cache_reset_ai_correct:
  forall s p' s', steps (g_start image_cache_reset) s (g_edges image_cache_reset) p' s' -> image_cache_reset_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem image_cache_reset_pot_correct:
  forall s p' s',
    steps (g_start image_cache_reset) s (g_edges image_cache_reset) p' s' ->
    (image_cache_reset_pot (g_start image_cache_reset) s >= image_cache_reset_pot p' s')%Q.
Proof.
  check_lp image_cache_reset_ai_correct image_cache_reset_hints.
Qed.

