Require Import pasta.Pasta.

Notation IDread_non_rle_pixel_z := 1%positive.
Notation IDread_non_rle_pixel_i := 2%positive.
Notation IDread_non_rle_pixel_sinfo_dref_off92 := 3%positive.
Notation IDread_non_rle_pixel_sinfo := 4%positive.
Definition read_non_rle_pixel : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDread_non_rle_pixel_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_non_rle_pixel_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDread_non_rle_pixel_i) s) <
             (eval (EVar IDread_non_rle_pixel_sinfo_dref_off92) s))%Z)),
             8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDread_non_rle_pixel_i) s) >=
             (eval (EVar IDread_non_rle_pixel_sinfo_dref_off92) s))%Z)),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDread_non_rle_pixel_i
             (Some (EAdd (EVar IDread_non_rle_pixel_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDread_non_rle_pixel_z
             (Some (EAdd (ENum (1)) (EVar IDread_non_rle_pixel_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition read_non_rle_pixel_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_non_rle_pixel_z) <= 0 /\ 1 * (s IDread_non_rle_pixel_z) <= 0 /\ 1 * (s IDread_non_rle_pixel_i) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) <= 0)%Z
    | 4%positive => (-1 * (s IDread_non_rle_pixel_i) <= 0 /\ 1 * (s IDread_non_rle_pixel_i) <= 0 /\ 1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) <= 0)%Z
    | 6%positive => (-1 * (s IDread_non_rle_pixel_i) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i)+ 1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0)%Z
    | 7%positive => (-1 * (s IDread_non_rle_pixel_i)+ 1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) <= 0)%Z
    | 8%positive => (-1 * (s IDread_non_rle_pixel_i) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0 /\ 1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) + 1 <= 0)%Z
    | 9%positive => (1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) + 1 <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) <= 0)%Z
    | 10%positive => (-1 * (s IDread_non_rle_pixel_i) <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0 /\ 1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) + 1 <= 0 /\ 1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0)%Z
    | 12%positive => (1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) + 1 <= 0 /\ -1 * (s IDread_non_rle_pixel_z) <= 0)%Z
    | 13%positive => (-1 * (s IDread_non_rle_pixel_z) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) + 1 <= 0 /\ 1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0)%Z
    | 14%positive => (1 * (s IDread_non_rle_pixel_i)+ -1 * (s IDread_non_rle_pixel_sinfo_dref_off92) <= 0 /\ -1 * (s IDread_non_rle_pixel_i) + 1 <= 0 /\ -1 * (s IDread_non_rle_pixel_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition read_non_rle_pixel_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 2%positive => ((s IDread_non_rle_pixel_z)
                     + max0((s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 3%positive => ((s IDread_non_rle_pixel_z)
                     + max0(-(s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 4%positive => ((s IDread_non_rle_pixel_z)
                     + max0(-(s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 5%positive => ((s IDread_non_rle_pixel_z)
                     + max0(-(s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 6%positive => ((s IDread_non_rle_pixel_z)
                     + max0(-(s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 7%positive => ((s IDread_non_rle_pixel_z))%Q
    | 8%positive => ((s IDread_non_rle_pixel_z)
                     + max0(-(s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 9%positive => ((1 # 1) + (s IDread_non_rle_pixel_z)
                     + max0(-1 - (s IDread_non_rle_pixel_i)
                            + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 10%positive => ((1 # 1) + (s IDread_non_rle_pixel_z)
                      + max0(-1 - (s IDread_non_rle_pixel_i)
                             + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 11%positive => ((1 # 1) + (s IDread_non_rle_pixel_z)
                      + max0(-(s IDread_non_rle_pixel_i)
                             + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 12%positive => ((1 # 1) + (s IDread_non_rle_pixel_z)
                      + max0(-(s IDread_non_rle_pixel_i)
                             + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 13%positive => ((1 # 1) + (s IDread_non_rle_pixel_z)
                      + max0(-(s IDread_non_rle_pixel_i)
                             + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | 14%positive => ((s IDread_non_rle_pixel_z)
                      + max0(-(s IDread_non_rle_pixel_i)
                             + (s IDread_non_rle_pixel_sinfo_dref_off92)))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_non_rle_pixel_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDread_non_rle_pixel_i)
                                                            + (s IDread_non_rle_pixel_sinfo_dref_off92)) (-1
                                                                    - (s IDread_non_rle_pixel_i)
                                                                    + (s IDread_non_rle_pixel_sinfo_dref_off92)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDread_non_rle_pixel_i)
                                                                + (s IDread_non_rle_pixel_sinfo_dref_off92))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDread_non_rle_pixel_i)
                                                    + (s IDread_non_rle_pixel_sinfo_dref_off92)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem read_non_rle_pixel_ai_correct:
  forall s p' s', steps (g_start read_non_rle_pixel) s (g_edges read_non_rle_pixel) p' s' -> read_non_rle_pixel_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_non_rle_pixel_pot_correct:
  forall s p' s',
    steps (g_start read_non_rle_pixel) s (g_edges read_non_rle_pixel) p' s' ->
    (read_non_rle_pixel_pot (g_start read_non_rle_pixel) s >= read_non_rle_pixel_pot p' s')%Q.
Proof.
  check_lp read_non_rle_pixel_ai_correct read_non_rle_pixel_hints.
Qed.

