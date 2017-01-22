Require Import pasta.Pasta.

Notation IDgs_image_cleanup_z := 1%positive.
Notation IDgs_image_cleanup_i := 2%positive.
Notation IDgs_image_cleanup_pie_dref_off16 := 3%positive.
Notation IDgs_image_cleanup_pie_dref_off40 := 4%positive.
Notation IDgs_image_cleanup_pie := 5%positive.
Definition gs_image_cleanup : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDgs_image_cleanup_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgs_image_cleanup_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDgs_image_cleanup_i)
             s) < (eval (EVar IDgs_image_cleanup_pie_dref_off40) s))%Z)),
             13%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDgs_image_cleanup_i)
             s) >= (eval (EVar IDgs_image_cleanup_pie_dref_off40) s))%Z)),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgs_image_cleanup_pie_dref_off16) s) <>
             (eval (ENum (0)) s))%Z)),11%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgs_image_cleanup_pie_dref_off16) s) =
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,12%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDgs_image_cleanup_i
             (Some (EAdd (EVar IDgs_image_cleanup_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDgs_image_cleanup_z
             (Some (EAdd (ENum (1)) (EVar IDgs_image_cleanup_z)))),
             19%positive)::(19%positive,AWeaken,5%positive)::nil
|}.

Definition gs_image_cleanup_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_image_cleanup_z) <= 0 /\ 1 * (s IDgs_image_cleanup_z) <= 0 /\ 1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ 1 * (s IDgs_image_cleanup_i) <= 0 /\ 1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 6%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0)%Z
    | 7%positive => (-1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 8%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ 1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0 /\ -1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0)%Z
    | 9%positive => (-1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0 /\ 1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0 /\ -1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ 1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0 /\ -1 * (s IDgs_image_cleanup_pie_dref_off16) <= 0)%Z
    | 11%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0)%Z
    | 12%positive => (-1 * (s IDgs_image_cleanup_i)+ 1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 13%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ 1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) + 1 <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgs_image_cleanup_i) <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0 /\ 1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) + 1 <= 0 /\ 1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0)%Z
    | 17%positive => (1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ -1 * (s IDgs_image_cleanup_i) + 1 <= 0 /\ -1 * (s IDgs_image_cleanup_z) <= 0)%Z
    | 18%positive => (-1 * (s IDgs_image_cleanup_z) <= 0 /\ -1 * (s IDgs_image_cleanup_i) + 1 <= 0 /\ 1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0)%Z
    | 19%positive => (1 * (s IDgs_image_cleanup_i)+ -1 * (s IDgs_image_cleanup_pie_dref_off40) <= 0 /\ -1 * (s IDgs_image_cleanup_i) + 1 <= 0 /\ -1 * (s IDgs_image_cleanup_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gs_image_cleanup_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 2%positive => ((s IDgs_image_cleanup_z)
                     + max0((s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 3%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 4%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 5%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 6%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 7%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 8%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 9%positive => ((s IDgs_image_cleanup_z)
                     + max0(-(s IDgs_image_cleanup_i)
                            + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 10%positive => ((s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 11%positive => ((s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 12%positive => ((s IDgs_image_cleanup_z))%Q
    | 13%positive => ((s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 14%positive => ((1 # 1) + (s IDgs_image_cleanup_z)
                      + max0(-1 - (s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 15%positive => ((1 # 1) + (s IDgs_image_cleanup_z)
                      + max0(-1 - (s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 16%positive => ((1 # 1) + (s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 17%positive => ((1 # 1) + (s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 18%positive => ((1 # 1) + (s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | 19%positive => ((s IDgs_image_cleanup_z)
                      + max0(-(s IDgs_image_cleanup_i)
                             + (s IDgs_image_cleanup_pie_dref_off40)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_image_cleanup_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDgs_image_cleanup_i)
                                                             + (s IDgs_image_cleanup_pie_dref_off40)) (-1
                                                                    - (s IDgs_image_cleanup_i)
                                                                    + (s IDgs_image_cleanup_pie_dref_off40)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDgs_image_cleanup_i)
                                            + (s IDgs_image_cleanup_pie_dref_off40))]
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDgs_image_cleanup_i)
                                                             + (s IDgs_image_cleanup_pie_dref_off40)) (-1
                                                                    - (s IDgs_image_cleanup_i)
                                                                    + (s IDgs_image_cleanup_pie_dref_off40)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDgs_image_cleanup_i)
                                            + (s IDgs_image_cleanup_pie_dref_off40))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDgs_image_cleanup_i)
                                                     + (s IDgs_image_cleanup_pie_dref_off40)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem gs_image_cleanup_ai_correct:
  forall s p' s', steps (g_start gs_image_cleanup) s (g_edges gs_image_cleanup) p' s' -> gs_image_cleanup_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_image_cleanup_pot_correct:
  forall s p' s',
    steps (g_start gs_image_cleanup) s (g_edges gs_image_cleanup) p' s' ->
    (gs_image_cleanup_pot (g_start gs_image_cleanup) s >= gs_image_cleanup_pot p' s')%Q.
Proof.
  check_lp gs_image_cleanup_ai_correct gs_image_cleanup_hints.
Qed.

