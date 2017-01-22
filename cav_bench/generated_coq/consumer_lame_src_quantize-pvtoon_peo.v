Require Import pasta.Pasta.

Notation IDon_pe_z := 1%positive.
Notation IDon_pe__tmp := 2%positive.
Notation IDon_pe__tmp1 := 3%positive.
Notation IDon_pe_bits := 4%positive.
Notation IDon_pe_ch := 5%positive.
Notation IDon_pe_gfp_dref_off204 := 6%positive.
Notation IDon_pe_gfp := 7%positive.
Notation IDon_pe_gr := 8%positive.
Notation IDon_pe_l3_side := 9%positive.
Notation IDon_pe_mean_bits := 10%positive.
Notation IDon_pe_pe := 11%positive.
Notation IDon_pe_targ_bits := 12%positive.
Definition on_pe : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDon_pe_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDon_pe__tmp1
             (Some (EVar IDon_pe_mean_bits))),3%positive)::
             (3%positive,(AAssign IDon_pe__tmp (Some (EVar IDon_pe_gr))),
             4%positive)::
             (4%positive,(AAssign IDon_pe_ch (Some (ENum (0)))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDon_pe_ch) s) <
             (eval (EVar IDon_pe_gfp_dref_off204) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDon_pe_ch) s) >=
             (eval (EVar IDon_pe_gfp_dref_off204) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDon_pe_bits (Some (ENum (0)))),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,ANone,15%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,20%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,18%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDon_pe_bits None),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,26%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,28%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (28%positive,ANone,30%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDon_pe_ch (Some (EAdd (EVar IDon_pe_ch)
             (ENum (1))))),32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDon_pe_z (Some (EAdd (ENum (1))
             (EVar IDon_pe_z)))),35%positive)::
             (35%positive,AWeaken,7%positive)::nil
|}.

Definition on_pe_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_z) <= 0)%Z
    | 3%positive => (-1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_z) <= 0)%Z
    | 4%positive => (1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_z) <= 0)%Z
    | 5%positive => (-1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 6%positive => (-1 * (s IDon_pe_ch) <= 0 /\ 1 * (s IDon_pe_ch) <= 0 /\ 1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_z) <= 0)%Z
    | 7%positive => (-1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 8%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch)+ 1 * (s IDon_pe_gfp_dref_off204) <= 0)%Z
    | 9%positive => (-1 * (s IDon_pe_ch)+ 1 * (s IDon_pe_gfp_dref_off204) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 10%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 12%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 13%positive => (-1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 14%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 15%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 16%positive => (-1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 17%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 18%positive => (-1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 19%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 20%positive => (-1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 21%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ -1 * (s IDon_pe_bits) <= 0)%Z
    | 22%positive => (-1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_bits) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 23%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 25%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 27%positive => (1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 28%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 30%positive => (-1 * (s IDon_pe_ch) <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) + 1 <= 0 /\ -1 * (s IDon_pe_z) <= 0 /\ -1 * (s IDon_pe_ch) <= 0)%Z
    | 32%positive => (-1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) <= 0 /\ -1 * (s IDon_pe_ch) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDon_pe_ch) + 1 <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) <= 0 /\ -1 * (s IDon_pe_z) <= 0)%Z
    | 34%positive => (-1 * (s IDon_pe_z) <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) <= 0 /\ -1 * (s IDon_pe_ch) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDon_pe_ch) + 1 <= 0 /\ 1 * (s IDon_pe_ch)+ -1 * (s IDon_pe_gfp_dref_off204) <= 0 /\ -1 * (s IDon_pe_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition on_pe_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDon_pe_gfp_dref_off204)))%Q
    | 2%positive => ((s IDon_pe_z) + max0((s IDon_pe_gfp_dref_off204)))%Q
    | 3%positive => ((s IDon_pe_z) + max0((s IDon_pe_gfp_dref_off204)))%Q
    | 4%positive => ((s IDon_pe_z) + max0((s IDon_pe_gfp_dref_off204)))%Q
    | 5%positive => ((s IDon_pe_z)
                     + max0(-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)))%Q
    | 6%positive => ((s IDon_pe_z)
                     + max0(-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)))%Q
    | 7%positive => ((s IDon_pe_z)
                     + max0(-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)))%Q
    | 8%positive => ((s IDon_pe_z)
                     + max0(-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)))%Q
    | 9%positive => ((s IDon_pe_z))%Q
    | 10%positive => ((s IDon_pe_z)
                      + max0(-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)))%Q
    | 11%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 12%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 13%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 14%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 15%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 16%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 17%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 18%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 19%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 20%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 21%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 22%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 23%positive => ((1 # 1) + (s IDon_pe_z)
                      + max0(-1 - (s IDon_pe_ch)
                             + (s IDon_pe_gfp_dref_off204)))%Q
    | 24%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 25%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 26%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 27%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 28%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 29%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 30%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 31%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 32%positive => ((1 # 1) - (s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 33%positive => ((1 # 1) - (s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 34%positive => ((1 # 1) - (s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | 35%positive => (-(s IDon_pe_ch) + (s IDon_pe_gfp_dref_off204)
                      + (s IDon_pe_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition on_pe_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDon_pe_ch)
                                                            + (s IDon_pe_gfp_dref_off204)) (-1
                                                                    - (s IDon_pe_ch)
                                                                    + (s IDon_pe_gfp_dref_off204)));
                     (*-1 0*) F_max0_ge_0 (-1 - (s IDon_pe_ch)
                                           + (s IDon_pe_gfp_dref_off204))]
    | 9%positive => []
    | 10%positive => [(*0 1*) F_max0_pre_decrement (-(s IDon_pe_ch)
                                                    + (s IDon_pe_gfp_dref_off204)) (1)]
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
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  - (s IDon_pe_ch)
                                                                  + (s IDon_pe_gfp_dref_off204))) (F_check_ge (-1
                                                                    - (s IDon_pe_ch)
                                                                    + (s IDon_pe_gfp_dref_off204)) (0))]
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
    | 35%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDon_pe_ch)
                                                                    + (s IDon_pe_gfp_dref_off204)) (0))) (F_max0_ge_0 (-
                                                                    (s IDon_pe_ch)
                                                                    + (s IDon_pe_gfp_dref_off204)))]
    | _ => []
  end.


Theorem on_pe_ai_correct:
  forall s p' s', steps (g_start on_pe) s (g_edges on_pe) p' s' -> on_pe_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem on_pe_pot_correct:
  forall s p' s',
    steps (g_start on_pe) s (g_edges on_pe) p' s' ->
    (on_pe_pot (g_start on_pe) s >= on_pe_pot p' s')%Q.
Proof.
  check_lp on_pe_ai_correct on_pe_hints.
Qed.

