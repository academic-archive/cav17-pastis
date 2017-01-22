Require Import pasta.Pasta.

Notation IDencode_mcu_DC_refine_z := 1%positive.
Notation IDencode_mcu_DC_refine_Al := 2%positive.
Notation IDencode_mcu_DC_refine_blkn := 3%positive.
Notation IDencode_mcu_DC_refine_cinfo_dref_off272 := 4%positive.
Notation IDencode_mcu_DC_refine_cinfo_dref_off360 := 5%positive.
Notation IDencode_mcu_DC_refine_cinfo_dref_off416 := 6%positive.
Notation IDencode_mcu_DC_refine_temp := 7%positive.
Notation IDencode_mcu_DC_refine_MCU_data := 8%positive.
Notation IDencode_mcu_DC_refine_cinfo := 9%positive.
Definition encode_mcu_DC_refine : graph := {|
  g_start := 1%positive;
  g_end := 22%positive;
  g_edges := (1%positive,(AAssign IDencode_mcu_DC_refine_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDencode_mcu_DC_refine_Al
             (Some (EVar IDencode_mcu_DC_refine_cinfo_dref_off416))),
             3%positive)::(3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off272)
             s) <> (eval (ENum (0)) s))%Z)),6%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off272)
             s) = (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,10%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (7%positive,ANone,9%positive)::(8%positive,ANone,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDencode_mcu_DC_refine_blkn
             (Some (ENum (0)))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_blkn) s) <
             (eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off360) s))%Z)),
             23%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_blkn) s) >=
             (eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off360) s))%Z)),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off272)
             s) <> (eval (ENum (0)) s))%Z)),17%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_DC_refine_cinfo_dref_off272)
             s) = (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,22%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (18%positive,ANone,20%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDencode_mcu_DC_refine_temp None),
             25%positive)::(25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDencode_mcu_DC_refine_blkn
             (Some (EAdd (EVar IDencode_mcu_DC_refine_blkn) (ENum (1))))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDencode_mcu_DC_refine_z
             (Some (EAdd (ENum (1)) (EVar IDencode_mcu_DC_refine_z)))),
             30%positive)::(30%positive,AWeaken,13%positive)::nil
|}.

Definition encode_mcu_DC_refine_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 3%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 4%positive => (1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 5%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off272) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off272) <= 0)%Z
    | 6%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 7%positive => (1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 8%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 9%positive => (1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 10%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 11%positive => (1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 12%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 13%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 14%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0)%Z
    | 15%positive => (-1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 16%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off272) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off272) <= 0)%Z
    | 17%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0)%Z
    | 18%positive => (-1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 19%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0)%Z
    | 20%positive => (-1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 21%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0)%Z
    | 22%positive => (-1 * (s IDencode_mcu_DC_refine_blkn)+ 1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 23%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) + 1 <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 25%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) + 1 <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) <= 0)%Z
    | 27%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) + 1 <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) <= 0)%Z
    | 29%positive => (-1 * (s IDencode_mcu_DC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_blkn) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDencode_mcu_DC_refine_blkn) + 1 <= 0 /\ 1 * (s IDencode_mcu_DC_refine_blkn)+ -1 * (s IDencode_mcu_DC_refine_cinfo_dref_off360) <= 0 /\ -1 * (s IDencode_mcu_DC_refine_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition encode_mcu_DC_refine_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 2%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 3%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 4%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 5%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 6%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 7%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 8%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 9%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                     + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 10%positive => (max0((s IDencode_mcu_DC_refine_cinfo_dref_off360))
                      + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 11%positive => (max0(-(s IDencode_mcu_DC_refine_blkn)
                           + (s IDencode_mcu_DC_refine_cinfo_dref_off360))
                      + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 12%positive => (max0(-(s IDencode_mcu_DC_refine_blkn)
                           + (s IDencode_mcu_DC_refine_cinfo_dref_off360))
                      + max0((s IDencode_mcu_DC_refine_z)))%Q
    | 13%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 14%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 15%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 16%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 17%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 18%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 19%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 20%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 21%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 22%positive => ((s IDencode_mcu_DC_refine_z))%Q
    | 23%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 24%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-1 - (s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 25%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-1 - (s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 26%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-1 - (s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 27%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 28%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 29%positive => ((1 # 1) + (s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | 30%positive => ((s IDencode_mcu_DC_refine_z)
                      + max0(-(s IDencode_mcu_DC_refine_blkn)
                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)))%Q
    | _ => (0 # 1)%Q
  end.

Definition encode_mcu_DC_refine_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDencode_mcu_DC_refine_z))) (F_check_ge ((s IDencode_mcu_DC_refine_z)) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDencode_mcu_DC_refine_blkn)
                                                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)) (-1
                                                                    - (s IDencode_mcu_DC_refine_blkn)
                                                                    + (s IDencode_mcu_DC_refine_cinfo_dref_off360)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDencode_mcu_DC_refine_blkn)
                                            + (s IDencode_mcu_DC_refine_cinfo_dref_off360))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDencode_mcu_DC_refine_blkn)
                                                             + (s IDencode_mcu_DC_refine_cinfo_dref_off360)) (-1
                                                                    - (s IDencode_mcu_DC_refine_blkn)
                                                                    + (s IDencode_mcu_DC_refine_cinfo_dref_off360)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDencode_mcu_DC_refine_blkn)
                                            + (s IDencode_mcu_DC_refine_cinfo_dref_off360))]
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDencode_mcu_DC_refine_blkn)
                                                     + (s IDencode_mcu_DC_refine_cinfo_dref_off360)) (1)]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | _ => []
  end.


Theorem encode_mcu_DC_refine_ai_correct:
  forall s p' s', steps (g_start encode_mcu_DC_refine) s (g_edges encode_mcu_DC_refine) p' s' -> encode_mcu_DC_refine_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem encode_mcu_DC_refine_pot_correct:
  forall s p' s',
    steps (g_start encode_mcu_DC_refine) s (g_edges encode_mcu_DC_refine) p' s' ->
    (encode_mcu_DC_refine_pot (g_start encode_mcu_DC_refine) s >= encode_mcu_DC_refine_pot p' s')%Q.
Proof.
  check_lp encode_mcu_DC_refine_ai_correct encode_mcu_DC_refine_hints.
Qed.

