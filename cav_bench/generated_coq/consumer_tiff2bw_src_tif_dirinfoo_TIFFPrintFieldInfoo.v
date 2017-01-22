Require Import pasta.Pasta.

Notation ID_TIFFPrintFieldInfo_z := 1%positive.
Notation ID_TIFFPrintFieldInfo_i := 2%positive.
Notation ID_TIFFPrintFieldInfo_tif_dref_off848 := 3%positive.
Notation ID_TIFFPrintFieldInfo_fd := 4%positive.
Notation ID_TIFFPrintFieldInfo_tif := 5%positive.
Definition _TIFFPrintFieldInfo : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign ID_TIFFPrintFieldInfo_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign ID_TIFFPrintFieldInfo_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFPrintFieldInfo_i) s) <
             (eval (EVar ID_TIFFPrintFieldInfo_tif_dref_off848) s))%Z)),
             8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFPrintFieldInfo_i) s) >=
             (eval (EVar ID_TIFFPrintFieldInfo_tif_dref_off848) s))%Z)),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign ID_TIFFPrintFieldInfo_i
             (Some (EAdd (EVar ID_TIFFPrintFieldInfo_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign ID_TIFFPrintFieldInfo_z
             (Some (EAdd (ENum (1)) (EVar ID_TIFFPrintFieldInfo_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition _TIFFPrintFieldInfo_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0)%Z
    | 3%positive => (-1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) <= 0)%Z
    | 4%positive => (-1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0)%Z
    | 5%positive => (-1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) <= 0)%Z
    | 6%positive => (-1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i)+ 1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0)%Z
    | 7%positive => (-1 * (s ID_TIFFPrintFieldInfo_i)+ 1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) <= 0)%Z
    | 8%positive => (-1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) + 1 <= 0)%Z
    | 9%positive => (1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) + 1 <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) <= 0)%Z
    | 10%positive => (-1 * (s ID_TIFFPrintFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) + 1 <= 0)%Z
    | 11%positive => (-1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) + 1 <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0)%Z
    | 12%positive => (1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) <= 0)%Z
    | 13%positive => (-1 * (s ID_TIFFPrintFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) + 1 <= 0 /\ 1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0)%Z
    | 14%positive => (1 * (s ID_TIFFPrintFieldInfo_i)+ -1 * (s ID_TIFFPrintFieldInfo_tif_dref_off848) <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFPrintFieldInfo_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition _TIFFPrintFieldInfo_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 2%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0((s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 3%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0(-(s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 4%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0(-(s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 5%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0(-(s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 6%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0(-(s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 7%positive => ((s ID_TIFFPrintFieldInfo_z))%Q
    | 8%positive => ((s ID_TIFFPrintFieldInfo_z)
                     + max0(-(s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 9%positive => ((1 # 1) + (s ID_TIFFPrintFieldInfo_z)
                     + max0(-1 - (s ID_TIFFPrintFieldInfo_i)
                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 10%positive => ((1 # 1) + (s ID_TIFFPrintFieldInfo_z)
                      + max0(-1 - (s ID_TIFFPrintFieldInfo_i)
                             + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 11%positive => ((1 # 1) + (s ID_TIFFPrintFieldInfo_z)
                      + max0(-(s ID_TIFFPrintFieldInfo_i)
                             + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 12%positive => ((1 # 1) + (s ID_TIFFPrintFieldInfo_z)
                      + max0(-(s ID_TIFFPrintFieldInfo_i)
                             + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 13%positive => ((1 # 1) + (s ID_TIFFPrintFieldInfo_z)
                      + max0(-(s ID_TIFFPrintFieldInfo_i)
                             + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | 14%positive => ((s ID_TIFFPrintFieldInfo_z)
                      + max0(-(s ID_TIFFPrintFieldInfo_i)
                             + (s ID_TIFFPrintFieldInfo_tif_dref_off848)))%Q
    | _ => (0 # 1)%Q
  end.

Definition _TIFFPrintFieldInfo_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s ID_TIFFPrintFieldInfo_i)
                                                            + (s ID_TIFFPrintFieldInfo_tif_dref_off848)) (-1
                                                                    - (s ID_TIFFPrintFieldInfo_i)
                                                                    + (s ID_TIFFPrintFieldInfo_tif_dref_off848)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s ID_TIFFPrintFieldInfo_i)
                                                                + (s ID_TIFFPrintFieldInfo_tif_dref_off848))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (-(s ID_TIFFPrintFieldInfo_i)
                                                    + (s ID_TIFFPrintFieldInfo_tif_dref_off848)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem _TIFFPrintFieldInfo_ai_correct:
  forall s p' s', steps (g_start _TIFFPrintFieldInfo) s (g_edges _TIFFPrintFieldInfo) p' s' -> _TIFFPrintFieldInfo_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem _TIFFPrintFieldInfo_pot_correct:
  forall s p' s',
    steps (g_start _TIFFPrintFieldInfo) s (g_edges _TIFFPrintFieldInfo) p' s' ->
    (_TIFFPrintFieldInfo_pot (g_start _TIFFPrintFieldInfo) s >= _TIFFPrintFieldInfo_pot p' s')%Q.
Proof.
  check_lp _TIFFPrintFieldInfo_ai_correct _TIFFPrintFieldInfo_hints.
Qed.

