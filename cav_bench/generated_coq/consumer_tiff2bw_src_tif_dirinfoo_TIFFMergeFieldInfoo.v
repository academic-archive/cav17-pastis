Require Import pasta.Pasta.

Notation ID_TIFFMergeFieldInfo_z := 1%positive.
Notation ID_TIFFMergeFieldInfo__tmp := 2%positive.
Notation ID_TIFFMergeFieldInfo_i := 3%positive.
Notation ID_TIFFMergeFieldInfo_tif_dref_off848 := 4%positive.
Notation ID_TIFFMergeFieldInfo_info := 5%positive.
Notation ID_TIFFMergeFieldInfo_n := 6%positive.
Notation ID_TIFFMergeFieldInfo_tif := 7%positive.
Definition _TIFFMergeFieldInfo : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign ID_TIFFMergeFieldInfo_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign ID_TIFFMergeFieldInfo__tmp
             (Some (EVar ID_TIFFMergeFieldInfo_n))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             s) > (eval (ENum (0)) s))%Z)),7%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             s) <= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,9%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign ID_TIFFMergeFieldInfo_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_i) s) <
             (eval (EVar ID_TIFFMergeFieldInfo__tmp) s))%Z)),24%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_i) s) >=
             (eval (EVar ID_TIFFMergeFieldInfo__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             s) > (eval (ENum (0)) s))%Z)),19%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             s) <= (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign ID_TIFFMergeFieldInfo_tif_dref_off848
             (Some (EAdd (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             (EVar ID_TIFFMergeFieldInfo__tmp)))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,23%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign ID_TIFFMergeFieldInfo_tif_dref_off848
             (Some (EAdd (EVar ID_TIFFMergeFieldInfo_tif_dref_off848)
             (EVar ID_TIFFMergeFieldInfo__tmp)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign ID_TIFFMergeFieldInfo_i
             (Some (EAdd (EVar ID_TIFFMergeFieldInfo_i) (ENum (1))))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign ID_TIFFMergeFieldInfo_z
             (Some (EAdd (ENum (1)) (EVar ID_TIFFMergeFieldInfo_z)))),
             30%positive)::(30%positive,AWeaken,12%positive)::nil
|}.

Definition _TIFFMergeFieldInfo_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 3%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 4%positive => (1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 5%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0)%Z
    | 6%positive => (1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 7%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0)%Z
    | 8%positive => (-1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 9%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 10%positive => (1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 11%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 12%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 13%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 14%positive => (1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 15%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0)%Z
    | 16%positive => (1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 17%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0)%Z
    | 18%positive => (-1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 19%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0)%Z
    | 20%positive => (-1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 21%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0)%Z
    | 22%positive => (1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_tif_dref_off848) + 1 <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 23%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFMergeFieldInfo__tmp)+ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 24%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 26%positive => (-1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 28%positive => (-1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) <= 0)%Z
    | 29%positive => (-1 * (s ID_TIFFMergeFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0)%Z
    | 30%positive => (-1 * (s ID_TIFFMergeFieldInfo__tmp)+ 1 * (s ID_TIFFMergeFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_i) + 1 <= 0 /\ -1 * (s ID_TIFFMergeFieldInfo_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition _TIFFMergeFieldInfo_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s ID_TIFFMergeFieldInfo_n)))%Q
    | 2%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo_n)))%Q
    | 3%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 4%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 5%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 6%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 7%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 8%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 9%positive => ((s ID_TIFFMergeFieldInfo_z)
                     + max0((s ID_TIFFMergeFieldInfo__tmp)))%Q
    | 10%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 11%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 12%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 13%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 14%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 15%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 16%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 17%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 18%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 19%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 20%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 21%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 22%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 23%positive => ((s ID_TIFFMergeFieldInfo_z))%Q
    | 24%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 25%positive => ((1 # 1) + (s ID_TIFFMergeFieldInfo_z)
                      + max0(-1 + (s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 26%positive => ((1 # 1) + (s ID_TIFFMergeFieldInfo_z)
                      + max0(-1 + (s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 27%positive => ((1 # 1) + (s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 28%positive => ((1 # 1) + (s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 29%positive => ((1 # 1) + (s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | 30%positive => ((s ID_TIFFMergeFieldInfo_z)
                      + max0((s ID_TIFFMergeFieldInfo__tmp)
                             - (s ID_TIFFMergeFieldInfo_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition _TIFFMergeFieldInfo_hints (p : node) (s : state) := 
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
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s ID_TIFFMergeFieldInfo__tmp)
                                                             - (s ID_TIFFMergeFieldInfo_i)) (-1
                                                                    + (s ID_TIFFMergeFieldInfo__tmp)
                                                                    - (s ID_TIFFMergeFieldInfo_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s ID_TIFFMergeFieldInfo__tmp)
                                            - (s ID_TIFFMergeFieldInfo_i))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s ID_TIFFMergeFieldInfo__tmp)
                                                             - (s ID_TIFFMergeFieldInfo_i)) (-1
                                                                    + (s ID_TIFFMergeFieldInfo__tmp)
                                                                    - (s ID_TIFFMergeFieldInfo_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s ID_TIFFMergeFieldInfo__tmp)
                                            - (s ID_TIFFMergeFieldInfo_i))]
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_pre_decrement ((s ID_TIFFMergeFieldInfo__tmp)
                                                     - (s ID_TIFFMergeFieldInfo_i)) (1)]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | _ => []
  end.


Theorem _TIFFMergeFieldInfo_ai_correct:
  forall s p' s', steps (g_start _TIFFMergeFieldInfo) s (g_edges _TIFFMergeFieldInfo) p' s' -> _TIFFMergeFieldInfo_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem _TIFFMergeFieldInfo_pot_correct:
  forall s p' s',
    steps (g_start _TIFFMergeFieldInfo) s (g_edges _TIFFMergeFieldInfo) p' s' ->
    (_TIFFMergeFieldInfo_pot (g_start _TIFFMergeFieldInfo) s >= _TIFFMergeFieldInfo_pot p' s')%Q.
Proof.
  check_lp _TIFFMergeFieldInfo_ai_correct _TIFFMergeFieldInfo_hints.
Qed.

