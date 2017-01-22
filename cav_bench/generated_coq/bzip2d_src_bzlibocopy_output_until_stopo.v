Require Import pasta.Pasta.

Notation IDcopy_output_until_stop_z := 1%positive.
Notation IDcopy_output_until_stop_progress_out := 2%positive.
Notation IDcopy_output_until_stop_s_dref_off116 := 3%positive.
Notation IDcopy_output_until_stop_s_dref_off120 := 4%positive.
Notation IDcopy_output_until_stop_s := 5%positive.
Definition copy_output_until_stop : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign IDcopy_output_until_stop_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcopy_output_until_stop_progress_out
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,21%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcopy_output_until_stop_s_dref_off120)
             s) >= (eval (EVar IDcopy_output_until_stop_s_dref_off116)
             s))%Z)),18%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcopy_output_until_stop_s_dref_off120)
             s) < (eval (EVar IDcopy_output_until_stop_s_dref_off116)
             s))%Z)),8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDcopy_output_until_stop_progress_out
             (Some (ENum (1)))),10%positive)::
             (10%positive,(AAssign IDcopy_output_until_stop_s_dref_off120
             (Some (EAdd (EVar IDcopy_output_until_stop_s_dref_off120)
             (ENum (1))))),11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,14%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcopy_output_until_stop_z
             (Some (EAdd (ENum (1)) (EVar IDcopy_output_until_stop_z)))),
             17%positive)::(17%positive,AWeaken,5%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,23%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::nil
|}.

Definition copy_output_until_stop_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0)%Z
    | 4%positive => (-1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0)%Z
    | 6%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0)%Z
    | 8%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) + 1 <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0)%Z
    | 10%positive => (-1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) + 1 <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0)%Z
    | 12%positive => (-1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0)%Z
    | 14%positive => (-1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0)%Z
    | 16%positive => (-1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcopy_output_until_stop_progress_out) + 1 <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_s_dref_off116)+ 1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_s_dref_off116)+ -1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0)%Z
    | 19%positive => (1 * (s IDcopy_output_until_stop_s_dref_off116)+ -1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0 /\ 1 * (s IDcopy_output_until_stop_s_dref_off116)+ -1 * (s IDcopy_output_until_stop_s_dref_off120) <= 0)%Z
    | 21%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0)%Z
    | 22%positive => (-1 * (s IDcopy_output_until_stop_z) <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ 1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0)%Z
    | 23%positive => (1 * (s IDcopy_output_until_stop_progress_out) + -1 <= 0 /\ -1 * (s IDcopy_output_until_stop_progress_out) <= 0 /\ -1 * (s IDcopy_output_until_stop_z) <= 0)%Z
    | _ => False
  end.

Definition copy_output_until_stop_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcopy_output_until_stop_s_dref_off116)
                          - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 2%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 3%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 4%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 5%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 6%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 7%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 8%positive => ((s IDcopy_output_until_stop_z)
                     + max0((s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 9%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                     + max0(-1 + (s IDcopy_output_until_stop_s_dref_off116)
                            - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 10%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0(-1 + (s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 11%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 12%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 13%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 14%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 15%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 16%positive => ((1 # 1) + (s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 17%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 18%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 19%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 20%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 21%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 22%positive => ((s IDcopy_output_until_stop_z)
                      + max0((s IDcopy_output_until_stop_s_dref_off116)
                             - (s IDcopy_output_until_stop_s_dref_off120)))%Q
    | 23%positive => ((s IDcopy_output_until_stop_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition copy_output_until_stop_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcopy_output_until_stop_s_dref_off116)
                                                    - (s IDcopy_output_until_stop_s_dref_off120)) (1)]
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
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcopy_output_until_stop_s_dref_off116)
                                                             - (s IDcopy_output_until_stop_s_dref_off120)) (-1
                                                                    + (s IDcopy_output_until_stop_s_dref_off116)
                                                                    - (s IDcopy_output_until_stop_s_dref_off120)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcopy_output_until_stop_s_dref_off116)
                                            - (s IDcopy_output_until_stop_s_dref_off120))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcopy_output_until_stop_s_dref_off116)
                                                             - (s IDcopy_output_until_stop_s_dref_off120)) (-1
                                                                    + (s IDcopy_output_until_stop_s_dref_off116)
                                                                    - (s IDcopy_output_until_stop_s_dref_off120)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcopy_output_until_stop_s_dref_off116)
                                            - (s IDcopy_output_until_stop_s_dref_off120))]
    | 23%positive => []
    | _ => []
  end.


Theorem copy_output_until_stop_ai_correct:
  forall s p' s', steps (g_start copy_output_until_stop) s (g_edges copy_output_until_stop) p' s' -> copy_output_until_stop_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem copy_output_until_stop_pot_correct:
  forall s p' s',
    steps (g_start copy_output_until_stop) s (g_edges copy_output_until_stop) p' s' ->
    (copy_output_until_stop_pot (g_start copy_output_until_stop) s >= copy_output_until_stop_pot p' s')%Q.
Proof.
  check_lp copy_output_until_stop_ai_correct copy_output_until_stop_hints.
Qed.

