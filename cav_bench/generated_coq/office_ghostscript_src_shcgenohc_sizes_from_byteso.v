Require Import pasta.Pasta.

Notation IDhc_sizes_from_bytes_z := 1%positive.
Notation IDhc_sizes_from_bytes__tmp := 2%positive.
Notation IDhc_sizes_from_bytes_def_dref_off24 := 3%positive.
Notation IDhc_sizes_from_bytes_def_dref_off8 := 4%positive.
Notation IDhc_sizes_from_bytes_i := 5%positive.
Notation IDhc_sizes_from_bytes_l := 6%positive.
Notation IDhc_sizes_from_bytes_n := 7%positive.
Notation IDhc_sizes_from_bytes_num_counts := 8%positive.
Notation IDhc_sizes_from_bytes_num_values := 9%positive.
Notation IDhc_sizes_from_bytes_dbytes := 10%positive.
Notation IDhc_sizes_from_bytes_def := 11%positive.
Notation IDhc_sizes_from_bytes_num_bytes := 12%positive.
Definition hc_sizes_from_bytes : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDhc_sizes_from_bytes_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_num_counts) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_l) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDhc_sizes_from_bytes__tmp
             (Some (EVar IDhc_sizes_from_bytes_num_bytes))),6%positive)::
             (6%positive,(AAssign IDhc_sizes_from_bytes_num_counts
             (Some (ENum (0)))),7%positive)::
             (7%positive,(AAssign IDhc_sizes_from_bytes_num_values
             (Some (ENum (0)))),8%positive)::
             (8%positive,(AAssign IDhc_sizes_from_bytes_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_i) s) <
             (eval (EVar IDhc_sizes_from_bytes__tmp) s))%Z)),17%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_i) s) >=
             (eval (EVar IDhc_sizes_from_bytes__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDhc_sizes_from_bytes_def_dref_off8
             (Some (EVar IDhc_sizes_from_bytes_num_counts))),14%positive)::
             (14%positive,(AAssign IDhc_sizes_from_bytes_def_dref_off24
             (Some (EVar IDhc_sizes_from_bytes_num_values))),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDhc_sizes_from_bytes_n None),19%positive)::
             (19%positive,(AAssign IDhc_sizes_from_bytes_l None),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_l) s) >
             (eval (EVar IDhc_sizes_from_bytes_num_counts) s))%Z)),
             23%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizes_from_bytes_l) s) <=
             (eval (EVar IDhc_sizes_from_bytes_num_counts) s))%Z)),
             22%positive)::(22%positive,AWeaken,26%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDhc_sizes_from_bytes_num_counts
             (Some (EVar IDhc_sizes_from_bytes_l))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDhc_sizes_from_bytes_num_values
             (Some (EAdd (EVar IDhc_sizes_from_bytes_num_values)
             (EVar IDhc_sizes_from_bytes_n)))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDhc_sizes_from_bytes_i
             (Some (EAdd (EVar IDhc_sizes_from_bytes_i) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDhc_sizes_from_bytes_z
             (Some (EAdd (ENum (1)) (EVar IDhc_sizes_from_bytes_z)))),
             32%positive)::(32%positive,AWeaken,11%positive)::nil
|}.

Definition hc_sizes_from_bytes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0)%Z
    | 4%positive => (-1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l) <= 0)%Z
    | 5%positive => (-1 * (s IDhc_sizes_from_bytes_l) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0)%Z
    | 6%positive => (-1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l) <= 0)%Z
    | 7%positive => (-1 * (s IDhc_sizes_from_bytes_l) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0)%Z
    | 8%positive => (-1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_values) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_values) <= 0)%Z
    | 9%positive => (-1 * (s IDhc_sizes_from_bytes_num_values) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_values) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 10%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_counts) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_num_values) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_num_values) <= 0)%Z
    | 11%positive => (-1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 12%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes__tmp)+ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 13%positive => (1 * (s IDhc_sizes_from_bytes__tmp)+ -1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 14%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes__tmp)+ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 15%positive => (1 * (s IDhc_sizes_from_bytes__tmp)+ -1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 16%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ 1 * (s IDhc_sizes_from_bytes__tmp)+ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 17%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 19%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 21%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ 1 * (s IDhc_sizes_from_bytes_l)+ -1 * (s IDhc_sizes_from_bytes_num_counts) <= 0)%Z
    | 23%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_l)+ 1 * (s IDhc_sizes_from_bytes_num_counts) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDhc_sizes_from_bytes_l)+ 1 * (s IDhc_sizes_from_bytes_num_counts) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 26%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 28%positive => (-1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 30%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) <= 0)%Z
    | 31%positive => (-1 * (s IDhc_sizes_from_bytes_z) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) <= 0)%Z
    | 32%positive => (-1 * (s IDhc_sizes_from_bytes__tmp)+ 1 * (s IDhc_sizes_from_bytes_i) <= 0 /\ -1 * (s IDhc_sizes_from_bytes_i) + 1 <= 0 /\ -1 * (s IDhc_sizes_from_bytes_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition hc_sizes_from_bytes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDhc_sizes_from_bytes_num_bytes)))%Q
    | 2%positive => (max0((s IDhc_sizes_from_bytes_num_bytes))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 3%positive => (max0((s IDhc_sizes_from_bytes_num_bytes))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 4%positive => (max0((s IDhc_sizes_from_bytes_num_bytes))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 5%positive => (max0((s IDhc_sizes_from_bytes_num_bytes))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 6%positive => (max0((s IDhc_sizes_from_bytes__tmp))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 7%positive => (max0((s IDhc_sizes_from_bytes__tmp))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 8%positive => (max0((s IDhc_sizes_from_bytes__tmp))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 9%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                          - (s IDhc_sizes_from_bytes_i))
                     + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 10%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 11%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 12%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 13%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 14%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 15%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 16%positive => ((s IDhc_sizes_from_bytes_z))%Q
    | 17%positive => (max0((s IDhc_sizes_from_bytes__tmp)
                           - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 18%positive => ((1 # 1)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 19%positive => ((1 # 1)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 20%positive => ((1 # 1)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i))
                      + max0((s IDhc_sizes_from_bytes_z)))%Q
    | 21%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 22%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 23%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 24%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 25%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 26%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 27%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 28%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0(-1 + (s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 29%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0((s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 30%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0((s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 31%positive => ((1 # 1) + (s IDhc_sizes_from_bytes_z)
                      + max0((s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | 32%positive => ((s IDhc_sizes_from_bytes_z)
                      + max0((s IDhc_sizes_from_bytes__tmp)
                             - (s IDhc_sizes_from_bytes_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition hc_sizes_from_bytes_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDhc_sizes_from_bytes__tmp)
                                                             - (s IDhc_sizes_from_bytes_i)) (-1
                                                                    + (s IDhc_sizes_from_bytes__tmp)
                                                                    - (s IDhc_sizes_from_bytes_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDhc_sizes_from_bytes__tmp)
                                            - (s IDhc_sizes_from_bytes_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDhc_sizes_from_bytes_z))) (F_check_ge ((s IDhc_sizes_from_bytes_z)) (0))]
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement ((s IDhc_sizes_from_bytes__tmp)
                                                    - (s IDhc_sizes_from_bytes_i)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDhc_sizes_from_bytes_z))) (F_check_ge ((s IDhc_sizes_from_bytes_z)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDhc_sizes_from_bytes_z)) (0))) (F_max0_ge_0 ((s IDhc_sizes_from_bytes_z)))]
    | _ => []
  end.


Theorem hc_sizes_from_bytes_ai_correct:
  forall s p' s', steps (g_start hc_sizes_from_bytes) s (g_edges hc_sizes_from_bytes) p' s' -> hc_sizes_from_bytes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem hc_sizes_from_bytes_pot_correct:
  forall s p' s',
    steps (g_start hc_sizes_from_bytes) s (g_edges hc_sizes_from_bytes) p' s' ->
    (hc_sizes_from_bytes_pot (g_start hc_sizes_from_bytes) s >= hc_sizes_from_bytes_pot p' s')%Q.
Proof.
  check_lp hc_sizes_from_bytes_ai_correct hc_sizes_from_bytes_hints.
Qed.

