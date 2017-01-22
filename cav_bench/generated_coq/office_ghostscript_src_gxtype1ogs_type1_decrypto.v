Require Import pasta.Pasta.

Notation IDgs_type1_decrypt_z := 1%positive.
Notation IDgs_type1_decrypt__tmp := 2%positive.
Notation IDgs_type1_decrypt_ch := 3%positive.
Notation IDgs_type1_decrypt_count := 4%positive.
Notation IDgs_type1_decrypt_pstate_dref := 5%positive.
Notation IDgs_type1_decrypt_state := 6%positive.
Notation IDgs_type1_decrypt_dest := 7%positive.
Notation IDgs_type1_decrypt_len := 8%positive.
Notation IDgs_type1_decrypt_pstate := 9%positive.
Notation IDgs_type1_decrypt_src := 10%positive.
Definition gs_type1_decrypt : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDgs_type1_decrypt_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgs_type1_decrypt__tmp
             (Some (EVar IDgs_type1_decrypt_len))),3%positive)::
             (3%positive,(AAssign IDgs_type1_decrypt_state
             (Some (EVar IDgs_type1_decrypt_pstate_dref))),4%positive)::
             (4%positive,(AAssign IDgs_type1_decrypt_count
             (Some (EVar IDgs_type1_decrypt__tmp))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgs_type1_decrypt_count) s) <>
             (eval (ENum (0)) s))%Z)),12%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgs_type1_decrypt_count) s) =
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDgs_type1_decrypt_pstate_dref
             (Some (EVar IDgs_type1_decrypt_state))),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDgs_type1_decrypt_ch None),14%positive)::
             (14%positive,(AAssign IDgs_type1_decrypt_state None),
             15%positive)::
             (15%positive,(AAssign IDgs_type1_decrypt_count
             (Some (EAdd (EVar IDgs_type1_decrypt_count) (ENum (-1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDgs_type1_decrypt_z
             (Some (EAdd (ENum (1)) (EVar IDgs_type1_decrypt_z)))),
             19%positive)::(19%positive,AWeaken,7%positive)::nil
|}.

Definition gs_type1_decrypt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_type1_decrypt_z) <= 0 /\ -1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0 /\ 1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 4%positive => (1 * (s IDgs_type1_decrypt_z) <= 0 /\ -1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0 /\ 1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 6%positive => (1 * (s IDgs_type1_decrypt_z) <= 0 /\ -1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0 /\ 1 * (s IDgs_type1_decrypt_count) <= 0 /\ -1 * (s IDgs_type1_decrypt_count) <= 0)%Z
    | 9%positive => (-1 * (s IDgs_type1_decrypt_count) <= 0 /\ 1 * (s IDgs_type1_decrypt_count) <= 0 /\ -1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 10%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0 /\ 1 * (s IDgs_type1_decrypt_count) <= 0 /\ -1 * (s IDgs_type1_decrypt_count) <= 0)%Z
    | 11%positive => (-1 * (s IDgs_type1_decrypt_count) <= 0 /\ 1 * (s IDgs_type1_decrypt_count) <= 0 /\ -1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 12%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 14%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 15%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 16%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 17%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 18%positive => (-1 * (s IDgs_type1_decrypt_z) <= 0)%Z
    | 19%positive => (-1 * (s IDgs_type1_decrypt_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gs_type1_decrypt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDgs_type1_decrypt_len))%Q
    | 2%positive => ((s IDgs_type1_decrypt_len)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 3%positive => ((s IDgs_type1_decrypt__tmp)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 4%positive => ((s IDgs_type1_decrypt__tmp)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 5%positive => ((s IDgs_type1_decrypt_count)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 6%positive => ((s IDgs_type1_decrypt_count)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 7%positive => ((s IDgs_type1_decrypt_count)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 8%positive => ((s IDgs_type1_decrypt_count)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 9%positive => ((s IDgs_type1_decrypt_count)
                     + max0((s IDgs_type1_decrypt_z)))%Q
    | 10%positive => ((s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 11%positive => ((s IDgs_type1_decrypt_z))%Q
    | 12%positive => ((s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 13%positive => ((s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 14%positive => ((s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 15%positive => ((s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 16%positive => ((1 # 1) + (s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 17%positive => ((1 # 1) + (s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 18%positive => ((1 # 1) + (s IDgs_type1_decrypt_count)
                      + max0((s IDgs_type1_decrypt_z)))%Q
    | 19%positive => ((1 # 1) + (s IDgs_type1_decrypt_count)
                      + max0(-1 + (s IDgs_type1_decrypt_z)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_type1_decrypt_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_type1_decrypt_z))) (F_check_ge ((s IDgs_type1_decrypt_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_type1_decrypt_count))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_type1_decrypt_count)) (0))) (F_max0_ge_0 ((s IDgs_type1_decrypt_count)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_type1_decrypt_z)) (0))) (F_max0_ge_0 ((s IDgs_type1_decrypt_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDgs_type1_decrypt_z))) (F_check_ge (-1
                                                                    + (s IDgs_type1_decrypt_z)) (0))]
    | _ => []
  end.


Theorem gs_type1_decrypt_ai_correct:
  forall s p' s', steps (g_start gs_type1_decrypt) s (g_edges gs_type1_decrypt) p' s' -> gs_type1_decrypt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_type1_decrypt_pot_correct:
  forall s p' s',
    steps (g_start gs_type1_decrypt) s (g_edges gs_type1_decrypt) p' s' ->
    (gs_type1_decrypt_pot (g_start gs_type1_decrypt) s >= gs_type1_decrypt_pot p' s')%Q.
Proof.
  check_lp gs_type1_decrypt_ai_correct gs_type1_decrypt_hints.
Qed.

