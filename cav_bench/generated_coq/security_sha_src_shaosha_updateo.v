Require Import pasta.Pasta.

Notation IDsha_update_z := 1%positive.
Notation IDsha_update__tmp := 2%positive.
Notation IDsha_update_buffer := 3%positive.
Notation IDsha_update_count := 4%positive.
Notation IDsha_update_sha_info := 5%positive.
Definition sha_update : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDsha_update_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsha_update__tmp
             (Some (EVar IDsha_update_count))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,5%positive)::
             (4%positive,ANone,6%positive)::(5%positive,ANone,6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDsha_update__tmp)
             s) >= (eval (ENum (64)) s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDsha_update__tmp)
             s) < (eval (ENum (64)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDsha_update__tmp
             (Some (ESub (EVar IDsha_update__tmp) (ENum (64))))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDsha_update_z (Some (EAdd (ENum (1))
             (EVar IDsha_update_z)))),16%positive)::
             (16%positive,AWeaken,8%positive)::nil
|}.

Definition sha_update_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsha_update_z) <= 0 /\ 1 * (s IDsha_update_z) <= 0)%Z
    | 4%positive => (1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsha_update_z) <= 0 /\ 1 * (s IDsha_update_z) <= 0)%Z
    | 6%positive => (1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 7%positive => (-1 * (s IDsha_update_z) <= 0 /\ 1 * (s IDsha_update_z) <= 0)%Z
    | 8%positive => (-1 * (s IDsha_update_z) <= 0)%Z
    | 9%positive => (-1 * (s IDsha_update_z) <= 0 /\ 1 * (s IDsha_update__tmp) + -63 <= 0)%Z
    | 10%positive => (1 * (s IDsha_update__tmp) + -63 <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 11%positive => (-1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update__tmp) + 64 <= 0)%Z
    | 12%positive => (-1 * (s IDsha_update__tmp) + 64 <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 13%positive => (-1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDsha_update__tmp) <= 0 /\ -1 * (s IDsha_update_z) <= 0)%Z
    | 15%positive => (-1 * (s IDsha_update_z) <= 0 /\ -1 * (s IDsha_update__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDsha_update__tmp) <= 0 /\ -1 * (s IDsha_update_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition sha_update_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 64) * max0((s IDsha_update_count)))%Q
    | 2%positive => ((1 # 64) * max0((s IDsha_update_count))
                     + max0((s IDsha_update_z)))%Q
    | 3%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 4%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 5%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 6%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 7%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 8%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 9%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                     + max0((s IDsha_update_z)))%Q
    | 10%positive => ((s IDsha_update_z))%Q
    | 11%positive => ((1 # 64) * max0((s IDsha_update__tmp))
                      + max0((s IDsha_update_z)))%Q
    | 12%positive => ((1 # 1) + (1 # 64) * max0(-64 + (s IDsha_update__tmp))
                      + max0((s IDsha_update_z)))%Q
    | 13%positive => ((1 # 1) + (1 # 64) * max0((s IDsha_update__tmp))
                      + max0((s IDsha_update_z)))%Q
    | 14%positive => ((1 # 1) + (1 # 64) * max0((s IDsha_update__tmp))
                      + max0((s IDsha_update_z)))%Q
    | 15%positive => ((1 # 1) + (1 # 64) * max0((s IDsha_update__tmp))
                      + max0((s IDsha_update_z)))%Q
    | 16%positive => ((1 # 1) + max0(-1 + (s IDsha_update_z))
                      + (1 # 64) * max0((s IDsha_update__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition sha_update_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-0.015625 0*) F_max0_monotonic (F_check_ge ((s IDsha_update__tmp)) (-64
                                                                    + (s IDsha_update__tmp)));
                     (*-0.015625 0*) F_max0_ge_0 (-64 + (s IDsha_update__tmp));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsha_update_z))) (F_check_ge ((s IDsha_update_z)) (0))]
    | 10%positive => []
    | 11%positive => [(*-0.015625 0*) F_max0_pre_decrement ((s IDsha_update__tmp)) (64)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsha_update_z)) (0))) (F_max0_ge_0 ((s IDsha_update_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDsha_update_z))) (F_check_ge (-1
                                                                    + (s IDsha_update_z)) (0))]
    | _ => []
  end.


Theorem sha_update_ai_correct:
  forall s p' s', steps (g_start sha_update) s (g_edges sha_update) p' s' -> sha_update_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem sha_update_pot_correct:
  forall s p' s',
    steps (g_start sha_update) s (g_edges sha_update) p' s' ->
    (sha_update_pot (g_start sha_update) s >= sha_update_pot p' s')%Q.
Proof.
  check_lp sha_update_ai_correct sha_update_hints.
Qed.

