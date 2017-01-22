Require Import pasta.Pasta.

Notation IDhc_put_last_bits_proc_z := 1%positive.
Notation IDhc_put_last_bits_proc__tmp := 2%positive.
Notation IDhc_put_last_bits_proc__tmp1 := 3%positive.
Notation IDhc_put_last_bits_proc_c := 4%positive.
Notation IDhc_put_last_bits_proc_ss_dref_off24 := 5%positive.
Notation IDhc_put_last_bits_proc_ss_dref_off28 := 6%positive.
Notation IDhc_put_last_bits_proc_ss_dref_off32 := 7%positive.
Notation IDhc_put_last_bits_proc_bits := 8%positive.
Notation IDhc_put_last_bits_proc_bits_left := 9%positive.
Notation IDhc_put_last_bits_proc_q := 10%positive.
Notation IDhc_put_last_bits_proc_ss := 11%positive.
Definition hc_put_last_bits_proc : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDhc_put_last_bits_proc_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDhc_put_last_bits_proc__tmp1
             (Some (EVar IDhc_put_last_bits_proc_bits))),3%positive)::
             (3%positive,(AAssign IDhc_put_last_bits_proc__tmp
             (Some (EVar IDhc_put_last_bits_proc_bits_left))),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDhc_put_last_bits_proc__tmp) s) <
             (eval (ENum (32)) s))%Z)),12%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDhc_put_last_bits_proc__tmp) s) >=
             (eval (ENum (32)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDhc_put_last_bits_proc_ss_dref_off28
             (Some (EVar IDhc_put_last_bits_proc__tmp1))),9%positive)::
             (9%positive,(AAssign IDhc_put_last_bits_proc_ss_dref_off32
             (Some (EVar IDhc_put_last_bits_proc__tmp))),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDhc_put_last_bits_proc_c None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDhc_put_last_bits_proc_ss_dref_off24)
             s) <> (eval (ENum (0)) s))%Z)),17%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDhc_put_last_bits_proc_ss_dref_off24)
             s) = (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,20%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDhc_put_last_bits_proc_c None),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDhc_put_last_bits_proc__tmp1 None),
             21%positive)::
             (21%positive,(AAssign IDhc_put_last_bits_proc__tmp
             (Some (EAdd (EVar IDhc_put_last_bits_proc__tmp) (ENum (8))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDhc_put_last_bits_proc_z
             (Some (EAdd (ENum (1)) (EVar IDhc_put_last_bits_proc_z)))),
             25%positive)::(25%positive,AWeaken,6%positive)::nil
|}.

Definition hc_put_last_bits_proc_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 3%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 4%positive => (1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 5%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 6%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 7%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc__tmp) + 32 <= 0)%Z
    | 8%positive => (-1 * (s IDhc_put_last_bits_proc__tmp) + 32 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 9%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc__tmp) + 32 <= 0)%Z
    | 10%positive => (-1 * (s IDhc_put_last_bits_proc__tmp) + 32 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc_ss_dref_off32) + 32 <= 0)%Z
    | 11%positive => (-1 * (s IDhc_put_last_bits_proc_ss_dref_off32) + 32 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ -1 * (s IDhc_put_last_bits_proc__tmp) + 32 <= 0)%Z
    | 12%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0)%Z
    | 13%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 14%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0)%Z
    | 15%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 16%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0 /\ 1 * (s IDhc_put_last_bits_proc_ss_dref_off24) <= 0 /\ -1 * (s IDhc_put_last_bits_proc_ss_dref_off24) <= 0)%Z
    | 17%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0)%Z
    | 18%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 19%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0)%Z
    | 20%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 21%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -31 <= 0)%Z
    | 22%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -39 <= 0)%Z
    | 23%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -39 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) <= 0)%Z
    | 24%positive => (-1 * (s IDhc_put_last_bits_proc_z) <= 0 /\ 1 * (s IDhc_put_last_bits_proc__tmp) + -39 <= 0)%Z
    | 25%positive => (1 * (s IDhc_put_last_bits_proc__tmp) + -39 <= 0 /\ -1 * (s IDhc_put_last_bits_proc_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition hc_put_last_bits_proc_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0(39
                                    - (s IDhc_put_last_bits_proc_bits_left)))%Q
    | 2%positive => ((1 # 8) * max0(39
                                    - (s IDhc_put_last_bits_proc_bits_left))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 3%positive => ((1 # 8) * max0(39
                                    - (s IDhc_put_last_bits_proc_bits_left))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 4%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 5%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 6%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 7%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 8%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 9%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                     + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 10%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                      + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 11%positive => ((s IDhc_put_last_bits_proc_z))%Q
    | 12%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                      + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 13%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                      + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 14%positive => ((1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp))
                      + max0((s IDhc_put_last_bits_proc_z)))%Q
    | 15%positive => ((s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 16%positive => ((s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 17%positive => ((s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(31 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(31 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(31 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(31 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 22%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 23%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 24%positive => ((1 # 1) + (s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | 25%positive => ((s IDhc_put_last_bits_proc_z)
                      + (1 # 8) * max0(39 - (s IDhc_put_last_bits_proc__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition hc_put_last_bits_proc_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge (39
                                                                 - (s IDhc_put_last_bits_proc__tmp)) (31
                                                                    - (s IDhc_put_last_bits_proc__tmp)));
                      (*-0.125 0*) F_max0_ge_0 (31
                                                - (s IDhc_put_last_bits_proc__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDhc_put_last_bits_proc_z))) (F_check_ge ((s IDhc_put_last_bits_proc_z)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDhc_put_last_bits_proc_z))) (F_check_ge ((s IDhc_put_last_bits_proc_z)) (0))]
    | 15%positive => []
    | 16%positive => [(*0 0.125*) F_max0_pre_decrement (39
                                                        - (s IDhc_put_last_bits_proc__tmp)) (8)]
    | 17%positive => [(*-0.125 0*) F_max0_pre_decrement (39
                                                         - (s IDhc_put_last_bits_proc__tmp)) (8)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDhc_put_last_bits_proc_z)) (0))) (F_max0_ge_0 ((s IDhc_put_last_bits_proc_z)))]
    | _ => []
  end.


Theorem hc_put_last_bits_proc_ai_correct:
  forall s p' s', steps (g_start hc_put_last_bits_proc) s (g_edges hc_put_last_bits_proc) p' s' -> hc_put_last_bits_proc_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem hc_put_last_bits_proc_pot_correct:
  forall s p' s',
    steps (g_start hc_put_last_bits_proc) s (g_edges hc_put_last_bits_proc) p' s' ->
    (hc_put_last_bits_proc_pot (g_start hc_put_last_bits_proc) s >= hc_put_last_bits_proc_pot p' s')%Q.
Proof.
  check_lp hc_put_last_bits_proc_ai_correct hc_put_last_bits_proc_hints.
Qed.

