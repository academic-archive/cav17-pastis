Require Import pasta.Pasta.

Notation IDNRL_z := 1%positive.
Notation IDNRL__tmp := 2%positive.
Notation IDNRL_ch := 3%positive.
Notation IDNRL_old := 4%positive.
Notation IDNRL_n := 5%positive.
Notation IDNRL_phone := 6%positive.
Notation IDNRL_s := 7%positive.
Definition NRL : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDNRL_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDNRL__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDNRL__tmp (Some (EVar IDNRL_n))),
             5%positive)::(5%positive,(AAssign IDNRL_old None),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDNRL__tmp (Some (EAdd (EVar IDNRL__tmp)
             (ENum (-1))))),8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDNRL__tmp) s) >
             (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDNRL__tmp) s) <=
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDNRL_ch None),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (15%positive,ANone,18%positive)::
             (16%positive,(AAssign IDNRL_ch None),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDNRL_z (Some (EAdd (ENum (1))
             (EVar IDNRL_z)))),7%positive)::nil
|}.

Definition NRL_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 3%positive => (-1 * (s IDNRL_z) <= 0 /\ 1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDNRL__tmp) <= 0 /\ 1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 5%positive => (-1 * (s IDNRL_z) <= 0 /\ 1 * (s IDNRL_z) <= 0)%Z
    | 6%positive => (1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 7%positive => (-1 * (s IDNRL_z) <= 0)%Z
    | 8%positive => (-1 * (s IDNRL_z) <= 0)%Z
    | 9%positive => (-1 * (s IDNRL_z) <= 0)%Z
    | 10%positive => (-1 * (s IDNRL_z) <= 0 /\ 1 * (s IDNRL__tmp) <= 0)%Z
    | 11%positive => (1 * (s IDNRL__tmp) <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 12%positive => (-1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDNRL__tmp) + 1 <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 14%positive => (-1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDNRL__tmp) + 1 <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 16%positive => (-1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDNRL__tmp) + 1 <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 18%positive => (-1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDNRL__tmp) + 1 <= 0 /\ -1 * (s IDNRL_z) <= 0)%Z
    | 20%positive => (-1 * (s IDNRL_z) <= 0 /\ -1 * (s IDNRL__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition NRL_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDNRL_n)))%Q
    | 2%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL_n)))%Q
    | 3%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL_n)))%Q
    | 4%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL_n)))%Q
    | 5%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 6%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 7%positive => ((s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 8%positive => ((s IDNRL_z) + max0((s IDNRL__tmp)))%Q
    | 9%positive => (max0((s IDNRL__tmp)) + max0((s IDNRL_z)))%Q
    | 10%positive => (max0((s IDNRL__tmp)) + max0((s IDNRL_z)))%Q
    | 11%positive => ((s IDNRL_z))%Q
    | 12%positive => (max0((s IDNRL__tmp)) + max0((s IDNRL_z)))%Q
    | 13%positive => (max0((s IDNRL__tmp)) + max0((s IDNRL_z)))%Q
    | 14%positive => (max0((s IDNRL__tmp)) + max0((s IDNRL_z)))%Q
    | 15%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDNRL_z) + max0(-1 + (s IDNRL__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition NRL_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDNRL_z)) (0))) (F_max0_ge_0 ((s IDNRL_z)))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDNRL__tmp)) (-1
                                                                    + (s IDNRL__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDNRL__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDNRL_z))) (F_check_ge ((s IDNRL_z)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement ((s IDNRL__tmp)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDNRL_z))) (F_check_ge ((s IDNRL_z)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem NRL_ai_correct:
  forall s p' s', steps (g_start NRL) s (g_edges NRL) p' s' -> NRL_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem NRL_pot_correct:
  forall s p' s',
    steps (g_start NRL) s (g_edges NRL) p' s' ->
    (NRL_pot (g_start NRL) s >= NRL_pot p' s')%Q.
Proof.
  check_lp NRL_ai_correct NRL_hints.
Qed.

