Require Import pasta.Pasta.

Notation IDnum_params_z := 1%positive.
Notation IDnum_params__tmp := 2%positive.
Notation IDnum_params__tmp1 := 3%positive.
Notation IDnum_params_mask := 4%positive.
Notation IDnum_params_count := 5%positive.
Notation IDnum_params_op := 6%positive.
Notation IDnum_params_pval := 7%positive.
Definition num_params : graph := {|
  g_start := 1%positive;
  g_end := 33%positive;
  g_edges := (1%positive,(AAssign IDnum_params_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDnum_params__tmp
             (Some (EVar IDnum_params_count))),3%positive)::
             (3%positive,(AAssign IDnum_params_mask (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDnum_params__tmp
             (Some (EAdd (EVar IDnum_params__tmp) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDnum_params__tmp) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),17%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDnum_params__tmp) (ENum (-1)))
             s) < (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDnum_params_mask)
             s) < (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDnum_params_mask)
             s) >= (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,14%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDnum_params__tmp1 None),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,33%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDnum_params_mask None),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,30%positive)::
             (20%positive,ANone,26%positive)::
             (20%positive,ANone,24%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDnum_params__tmp1 (Some (ENum (-17)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,33%positive)::
             (24%positive,(AAssign IDnum_params_mask
             (Some (EAdd (EVar IDnum_params_mask) (ENum (1))))),25%positive)::
             (25%positive,ANone,27%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDnum_params_z (Some (EAdd (ENum (1))
             (EVar IDnum_params_z)))),5%positive)::
             (30%positive,(AAssign IDnum_params__tmp1 (Some (ENum (-20)))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::nil
|}.

Definition num_params_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 3%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params_z) <= 0)%Z
    | 4%positive => (1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params_mask) <= 0 /\ -1 * (s IDnum_params_mask) <= 0)%Z
    | 5%positive => (-1 * (s IDnum_params_z) <= 0)%Z
    | 6%positive => (-1 * (s IDnum_params_z) <= 0)%Z
    | 7%positive => (-1 * (s IDnum_params_z) <= 0)%Z
    | 8%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0)%Z
    | 9%positive => (1 * (s IDnum_params__tmp) <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 10%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0 /\ -1 * (s IDnum_params_mask) <= 0)%Z
    | 11%positive => (-1 * (s IDnum_params_mask) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 12%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0 /\ 1 * (s IDnum_params_mask) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDnum_params_mask) + 1 <= 0 /\ 1 * (s IDnum_params__tmp) <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 14%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0)%Z
    | 15%positive => (1 * (s IDnum_params__tmp) <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 16%positive => (-1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp) <= 0)%Z
    | 17%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 19%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 21%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp1) + 17 <= 0 /\ -1 * (s IDnum_params__tmp1) + -17 <= 0)%Z
    | 23%positive => (-1 * (s IDnum_params__tmp1) + -17 <= 0 /\ 1 * (s IDnum_params__tmp1) + 17 <= 0 /\ -1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 26%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 28%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0)%Z
    | 30%positive => (-1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDnum_params__tmp) + 1 <= 0 /\ -1 * (s IDnum_params_z) <= 0 /\ 1 * (s IDnum_params__tmp1) + 20 <= 0 /\ -1 * (s IDnum_params__tmp1) + -20 <= 0)%Z
    | 32%positive => (-1 * (s IDnum_params__tmp1) + -20 <= 0 /\ 1 * (s IDnum_params__tmp1) + 20 <= 0 /\ -1 * (s IDnum_params_z) <= 0 /\ -1 * (s IDnum_params__tmp) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDnum_params_z) <= 0)%Z
    | _ => False
  end.

Definition num_params_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDnum_params_count)))%Q
    | 2%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params_count)))%Q
    | 3%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params__tmp)))%Q
    | 4%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params__tmp)))%Q
    | 5%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params__tmp)))%Q
    | 6%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 7%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 8%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 9%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 10%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 11%positive => ((s IDnum_params_z))%Q
    | 12%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 13%positive => ((s IDnum_params_z))%Q
    | 14%positive => ((s IDnum_params_z))%Q
    | 15%positive => ((s IDnum_params_z))%Q
    | 16%positive => ((s IDnum_params_z))%Q
    | 17%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 18%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 19%positive => ((s IDnum_params_z) + max0((s IDnum_params__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 22%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params__tmp))
                      + (1 # 3) * max0(20 + (s IDnum_params__tmp1)))%Q
    | 23%positive => ((s IDnum_params_z) + max0(-1 + (s IDnum_params__tmp))
                      + (1 # 3) * max0(20 + (s IDnum_params__tmp1)))%Q
    | 24%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 25%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 26%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 27%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 28%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 29%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 30%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 31%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 32%positive => ((1 # 1) + (s IDnum_params_z)
                      + max0(-1 + (s IDnum_params__tmp)))%Q
    | 33%positive => ((s IDnum_params_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition num_params_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDnum_params__tmp)) (-1
                                                                    + (s IDnum_params__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDnum_params__tmp))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDnum_params__tmp)) (-1
                                                                    + (s IDnum_params__tmp)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDnum_params__tmp))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_pre_decrement ((s IDnum_params__tmp)) (1)]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDnum_params__tmp));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                                    + (s IDnum_params__tmp1))) (F_check_ge (0) (0))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDnum_params__tmp))]
    | 33%positive => []
    | _ => []
  end.


Theorem num_params_ai_correct:
  forall s p' s', steps (g_start num_params) s (g_edges num_params) p' s' -> num_params_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem num_params_pot_correct:
  forall s p' s',
    steps (g_start num_params) s (g_edges num_params) p' s' ->
    (num_params_pot (g_start num_params) s >= num_params_pot p' s')%Q.
Proof.
  check_lp num_params_ai_correct num_params_hints.
Qed.

