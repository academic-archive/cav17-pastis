Require Import pasta.Pasta.

Notation IDtrueRandAccum_z := 1%positive.
Notation IDtrueRandAccum__tmp := 2%positive.
Notation IDtrueRandAccum_c := 3%positive.
Notation IDtrueRandAccum_trueRandBits := 4%positive.
Notation IDtrueRandAccum_trueRandPending := 5%positive.
Notation IDtrueRandAccum_count := 6%positive.
Definition trueRandAccum : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDtrueRandAccum_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDtrueRandAccum_trueRandBits) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDtrueRandAccum__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDtrueRandAccum__tmp
             (Some (EVar IDtrueRandAccum_count))),6%positive)::
             (6%positive,(AAssign IDtrueRandAccum__tmp
             (Some (EAdd (EVar IDtrueRandAccum__tmp)
             (EVar IDtrueRandAccum_trueRandPending)))),7%positive)::
             (7%positive,(AAssign IDtrueRandAccum_trueRandPending
             (Some (ENum (0)))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDtrueRandAccum__tmp)
             s) > (eval (ENum (3072)) s))%Z)),11%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDtrueRandAccum__tmp)
             s) <= (eval (ENum (3072)) s))%Z)),10%positive)::
             (10%positive,AWeaken,15%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDtrueRandAccum__tmp
             (Some (ENum (3072)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDtrueRandAccum_trueRandBits) s) >=
             (eval (EVar IDtrueRandAccum__tmp) s))%Z)),28%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDtrueRandAccum_trueRandBits) s) <
             (eval (EVar IDtrueRandAccum__tmp) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDtrueRandAccum_c None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDtrueRandAccum_trueRandBits) s) <
             (eval (EVar IDtrueRandAccum__tmp) s))%Z)),25%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDtrueRandAccum_trueRandBits) s) >=
             (eval (EVar IDtrueRandAccum__tmp) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,31%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDtrueRandAccum_z (Some (EAdd (ENum (1))
             (EVar IDtrueRandAccum_z)))),18%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::nil
|}.

Definition trueRandAccum_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0)%Z
    | 3%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 4%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDtrueRandAccum__tmp) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 6%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0)%Z
    | 7%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 8%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0)%Z
    | 9%positive => (-1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 10%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 11%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum__tmp) + 3073 <= 0)%Z
    | 12%positive => (-1 * (s IDtrueRandAccum__tmp) + 3073 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 13%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum__tmp) + 3072 <= 0)%Z
    | 14%positive => (-1 * (s IDtrueRandAccum__tmp) + 3072 <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 15%positive => (-1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 16%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 18%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 19%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0)%Z
    | 20%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 21%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0)%Z
    | 22%positive => (False)%Z
    | 23%positive => (False)%Z
    | 24%positive => (False)%Z
    | 25%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 26%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0)%Z
    | 27%positive => (-1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum__tmp)+ 1 * (s IDtrueRandAccum_trueRandBits) + 1 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 28%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum__tmp)+ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 29%positive => (1 * (s IDtrueRandAccum__tmp)+ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | 30%positive => (1 * (s IDtrueRandAccum__tmp) + -3072 <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum__tmp)+ -1 * (s IDtrueRandAccum_trueRandBits) <= 0)%Z
    | 31%positive => (1 * (s IDtrueRandAccum__tmp)+ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandBits) <= 0 /\ 1 * (s IDtrueRandAccum_z) <= 0 /\ -1 * (s IDtrueRandAccum_z) <= 0 /\ 1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ -1 * (s IDtrueRandAccum_trueRandPending) <= 0 /\ 1 * (s IDtrueRandAccum__tmp) + -3072 <= 0)%Z
    | _ => False
  end.

Definition trueRandAccum_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (0)%Q
    | 2%positive => ((s IDtrueRandAccum_z))%Q
    | 3%positive => ((s IDtrueRandAccum_z))%Q
    | 4%positive => ((s IDtrueRandAccum_z))%Q
    | 5%positive => ((s IDtrueRandAccum_z))%Q
    | 6%positive => ((s IDtrueRandAccum_z))%Q
    | 7%positive => ((s IDtrueRandAccum_z))%Q
    | 8%positive => ((s IDtrueRandAccum_z))%Q
    | 9%positive => ((s IDtrueRandAccum_z))%Q
    | 10%positive => ((s IDtrueRandAccum_z))%Q
    | 11%positive => ((s IDtrueRandAccum_z))%Q
    | 12%positive => ((s IDtrueRandAccum_z))%Q
    | 13%positive => ((s IDtrueRandAccum_z))%Q
    | 14%positive => ((s IDtrueRandAccum_z))%Q
    | 15%positive => (max0((s IDtrueRandAccum_z)))%Q
    | 16%positive => (max0((s IDtrueRandAccum_z)))%Q
    | 17%positive => (0)%Q
    | 18%positive => (0)%Q
    | 19%positive => (0)%Q
    | 20%positive => (0)%Q
    | 21%positive => (0)%Q
    | 22%positive => (0)%Q
    | 23%positive => ((s IDtrueRandAccum_z) + max0(-(s IDtrueRandAccum_z)))%Q
    | 24%positive => ((s IDtrueRandAccum_z) + max0(-(s IDtrueRandAccum_z)))%Q
    | 25%positive => (0)%Q
    | 26%positive => (0)%Q
    | 27%positive => (0)%Q
    | 28%positive => (max0((s IDtrueRandAccum_z)))%Q
    | 29%positive => (max0((s IDtrueRandAccum_z)))%Q
    | 30%positive => (max0((s IDtrueRandAccum_z)))%Q
    | 31%positive => ((s IDtrueRandAccum_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition trueRandAccum_hints (p : node) (s : state) := 
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
    | 10%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDtrueRandAccum_z)) (0))) (F_max0_ge_0 ((s IDtrueRandAccum_z)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDtrueRandAccum_z)) (0))) (F_max0_ge_0 ((s IDtrueRandAccum_z)))]
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDtrueRandAccum_z))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDtrueRandAccum_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDtrueRandAccum_z)))]
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDtrueRandAccum_z))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDtrueRandAccum_z))) (F_check_ge ((s IDtrueRandAccum_z)) (0))]
    | 31%positive => []
    | _ => []
  end.


Theorem trueRandAccum_ai_correct:
  forall s p' s', steps (g_start trueRandAccum) s (g_edges trueRandAccum) p' s' -> trueRandAccum_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem trueRandAccum_pot_correct:
  forall s p' s',
    steps (g_start trueRandAccum) s (g_edges trueRandAccum) p' s' ->
    (trueRandAccum_pot (g_start trueRandAccum) s >= trueRandAccum_pot p' s')%Q.
Proof.
  check_lp trueRandAccum_ai_correct trueRandAccum_hints.
Qed.

