Require Import pasta.Pasta.

Notation IDmp_neg_z := 1%positive.
Notation IDmp_neg_global_precision := 2%positive.
Notation IDmp_neg_precision := 3%positive.
Notation IDmp_neg_r := 4%positive.
Definition mp_neg : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDmp_neg_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmp_neg_precision
             (Some (EVar IDmp_neg_global_precision))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDmp_neg_precision
             (Some (EAdd (EVar IDmp_neg_precision) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmp_neg_precision) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmp_neg_precision) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDmp_neg_z (Some (EAdd (ENum (1))
             (EVar IDmp_neg_z)))),4%positive)::nil
|}.

Definition mp_neg_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmp_neg_z) <= 0 /\ -1 * (s IDmp_neg_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmp_neg_z) <= 0 /\ 1 * (s IDmp_neg_z) <= 0)%Z
    | 4%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmp_neg_z) <= 0 /\ 1 * (s IDmp_neg_precision) + -1 <= 0 /\ -1 * (s IDmp_neg_precision) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDmp_neg_precision) + 1 <= 0 /\ 1 * (s IDmp_neg_precision) + -1 <= 0 /\ -1 * (s IDmp_neg_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmp_neg_z) <= 0)%Z
    | _ => False
  end.

Definition mp_neg_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDmp_neg_global_precision))%Q
    | 2%positive => ((s IDmp_neg_global_precision) + (s IDmp_neg_z))%Q
    | 3%positive => ((s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 4%positive => ((s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 5%positive => ((s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 6%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 7%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 8%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 9%positive => ((s IDmp_neg_z))%Q
    | 10%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 11%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | 12%positive => ((1 # 1) + (s IDmp_neg_precision) + (s IDmp_neg_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mp_neg_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-2 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDmp_neg_precision))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmp_neg_precision)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmp_neg_precision)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem mp_neg_ai_correct:
  forall s p' s', steps (g_start mp_neg) s (g_edges mp_neg) p' s' -> mp_neg_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mp_neg_pot_correct:
  forall s p' s',
    steps (g_start mp_neg) s (g_edges mp_neg) p' s' ->
    (mp_neg_pot (g_start mp_neg) s >= mp_neg_pot p' s')%Q.
Proof.
  check_lp mp_neg_ai_correct mp_neg_hints.
Qed.

