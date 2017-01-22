Require Import pasta.Pasta.

Notation IDCoefficients_40_159_z := 1%positive.
Notation IDCoefficients_40_159_i := 2%positive.
Notation IDCoefficients_40_159_LARp := 3%positive.
Notation IDCoefficients_40_159_LARpp_j := 4%positive.
Definition Coefficients_40_159 : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDCoefficients_40_159_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDCoefficients_40_159_i (Some (ENum (1)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDCoefficients_40_159_i) s) <=
             (eval (ENum (8)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDCoefficients_40_159_i) s) >
             (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDCoefficients_40_159_i
             (Some (EAdd (EVar IDCoefficients_40_159_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDCoefficients_40_159_z
             (Some (EAdd (ENum (1)) (EVar IDCoefficients_40_159_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition Coefficients_40_159_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0)%Z
    | 3%positive => (-1 * (s IDCoefficients_40_159_z) <= 0 /\ 1 * (s IDCoefficients_40_159_z) <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -1 <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDCoefficients_40_159_i) + 1 <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -1 <= 0 /\ 1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0)%Z
    | 5%positive => (-1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 1 <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -9 <= 0)%Z
    | 6%positive => (1 * (s IDCoefficients_40_159_i) + -9 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 9 <= 0)%Z
    | 7%positive => (-1 * (s IDCoefficients_40_159_i) + 9 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -9 <= 0)%Z
    | 8%positive => (-1 * (s IDCoefficients_40_159_i) + 1 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -8 <= 0)%Z
    | 9%positive => (1 * (s IDCoefficients_40_159_i) + -8 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDCoefficients_40_159_i) + 1 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -8 <= 0)%Z
    | 11%positive => (-1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 2 <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -9 <= 0)%Z
    | 12%positive => (1 * (s IDCoefficients_40_159_i) + -9 <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 2 <= 0 /\ -1 * (s IDCoefficients_40_159_z) <= 0)%Z
    | 13%positive => (-1 * (s IDCoefficients_40_159_z) <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 2 <= 0 /\ 1 * (s IDCoefficients_40_159_i) + -9 <= 0)%Z
    | 14%positive => (1 * (s IDCoefficients_40_159_i) + -9 <= 0 /\ -1 * (s IDCoefficients_40_159_i) + 2 <= 0 /\ -1 * (s IDCoefficients_40_159_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Coefficients_40_159_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDCoefficients_40_159_z))%Q
    | 3%positive => ((s IDCoefficients_40_159_z)
                     + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 4%positive => ((s IDCoefficients_40_159_z)
                     + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 5%positive => ((s IDCoefficients_40_159_z)
                     + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 6%positive => ((s IDCoefficients_40_159_z)
                     + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 7%positive => ((s IDCoefficients_40_159_z))%Q
    | 8%positive => ((s IDCoefficients_40_159_z)
                     + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 9%positive => ((1 # 1) + (s IDCoefficients_40_159_z)
                     + max0(8 - (s IDCoefficients_40_159_i)))%Q
    | 10%positive => ((1 # 1) + (s IDCoefficients_40_159_z)
                      + max0(8 - (s IDCoefficients_40_159_i)))%Q
    | 11%positive => ((1 # 1) + (s IDCoefficients_40_159_z)
                      + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 12%positive => ((1 # 1) + (s IDCoefficients_40_159_z)
                      + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 13%positive => ((1 # 1) + (s IDCoefficients_40_159_z)
                      + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | 14%positive => ((s IDCoefficients_40_159_z)
                      + max0(9 - (s IDCoefficients_40_159_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Coefficients_40_159_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (9
                                                            - (s IDCoefficients_40_159_i)) (8
                                                                    - (s IDCoefficients_40_159_i)));
                     (*-1 0*) F_max0_ge_0 (8 - (s IDCoefficients_40_159_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                    - (s IDCoefficients_40_159_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem Coefficients_40_159_ai_correct:
  forall s p' s', steps (g_start Coefficients_40_159) s (g_edges Coefficients_40_159) p' s' -> Coefficients_40_159_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Coefficients_40_159_pot_correct:
  forall s p' s',
    steps (g_start Coefficients_40_159) s (g_edges Coefficients_40_159) p' s' ->
    (Coefficients_40_159_pot (g_start Coefficients_40_159) s >= Coefficients_40_159_pot p' s')%Q.
Proof.
  check_lp Coefficients_40_159_ai_correct Coefficients_40_159_hints.
Qed.

