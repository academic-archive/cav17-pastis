Require Import pasta.Pasta.

Notation IDCoefficients_0_12_z := 1%positive.
Notation IDCoefficients_0_12_i := 2%positive.
Notation IDCoefficients_0_12_ltmp := 3%positive.
Notation IDCoefficients_0_12_LARp := 4%positive.
Notation IDCoefficients_0_12_LARpp_j := 5%positive.
Notation IDCoefficients_0_12_LARpp_j_1 := 6%positive.
Definition Coefficients_0_12 : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDCoefficients_0_12_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDCoefficients_0_12_i (Some (ENum (1)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDCoefficients_0_12_i) s) <=
             (eval (ENum (8)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDCoefficients_0_12_i) s) >
             (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDCoefficients_0_12_ltmp None),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,13%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,14%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDCoefficients_0_12_ltmp None),
             15%positive)::(15%positive,AWeaken,16%positive)::
             (16%positive,ANone,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,19%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDCoefficients_0_12_i
             (Some (EAdd (EVar IDCoefficients_0_12_i) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDCoefficients_0_12_z
             (Some (EAdd (ENum (1)) (EVar IDCoefficients_0_12_z)))),
             24%positive)::(24%positive,AWeaken,5%positive)::nil
|}.

Definition Coefficients_0_12_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0)%Z
    | 3%positive => (-1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -1 <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -1 <= 0 /\ 1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0)%Z
    | 5%positive => (-1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -9 <= 0)%Z
    | 6%positive => (1 * (s IDCoefficients_0_12_i) + -9 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 9 <= 0)%Z
    | 7%positive => (-1 * (s IDCoefficients_0_12_i) + 9 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -9 <= 0)%Z
    | 8%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 9%positive => (1 * (s IDCoefficients_0_12_i) + -8 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 11%positive => (1 * (s IDCoefficients_0_12_i) + -8 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 13%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 14%positive => (1 * (s IDCoefficients_0_12_i) + -8 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 16%positive => (1 * (s IDCoefficients_0_12_i) + -8 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 18%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 19%positive => (1 * (s IDCoefficients_0_12_i) + -8 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDCoefficients_0_12_i) + 1 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -8 <= 0)%Z
    | 21%positive => (-1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 2 <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -9 <= 0)%Z
    | 22%positive => (1 * (s IDCoefficients_0_12_i) + -9 <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 2 <= 0 /\ -1 * (s IDCoefficients_0_12_z) <= 0)%Z
    | 23%positive => (-1 * (s IDCoefficients_0_12_z) <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 2 <= 0 /\ 1 * (s IDCoefficients_0_12_i) + -9 <= 0)%Z
    | 24%positive => (1 * (s IDCoefficients_0_12_i) + -9 <= 0 /\ -1 * (s IDCoefficients_0_12_i) + 2 <= 0 /\ -1 * (s IDCoefficients_0_12_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Coefficients_0_12_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDCoefficients_0_12_z))%Q
    | 3%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 4%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 5%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 6%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 7%positive => ((s IDCoefficients_0_12_z))%Q
    | 8%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 9%positive => ((s IDCoefficients_0_12_z)
                     + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 10%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 11%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 12%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 13%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 14%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 15%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 16%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(8 - (s IDCoefficients_0_12_i)))%Q
    | 17%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(8 - (s IDCoefficients_0_12_i)))%Q
    | 18%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(8 - (s IDCoefficients_0_12_i)))%Q
    | 19%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(8 - (s IDCoefficients_0_12_i)))%Q
    | 20%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(8 - (s IDCoefficients_0_12_i)))%Q
    | 21%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 22%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 23%positive => ((1 # 1) + (s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | 24%positive => ((s IDCoefficients_0_12_z)
                      + max0(9 - (s IDCoefficients_0_12_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Coefficients_0_12_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (9
                                                            - (s IDCoefficients_0_12_i)) (8
                                                                    - (s IDCoefficients_0_12_i)));
                     (*-1 0*) F_max0_ge_0 (8 - (s IDCoefficients_0_12_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                     - (s IDCoefficients_0_12_i)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | _ => []
  end.


Theorem Coefficients_0_12_ai_correct:
  forall s p' s', steps (g_start Coefficients_0_12) s (g_edges Coefficients_0_12) p' s' -> Coefficients_0_12_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Coefficients_0_12_pot_correct:
  forall s p' s',
    steps (g_start Coefficients_0_12) s (g_edges Coefficients_0_12) p' s' ->
    (Coefficients_0_12_pot (g_start Coefficients_0_12) s >= Coefficients_0_12_pot p' s')%Q.
Proof.
  check_lp Coefficients_0_12_ai_correct Coefficients_0_12_hints.
Qed.

