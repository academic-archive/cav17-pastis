Require Import pasta.Pasta.

Notation IDtransencode_coef_controller_z := 1%positive.
Notation IDtransencode_coef_controller_i := 2%positive.
Notation IDtransencode_coef_controller_cinfo := 3%positive.
Notation IDtransencode_coef_controller_coef_arrays := 4%positive.
Definition transencode_coef_controller : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDtransencode_coef_controller_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDtransencode_coef_controller_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDtransencode_coef_controller_i) s) <
             (eval (ENum (10)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDtransencode_coef_controller_i) s) >=
             (eval (ENum (10)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDtransencode_coef_controller_i
             (Some (EAdd (EVar IDtransencode_coef_controller_i)
             (ENum (1))))),11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDtransencode_coef_controller_z
             (Some (EAdd (ENum (1))
             (EVar IDtransencode_coef_controller_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition transencode_coef_controller_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0)%Z
    | 3%positive => (-1 * (s IDtransencode_coef_controller_z) <= 0 /\ 1 * (s IDtransencode_coef_controller_z) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) <= 0)%Z
    | 4%positive => (-1 * (s IDtransencode_coef_controller_i) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) <= 0 /\ 1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0)%Z
    | 5%positive => (-1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -10 <= 0)%Z
    | 6%positive => (1 * (s IDtransencode_coef_controller_i) + -10 <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) + 10 <= 0)%Z
    | 7%positive => (-1 * (s IDtransencode_coef_controller_i) + 10 <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -10 <= 0)%Z
    | 8%positive => (-1 * (s IDtransencode_coef_controller_i) <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -9 <= 0)%Z
    | 9%positive => (1 * (s IDtransencode_coef_controller_i) + -9 <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) <= 0)%Z
    | 10%positive => (-1 * (s IDtransencode_coef_controller_i) <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -9 <= 0)%Z
    | 11%positive => (-1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) + 1 <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -10 <= 0)%Z
    | 12%positive => (1 * (s IDtransencode_coef_controller_i) + -10 <= 0 /\ -1 * (s IDtransencode_coef_controller_i) + 1 <= 0 /\ -1 * (s IDtransencode_coef_controller_z) <= 0)%Z
    | 13%positive => (-1 * (s IDtransencode_coef_controller_z) <= 0 /\ -1 * (s IDtransencode_coef_controller_i) + 1 <= 0 /\ 1 * (s IDtransencode_coef_controller_i) + -10 <= 0)%Z
    | 14%positive => (1 * (s IDtransencode_coef_controller_i) + -10 <= 0 /\ -1 * (s IDtransencode_coef_controller_i) + 1 <= 0 /\ -1 * (s IDtransencode_coef_controller_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition transencode_coef_controller_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((10 # 1))%Q
    | 2%positive => ((10 # 1) + (s IDtransencode_coef_controller_z))%Q
    | 3%positive => ((s IDtransencode_coef_controller_z)
                     + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 4%positive => ((s IDtransencode_coef_controller_z)
                     + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 5%positive => ((s IDtransencode_coef_controller_z)
                     + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 6%positive => ((s IDtransencode_coef_controller_z)
                     + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 7%positive => ((s IDtransencode_coef_controller_z))%Q
    | 8%positive => ((s IDtransencode_coef_controller_z)
                     + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 9%positive => ((1 # 1) + (s IDtransencode_coef_controller_z)
                     + max0(9 - (s IDtransencode_coef_controller_i)))%Q
    | 10%positive => ((1 # 1) + (s IDtransencode_coef_controller_z)
                      + max0(9 - (s IDtransencode_coef_controller_i)))%Q
    | 11%positive => ((1 # 1) + (s IDtransencode_coef_controller_z)
                      + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 12%positive => ((1 # 1) + (s IDtransencode_coef_controller_z)
                      + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 13%positive => ((1 # 1) + (s IDtransencode_coef_controller_z)
                      + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | 14%positive => ((s IDtransencode_coef_controller_z)
                      + max0(10 - (s IDtransencode_coef_controller_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition transencode_coef_controller_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (10
                                                            - (s IDtransencode_coef_controller_i)) (9
                                                                    - (s IDtransencode_coef_controller_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                - (s IDtransencode_coef_controller_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (10
                                                    - (s IDtransencode_coef_controller_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem transencode_coef_controller_ai_correct:
  forall s p' s', steps (g_start transencode_coef_controller) s (g_edges transencode_coef_controller) p' s' -> transencode_coef_controller_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem transencode_coef_controller_pot_correct:
  forall s p' s',
    steps (g_start transencode_coef_controller) s (g_edges transencode_coef_controller) p' s' ->
    (transencode_coef_controller_pot (g_start transencode_coef_controller) s >= transencode_coef_controller_pot p' s')%Q.
Proof.
  check_lp transencode_coef_controller_ai_correct transencode_coef_controller_hints.
Qed.

