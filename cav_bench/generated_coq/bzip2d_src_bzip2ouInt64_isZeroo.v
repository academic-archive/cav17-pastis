Require Import pasta.Pasta.

Notation IDuInt64_isZero_z := 1%positive.
Notation IDuInt64_isZero__tmp := 2%positive.
Notation IDuInt64_isZero_i := 3%positive.
Notation IDuInt64_isZero_n := 4%positive.
Definition uInt64_isZero : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDuInt64_isZero_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDuInt64_isZero_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDuInt64_isZero_i)
             s) < (eval (ENum (8)) s))%Z)),10%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDuInt64_isZero_i)
             s) >= (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDuInt64_isZero__tmp (Some (ENum (1)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,21%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,18%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDuInt64_isZero_i
             (Some (EAdd (EVar IDuInt64_isZero_i) (ENum (1))))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDuInt64_isZero_z (Some (EAdd (ENum (1))
             (EVar IDuInt64_isZero_z)))),17%positive)::
             (17%positive,AWeaken,5%positive)::
             (18%positive,(AAssign IDuInt64_isZero__tmp (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::nil
|}.

Definition uInt64_isZero_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0)%Z
    | 3%positive => (-1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0)%Z
    | 4%positive => (-1 * (s IDuInt64_isZero_i) <= 0 /\ 1 * (s IDuInt64_isZero_i) <= 0 /\ 1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0)%Z
    | 5%positive => (-1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDuInt64_isZero_i) + 8 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) + 8 <= 0 /\ 1 * (s IDuInt64_isZero__tmp) + -1 <= 0 /\ -1 * (s IDuInt64_isZero__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDuInt64_isZero__tmp) + 1 <= 0 /\ 1 * (s IDuInt64_isZero__tmp) + -1 <= 0 /\ -1 * (s IDuInt64_isZero_i) + 8 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0)%Z
    | 10%positive => (-1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -7 <= 0)%Z
    | 11%positive => (1 * (s IDuInt64_isZero_i) + -7 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0)%Z
    | 12%positive => (-1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -7 <= 0)%Z
    | 13%positive => (1 * (s IDuInt64_isZero_i) + -7 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0)%Z
    | 14%positive => (-1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDuInt64_isZero_i) + 1 <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0)%Z
    | 16%positive => (-1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDuInt64_isZero_i) + 1 <= 0 /\ 1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ -1 * (s IDuInt64_isZero_z) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -7 <= 0)%Z
    | 19%positive => (1 * (s IDuInt64_isZero_i) + -7 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0 /\ 1 * (s IDuInt64_isZero__tmp) <= 0 /\ -1 * (s IDuInt64_isZero__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDuInt64_isZero__tmp) <= 0 /\ 1 * (s IDuInt64_isZero__tmp) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ 1 * (s IDuInt64_isZero_i) + -7 <= 0)%Z
    | 21%positive => (1 * (s IDuInt64_isZero_i) + -8 <= 0 /\ 1 * (s IDuInt64_isZero__tmp) + -1 <= 0 /\ -1 * (s IDuInt64_isZero_z) <= 0 /\ -1 * (s IDuInt64_isZero_i) <= 0 /\ -1 * (s IDuInt64_isZero__tmp) <= 0)%Z
    | _ => False
  end.

Definition uInt64_isZero_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDuInt64_isZero_z))%Q
    | 3%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 4%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 5%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 6%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 7%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 8%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 9%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 10%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 11%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 12%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 13%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 14%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 15%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 16%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 17%positive => ((s IDuInt64_isZero_z) + max0(8 - (s IDuInt64_isZero_i)))%Q
    | 18%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 19%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 20%positive => ((1 # 1) + (s IDuInt64_isZero_z)
                      + max0(7 - (s IDuInt64_isZero_i)))%Q
    | 21%positive => ((s IDuInt64_isZero_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition uInt64_isZero_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDuInt64_isZero_i)) (7
                                                                    - (s IDuInt64_isZero_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDuInt64_isZero_i))]
    | 10%positive => [(*-1 1e-12*) F_max0_pre_decrement (8
                                                         - (s IDuInt64_isZero_i)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1.14286 0*) F_max0_ge_0 (7 - (s IDuInt64_isZero_i));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuInt64_isZero_i))) (F_check_ge (0) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuInt64_isZero_i)) (0))) (F_max0_ge_0 ((s IDuInt64_isZero_i)));
                      (*0 0.142857*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDuInt64_isZero_i)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDuInt64_isZero_i)))]
    | 21%positive => []
    | _ => []
  end.


Theorem uInt64_isZero_ai_correct:
  forall s p' s', steps (g_start uInt64_isZero) s (g_edges uInt64_isZero) p' s' -> uInt64_isZero_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem uInt64_isZero_pot_correct:
  forall s p' s',
    steps (g_start uInt64_isZero) s (g_edges uInt64_isZero) p' s' ->
    (uInt64_isZero_pot (g_start uInt64_isZero) s >= uInt64_isZero_pot p' s')%Q.
Proof.
  check_lp uInt64_isZero_ai_correct uInt64_isZero_hints.
Qed.

