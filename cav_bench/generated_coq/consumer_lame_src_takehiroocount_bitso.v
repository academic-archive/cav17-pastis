Require Import pasta.Pasta.

Notation IDcount_bits_z := 1%positive.
Notation IDcount_bits__tmp := 2%positive.
Notation IDcount_bits_gfp_dref_off260 := 3%positive.
Notation IDcount_bits_i := 4%positive.
Notation IDcount_bits_cod_info := 5%positive.
Notation IDcount_bits_gfp := 6%positive.
Notation IDcount_bits_ix := 7%positive.
Notation IDcount_bits_xr := 8%positive.
Definition count_bits : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDcount_bits_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcount_bits_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDcount_bits_i) s) <
             (eval (ENum (576)) s))%Z)),20%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDcount_bits_i) s) >=
             (eval (ENum (576)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcount_bits_gfp_dref_off260) s) <>
             (eval (ENum (0)) s))%Z)),11%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcount_bits_gfp_dref_off260) s) =
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,14%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,16%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,17%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDcount_bits__tmp None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,31%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,28%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDcount_bits_i
             (Some (EAdd (EVar IDcount_bits_i) (ENum (1))))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDcount_bits_z (Some (EAdd (ENum (1))
             (EVar IDcount_bits_z)))),27%positive)::
             (27%positive,AWeaken,5%positive)::
             (28%positive,(AAssign IDcount_bits__tmp None),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::nil
|}.

Definition count_bits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) <= 0 /\ -1 * (s IDcount_bits_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcount_bits_i) <= 0 /\ 1 * (s IDcount_bits_i) <= 0 /\ 1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 6%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 7%positive => (-1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 8%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0 /\ 1 * (s IDcount_bits_gfp_dref_off260) <= 0 /\ -1 * (s IDcount_bits_gfp_dref_off260) <= 0)%Z
    | 9%positive => (-1 * (s IDcount_bits_gfp_dref_off260) <= 0 /\ 1 * (s IDcount_bits_gfp_dref_off260) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 10%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0 /\ 1 * (s IDcount_bits_gfp_dref_off260) <= 0 /\ -1 * (s IDcount_bits_gfp_dref_off260) <= 0)%Z
    | 11%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 12%positive => (-1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 13%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 14%positive => (-1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 15%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 16%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 17%positive => (-1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 18%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) + 576 <= 0)%Z
    | 19%positive => (-1 * (s IDcount_bits_i) + 576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0)%Z
    | 20%positive => (-1 * (s IDcount_bits_i) <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -575 <= 0)%Z
    | 21%positive => (1 * (s IDcount_bits_i) + -575 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) <= 0)%Z
    | 22%positive => (-1 * (s IDcount_bits_i) <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -575 <= 0)%Z
    | 23%positive => (1 * (s IDcount_bits_i) + -575 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) <= 0)%Z
    | 24%positive => (-1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDcount_bits_i) + 1 <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0)%Z
    | 26%positive => (-1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDcount_bits_i) + 1 <= 0 /\ 1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDcount_bits_i) <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -575 <= 0)%Z
    | 29%positive => (1 * (s IDcount_bits_i) + -575 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) <= 0)%Z
    | 30%positive => (-1 * (s IDcount_bits_i) <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ 1 * (s IDcount_bits_i) + -575 <= 0)%Z
    | 31%positive => (1 * (s IDcount_bits_i) + -576 <= 0 /\ -1 * (s IDcount_bits_z) <= 0 /\ -1 * (s IDcount_bits_i) <= 0)%Z
    | _ => False
  end.

Definition count_bits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((576 # 1))%Q
    | 2%positive => ((576 # 1) + (s IDcount_bits_z))%Q
    | 3%positive => ((s IDcount_bits_z) + max0(576 - (s IDcount_bits_i)))%Q
    | 4%positive => ((s IDcount_bits_z) + max0(576 - (s IDcount_bits_i)))%Q
    | 5%positive => ((s IDcount_bits_z) + max0(576 - (s IDcount_bits_i)))%Q
    | 6%positive => ((s IDcount_bits_z) + max0(576 - (s IDcount_bits_i)))%Q
    | 7%positive => ((s IDcount_bits_z))%Q
    | 8%positive => ((s IDcount_bits_z))%Q
    | 9%positive => ((s IDcount_bits_z))%Q
    | 10%positive => ((s IDcount_bits_z))%Q
    | 11%positive => ((s IDcount_bits_z))%Q
    | 12%positive => ((s IDcount_bits_z))%Q
    | 13%positive => ((s IDcount_bits_z))%Q
    | 14%positive => ((s IDcount_bits_z))%Q
    | 15%positive => ((s IDcount_bits_z))%Q
    | 16%positive => ((s IDcount_bits_z))%Q
    | 17%positive => ((s IDcount_bits_z))%Q
    | 18%positive => ((s IDcount_bits_z))%Q
    | 19%positive => ((s IDcount_bits_z))%Q
    | 20%positive => ((s IDcount_bits_z) + max0(576 - (s IDcount_bits_i)))%Q
    | 21%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 22%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 23%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 24%positive => (-(0 # 1) + (0 # 1) * (s IDcount_bits_i)
                      + (s IDcount_bits_z)
                      + (1 # 1) * max0(576 - (s IDcount_bits_i)))%Q
    | 25%positive => (-(0 # 1) + (0 # 1) * (s IDcount_bits_i)
                      + (s IDcount_bits_z)
                      + (1 # 1) * max0(576 - (s IDcount_bits_i)))%Q
    | 26%positive => (-(0 # 1) + (0 # 1) * (s IDcount_bits_i)
                      + (s IDcount_bits_z)
                      + (1 # 1) * max0(576 - (s IDcount_bits_i)))%Q
    | 27%positive => (-(1 # 1) + (0 # 1) * (s IDcount_bits_i)
                      + (s IDcount_bits_z)
                      + (1 # 1) * max0(576 - (s IDcount_bits_i)))%Q
    | 28%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 29%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 30%positive => ((0 # 1) * (s IDcount_bits_i) + (s IDcount_bits_z)
                      + (1 # 1) * max0(575 - (s IDcount_bits_i)))%Q
    | 31%positive => ((s IDcount_bits_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition count_bits_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (576
                                                               - (s IDcount_bits_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (576 - (s IDcount_bits_i)) (1);
                      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - (s IDcount_bits_i)) (0))) (F_max0_ge_0 (575
                                                                    - (s IDcount_bits_i)))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-0.00173913 0*) F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - (s IDcount_bits_i))) (F_check_ge (576
                                                                    - (s IDcount_bits_i)) (0))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1.00174 0*) F_max0_ge_0 (575 - (s IDcount_bits_i));
                      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcount_bits_i))) (F_check_ge (0) (0));
                      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcount_bits_i)) (0))) (F_max0_ge_0 ((s IDcount_bits_i)))]
    | 31%positive => []
    | _ => []
  end.


Theorem count_bits_ai_correct:
  forall s p' s', steps (g_start count_bits) s (g_edges count_bits) p' s' -> count_bits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem count_bits_pot_correct:
  forall s p' s',
    steps (g_start count_bits) s (g_edges count_bits) p' s' ->
    (count_bits_pot (g_start count_bits) s >= count_bits_pot p' s')%Q.
Proof.
  check_lp count_bits_ai_correct count_bits_hints.
Qed.

