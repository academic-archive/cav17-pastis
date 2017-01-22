Require Import pasta.Pasta.

Notation IDwblong_z := 1%positive.
Notation IDwblong__tmp := 2%positive.
Notation IDwblong__tmp1 := 3%positive.
Notation IDwblong_i := 4%positive.
Notation IDwblong_fd := 5%positive.
Notation IDwblong_x := 6%positive.
Definition wblong : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDwblong_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDwblong__tmp1 (Some (EVar IDwblong_fd))),
             3%positive)::
             (3%positive,(AAssign IDwblong__tmp (Some (EVar IDwblong_x))),
             4%positive)::
             (4%positive,(AAssign IDwblong_i (Some (ENum (24)))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDwblong_i) s) >=
             (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDwblong_i) s) <
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDwblong_i (Some (ESub (EVar IDwblong_i)
             (ENum (8))))),13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDwblong_z (Some (EAdd (ENum (1))
             (EVar IDwblong_z)))),16%positive)::
             (16%positive,AWeaken,7%positive)::nil
|}.

Definition wblong_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_z) <= 0)%Z
    | 3%positive => (-1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_z) <= 0)%Z
    | 4%positive => (1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_z) <= 0)%Z
    | 5%positive => (-1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + -24 <= 0 /\ -1 * (s IDwblong_i) + 24 <= 0)%Z
    | 6%positive => (-1 * (s IDwblong_i) + 24 <= 0 /\ 1 * (s IDwblong_i) + -24 <= 0 /\ 1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_z) <= 0)%Z
    | 7%positive => (-1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + -24 <= 0 /\ -1 * (s IDwblong_i) + -8 <= 0)%Z
    | 8%positive => (-1 * (s IDwblong_i) + -8 <= 0 /\ -1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + 1 <= 0)%Z
    | 9%positive => (1 * (s IDwblong_i) + 1 <= 0 /\ -1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_i) + -8 <= 0)%Z
    | 10%positive => (1 * (s IDwblong_i) + -24 <= 0 /\ -1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_i) <= 0)%Z
    | 11%positive => (-1 * (s IDwblong_i) <= 0 /\ -1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + -24 <= 0)%Z
    | 12%positive => (1 * (s IDwblong_i) + -24 <= 0 /\ -1 * (s IDwblong_z) <= 0 /\ -1 * (s IDwblong_i) <= 0)%Z
    | 13%positive => (-1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + -16 <= 0 /\ -1 * (s IDwblong_i) + -8 <= 0)%Z
    | 14%positive => (-1 * (s IDwblong_i) + -8 <= 0 /\ 1 * (s IDwblong_i) + -16 <= 0 /\ -1 * (s IDwblong_z) <= 0)%Z
    | 15%positive => (-1 * (s IDwblong_z) <= 0 /\ 1 * (s IDwblong_i) + -16 <= 0 /\ -1 * (s IDwblong_i) + -8 <= 0)%Z
    | 16%positive => (-1 * (s IDwblong_i) + -8 <= 0 /\ 1 * (s IDwblong_i) + -16 <= 0 /\ -1 * (s IDwblong_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition wblong_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDwblong_z))%Q
    | 3%positive => ((4 # 1) + (s IDwblong_z))%Q
    | 4%positive => ((4 # 1) + (s IDwblong_z))%Q
    | 5%positive => ((s IDwblong_z) + (1 # 8) * max0(8 + (s IDwblong_i)))%Q
    | 6%positive => ((s IDwblong_z) + (1 # 8) * max0(8 + (s IDwblong_i)))%Q
    | 7%positive => ((s IDwblong_z) + (1 # 8) * max0(8 + (s IDwblong_i)))%Q
    | 8%positive => ((s IDwblong_z) + (1 # 8) * max0(8 + (s IDwblong_i)))%Q
    | 9%positive => ((s IDwblong_z))%Q
    | 10%positive => ((s IDwblong_z) + (1 # 8) * max0(8 + (s IDwblong_i)))%Q
    | 11%positive => ((1 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | 12%positive => ((1 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | 13%positive => ((2 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | 14%positive => ((2 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | 15%positive => ((2 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | 16%positive => ((1 # 1) + (1 # 8) * (s IDwblong_i) + (s IDwblong_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition wblong_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge (8
                                                                + (s IDwblong_i)) ((s IDwblong_i)));
                     (*-0.125 0*) F_max0_ge_0 ((s IDwblong_i))]
    | 9%positive => []
    | 10%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    + (s IDwblong_i))) (F_check_ge (8
                                                                    + (s IDwblong_i)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    + (s IDwblong_i)) (0))) (F_max0_ge_0 (8
                                                                    + (s IDwblong_i)))]
    | _ => []
  end.


Theorem wblong_ai_correct:
  forall s p' s', steps (g_start wblong) s (g_edges wblong) p' s' -> wblong_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem wblong_pot_correct:
  forall s p' s',
    steps (g_start wblong) s (g_edges wblong) p' s' ->
    (wblong_pot (g_start wblong) s >= wblong_pot p' s')%Q.
Proof.
  check_lp wblong_ai_correct wblong_hints.
Qed.

