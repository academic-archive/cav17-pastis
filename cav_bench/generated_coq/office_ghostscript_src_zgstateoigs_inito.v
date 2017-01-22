Require Import pasta.Pasta.

Notation IDigs_init_z := 1%positive.
Notation IDigs_init_i := 2%positive.
Definition igs_init : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDigs_init_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDigs_init_i (Some (ENum (25)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDigs_init_i
             (Some (EAdd (EVar IDigs_init_i) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EAdd (EVar IDigs_init_i)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EAdd (EVar IDigs_init_i)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDigs_init_z (Some (EAdd (ENum (1))
             (EVar IDigs_init_z)))),4%positive)::nil
|}.

Definition igs_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDigs_init_z) <= 0 /\ -1 * (s IDigs_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -25 <= 0 /\ -1 * (s IDigs_init_i) + 25 <= 0)%Z
    | 4%positive => (1 * (s IDigs_init_i) + -25 <= 0 /\ -1 * (s IDigs_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -25 <= 0)%Z
    | 6%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -24 <= 0)%Z
    | 7%positive => (1 * (s IDigs_init_i) + -24 <= 0 /\ -1 * (s IDigs_init_z) <= 0)%Z
    | 8%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -1 <= 0 /\ -1 * (s IDigs_init_i) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDigs_init_i) + 1 <= 0 /\ 1 * (s IDigs_init_i) + -1 <= 0 /\ -1 * (s IDigs_init_z) <= 0)%Z
    | 10%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -24 <= 0)%Z
    | 11%positive => (1 * (s IDigs_init_i) + -24 <= 0 /\ -1 * (s IDigs_init_z) <= 0)%Z
    | 12%positive => (-1 * (s IDigs_init_z) <= 0 /\ 1 * (s IDigs_init_i) + -24 <= 0)%Z
    | _ => False
  end.

Definition igs_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((23 # 1))%Q
    | 2%positive => ((23 # 1) + (s IDigs_init_z))%Q
    | 3%positive => (-(2 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 4%positive => (-(2 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 5%positive => (-(2 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 6%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 7%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 8%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 9%positive => ((s IDigs_init_z))%Q
    | 10%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 11%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | 12%positive => (-(1 # 1) + (s IDigs_init_i) + (s IDigs_init_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition igs_init_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDigs_init_i))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDigs_init_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDigs_init_i)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem igs_init_ai_correct:
  forall s p' s', steps (g_start igs_init) s (g_edges igs_init) p' s' -> igs_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem igs_init_pot_correct:
  forall s p' s',
    steps (g_start igs_init) s (g_edges igs_init) p' s' ->
    (igs_init_pot (g_start igs_init) s >= igs_init_pot p' s')%Q.
Proof.
  check_lp igs_init_ai_correct igs_init_hints.
Qed.

