Require Import pasta.Pasta.

Notation IDmdct_long_z := 1%positive.
Notation IDmdct_long_j := 2%positive.
Notation IDmdct_long_in := 3%positive.
Notation IDmdct_long_out := 4%positive.
Definition mdct_long : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDmdct_long_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmdct_long_j (Some (ENum (11)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDmdct_long_j
             (Some (EAdd (EVar IDmdct_long_j) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmdct_long_j)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmdct_long_j)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDmdct_long_z (Some (EAdd (ENum (1))
             (EVar IDmdct_long_z)))),4%positive)::nil
|}.

Definition mdct_long_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmdct_long_z) <= 0 /\ -1 * (s IDmdct_long_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmdct_long_z) <= 0 /\ 1 * (s IDmdct_long_z) <= 0 /\ 1 * (s IDmdct_long_j) + -11 <= 0 /\ -1 * (s IDmdct_long_j) + 11 <= 0)%Z
    | 4%positive => (1 * (s IDmdct_long_j) + -11 <= 0 /\ -1 * (s IDmdct_long_z) <= 0 /\ -1 * (s IDmdct_long_j) + 1 <= 0)%Z
    | 5%positive => (-1 * (s IDmdct_long_j) + 1 <= 0 /\ -1 * (s IDmdct_long_z) <= 0 /\ 1 * (s IDmdct_long_j) + -11 <= 0)%Z
    | 6%positive => (-1 * (s IDmdct_long_z) <= 0 /\ -1 * (s IDmdct_long_j) <= 0 /\ 1 * (s IDmdct_long_j) + -10 <= 0)%Z
    | 7%positive => (1 * (s IDmdct_long_j) + -10 <= 0 /\ -1 * (s IDmdct_long_j) <= 0 /\ -1 * (s IDmdct_long_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmdct_long_z) <= 0 /\ -1 * (s IDmdct_long_j) <= 0 /\ 1 * (s IDmdct_long_j) <= 0)%Z
    | 9%positive => (1 * (s IDmdct_long_j) <= 0 /\ -1 * (s IDmdct_long_j) <= 0 /\ -1 * (s IDmdct_long_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmdct_long_z) <= 0 /\ 1 * (s IDmdct_long_j) + -10 <= 0 /\ -1 * (s IDmdct_long_j) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDmdct_long_j) + 1 <= 0 /\ 1 * (s IDmdct_long_j) + -10 <= 0 /\ -1 * (s IDmdct_long_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmdct_long_z) <= 0 /\ 1 * (s IDmdct_long_j) + -10 <= 0 /\ -1 * (s IDmdct_long_j) + 1 <= 0)%Z
    | _ => False
  end.

Definition mdct_long_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((10 # 1))%Q
    | 2%positive => ((10 # 1) + (s IDmdct_long_z))%Q
    | 3%positive => (-(1 # 1) + (s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 4%positive => (-(1 # 1) + (s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 5%positive => (-(1 # 1) + (s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 6%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 7%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 8%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 9%positive => ((s IDmdct_long_z))%Q
    | 10%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 11%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | 12%positive => ((s IDmdct_long_j) + (s IDmdct_long_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mdct_long_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDmdct_long_j)) (-1
                                                                    + (s IDmdct_long_j)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDmdct_long_j));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmdct_long_j)) (0))) (F_max0_ge_0 ((s IDmdct_long_j)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem mdct_long_ai_correct:
  forall s p' s', steps (g_start mdct_long) s (g_edges mdct_long) p' s' -> mdct_long_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mdct_long_pot_correct:
  forall s p' s',
    steps (g_start mdct_long) s (g_edges mdct_long) p' s' ->
    (mdct_long_pot (g_start mdct_long) s >= mdct_long_pot p' s')%Q.
Proof.
  check_lp mdct_long_ai_correct mdct_long_hints.
Qed.

