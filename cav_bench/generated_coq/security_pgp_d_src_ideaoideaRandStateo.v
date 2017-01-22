Require Import pasta.Pasta.

Notation IDideaRandState_z := 1%positive.
Notation IDideaRandState_i := 2%positive.
Notation IDideaRandState_context := 3%positive.
Notation IDideaRandState_key := 4%positive.
Notation IDideaRandState_seed := 5%positive.
Definition ideaRandState : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDideaRandState_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDideaRandState_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDideaRandState_i)
             s) < (eval (ENum (8)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDideaRandState_i)
             s) >= (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDideaRandState_i
             (Some (EAdd (EVar IDideaRandState_i) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDideaRandState_z (Some (EAdd (ENum (1))
             (EVar IDideaRandState_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition ideaRandState_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaRandState_z) <= 0 /\ 1 * (s IDideaRandState_z) <= 0 /\ 1 * (s IDideaRandState_i) <= 0 /\ -1 * (s IDideaRandState_i) <= 0)%Z
    | 4%positive => (-1 * (s IDideaRandState_i) <= 0 /\ 1 * (s IDideaRandState_i) <= 0 /\ 1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_i) <= 0 /\ 1 * (s IDideaRandState_i) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDideaRandState_i) + -8 <= 0 /\ -1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_i) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDideaRandState_i) + 8 <= 0 /\ -1 * (s IDideaRandState_z) <= 0 /\ 1 * (s IDideaRandState_i) + -8 <= 0)%Z
    | 8%positive => (-1 * (s IDideaRandState_i) <= 0 /\ -1 * (s IDideaRandState_z) <= 0 /\ 1 * (s IDideaRandState_i) + -7 <= 0)%Z
    | 9%positive => (1 * (s IDideaRandState_i) + -7 <= 0 /\ -1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_i) <= 0)%Z
    | 10%positive => (-1 * (s IDideaRandState_i) <= 0 /\ -1 * (s IDideaRandState_z) <= 0 /\ 1 * (s IDideaRandState_i) + -7 <= 0)%Z
    | 11%positive => (-1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_i) + 1 <= 0 /\ 1 * (s IDideaRandState_i) + -8 <= 0)%Z
    | 12%positive => (1 * (s IDideaRandState_i) + -8 <= 0 /\ -1 * (s IDideaRandState_i) + 1 <= 0 /\ -1 * (s IDideaRandState_z) <= 0)%Z
    | 13%positive => (-1 * (s IDideaRandState_z) <= 0 /\ -1 * (s IDideaRandState_i) + 1 <= 0 /\ 1 * (s IDideaRandState_i) + -8 <= 0)%Z
    | 14%positive => (1 * (s IDideaRandState_i) + -8 <= 0 /\ -1 * (s IDideaRandState_i) + 1 <= 0 /\ -1 * (s IDideaRandState_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ideaRandState_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDideaRandState_z))%Q
    | 3%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | 4%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | 5%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | 6%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | 7%positive => ((s IDideaRandState_z))%Q
    | 8%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | 9%positive => ((1 # 1) + (s IDideaRandState_z)
                     + max0(7 - (s IDideaRandState_i)))%Q
    | 10%positive => ((1 # 1) + (s IDideaRandState_z)
                      + max0(7 - (s IDideaRandState_i)))%Q
    | 11%positive => ((1 # 1) + (s IDideaRandState_z)
                      + max0(8 - (s IDideaRandState_i)))%Q
    | 12%positive => ((1 # 1) + (s IDideaRandState_z)
                      + max0(8 - (s IDideaRandState_i)))%Q
    | 13%positive => ((1 # 1) + (s IDideaRandState_z)
                      + max0(8 - (s IDideaRandState_i)))%Q
    | 14%positive => ((s IDideaRandState_z) + max0(8 - (s IDideaRandState_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaRandState_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDideaRandState_i)) (7
                                                                    - (s IDideaRandState_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                - (s IDideaRandState_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (8 - (s IDideaRandState_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem ideaRandState_ai_correct:
  forall s p' s', steps (g_start ideaRandState) s (g_edges ideaRandState) p' s' -> ideaRandState_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaRandState_pot_correct:
  forall s p' s',
    steps (g_start ideaRandState) s (g_edges ideaRandState) p' s' ->
    (ideaRandState_pot (g_start ideaRandState) s >= ideaRandState_pot p' s')%Q.
Proof.
  check_lp ideaRandState_ai_correct ideaRandState_hints.
Qed.

