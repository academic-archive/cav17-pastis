Require Import pasta.Pasta.

Notation IDkeyIDstring_z := 1%positive.
Notation IDkeyIDstring_i := 2%positive.
Notation IDkeyIDstring_keyID := 3%positive.
Definition keyIDstring : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDkeyIDstring_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDkeyIDstring_i (Some (ENum (4)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDkeyIDstring_i) s) <
             (eval (ENum (8)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDkeyIDstring_i)
             s) >= (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDkeyIDstring_i
             (Some (EAdd (EVar IDkeyIDstring_i) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDkeyIDstring_z (Some (EAdd (ENum (1))
             (EVar IDkeyIDstring_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition keyIDstring_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0)%Z
    | 3%positive => (-1 * (s IDkeyIDstring_z) <= 0 /\ 1 * (s IDkeyIDstring_z) <= 0 /\ 1 * (s IDkeyIDstring_i) + -4 <= 0 /\ -1 * (s IDkeyIDstring_i) + 4 <= 0)%Z
    | 4%positive => (-1 * (s IDkeyIDstring_i) + 4 <= 0 /\ 1 * (s IDkeyIDstring_i) + -4 <= 0 /\ 1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0)%Z
    | 5%positive => (-1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_i) + 4 <= 0 /\ 1 * (s IDkeyIDstring_i) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDkeyIDstring_i) + -8 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_i) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDkeyIDstring_i) + 8 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0 /\ 1 * (s IDkeyIDstring_i) + -8 <= 0)%Z
    | 8%positive => (-1 * (s IDkeyIDstring_i) + 4 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0 /\ 1 * (s IDkeyIDstring_i) + -7 <= 0)%Z
    | 9%positive => (1 * (s IDkeyIDstring_i) + -7 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_i) + 4 <= 0)%Z
    | 10%positive => (-1 * (s IDkeyIDstring_i) + 4 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0 /\ 1 * (s IDkeyIDstring_i) + -7 <= 0)%Z
    | 11%positive => (-1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_i) + 5 <= 0 /\ 1 * (s IDkeyIDstring_i) + -8 <= 0)%Z
    | 12%positive => (1 * (s IDkeyIDstring_i) + -8 <= 0 /\ -1 * (s IDkeyIDstring_i) + 5 <= 0 /\ -1 * (s IDkeyIDstring_z) <= 0)%Z
    | 13%positive => (-1 * (s IDkeyIDstring_z) <= 0 /\ -1 * (s IDkeyIDstring_i) + 5 <= 0 /\ 1 * (s IDkeyIDstring_i) + -8 <= 0)%Z
    | 14%positive => (1 * (s IDkeyIDstring_i) + -8 <= 0 /\ -1 * (s IDkeyIDstring_i) + 5 <= 0 /\ -1 * (s IDkeyIDstring_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition keyIDstring_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDkeyIDstring_z))%Q
    | 3%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | 4%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | 5%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | 6%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | 7%positive => ((s IDkeyIDstring_z))%Q
    | 8%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | 9%positive => ((1 # 1) + (s IDkeyIDstring_z)
                     + max0(7 - (s IDkeyIDstring_i)))%Q
    | 10%positive => ((1 # 1) + (s IDkeyIDstring_z)
                      + max0(7 - (s IDkeyIDstring_i)))%Q
    | 11%positive => ((1 # 1) + (s IDkeyIDstring_z)
                      + max0(8 - (s IDkeyIDstring_i)))%Q
    | 12%positive => ((1 # 1) + (s IDkeyIDstring_z)
                      + max0(8 - (s IDkeyIDstring_i)))%Q
    | 13%positive => ((1 # 1) + (s IDkeyIDstring_z)
                      + max0(8 - (s IDkeyIDstring_i)))%Q
    | 14%positive => ((s IDkeyIDstring_z) + max0(8 - (s IDkeyIDstring_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition keyIDstring_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDkeyIDstring_i)) (7
                                                                    - (s IDkeyIDstring_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDkeyIDstring_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (8 - (s IDkeyIDstring_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem keyIDstring_ai_correct:
  forall s p' s', steps (g_start keyIDstring) s (g_edges keyIDstring) p' s' -> keyIDstring_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem keyIDstring_pot_correct:
  forall s p' s',
    steps (g_start keyIDstring) s (g_edges keyIDstring) p' s' ->
    (keyIDstring_pot (g_start keyIDstring) s >= keyIDstring_pot p' s')%Q.
Proof.
  check_lp keyIDstring_ai_correct keyIDstring_hints.
Qed.

