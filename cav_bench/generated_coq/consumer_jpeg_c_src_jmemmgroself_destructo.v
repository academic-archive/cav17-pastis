Require Import pasta.Pasta.

Notation IDself_destruct_z := 1%positive.
Notation IDself_destruct_pool := 2%positive.
Notation IDself_destruct_cinfo := 3%positive.
Definition self_destruct : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDself_destruct_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDself_destruct_pool (Some (ENum (1)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDself_destruct_pool)
             s) >= (eval (ENum (0)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDself_destruct_pool)
             s) < (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDself_destruct_pool
             (Some (EAdd (EVar IDself_destruct_pool) (ENum (-1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDself_destruct_z (Some (EAdd (ENum (1))
             (EVar IDself_destruct_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition self_destruct_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDself_destruct_z) <= 0 /\ -1 * (s IDself_destruct_z) <= 0)%Z
    | 3%positive => (-1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) + -1 <= 0 /\ -1 * (s IDself_destruct_pool) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDself_destruct_pool) + 1 <= 0 /\ 1 * (s IDself_destruct_pool) + -1 <= 0 /\ 1 * (s IDself_destruct_z) <= 0 /\ -1 * (s IDself_destruct_z) <= 0)%Z
    | 5%positive => (-1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) + -1 <= 0 /\ -1 * (s IDself_destruct_pool) + -1 <= 0)%Z
    | 6%positive => (-1 * (s IDself_destruct_pool) + -1 <= 0 /\ -1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDself_destruct_pool) + 1 <= 0 /\ -1 * (s IDself_destruct_z) <= 0 /\ -1 * (s IDself_destruct_pool) + -1 <= 0)%Z
    | 8%positive => (1 * (s IDself_destruct_pool) + -1 <= 0 /\ -1 * (s IDself_destruct_z) <= 0 /\ -1 * (s IDself_destruct_pool) <= 0)%Z
    | 9%positive => (-1 * (s IDself_destruct_pool) <= 0 /\ -1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) + -1 <= 0)%Z
    | 10%positive => (1 * (s IDself_destruct_pool) + -1 <= 0 /\ -1 * (s IDself_destruct_z) <= 0 /\ -1 * (s IDself_destruct_pool) <= 0)%Z
    | 11%positive => (-1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) <= 0 /\ -1 * (s IDself_destruct_pool) + -1 <= 0)%Z
    | 12%positive => (-1 * (s IDself_destruct_pool) + -1 <= 0 /\ 1 * (s IDself_destruct_pool) <= 0 /\ -1 * (s IDself_destruct_z) <= 0)%Z
    | 13%positive => (-1 * (s IDself_destruct_z) <= 0 /\ 1 * (s IDself_destruct_pool) <= 0 /\ -1 * (s IDself_destruct_pool) + -1 <= 0)%Z
    | 14%positive => (-1 * (s IDself_destruct_pool) + -1 <= 0 /\ 1 * (s IDself_destruct_pool) <= 0 /\ -1 * (s IDself_destruct_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition self_destruct_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1))%Q
    | 2%positive => ((2 # 1) + (s IDself_destruct_z))%Q
    | 3%positive => ((s IDself_destruct_z)
                     + max0(1 + (s IDself_destruct_pool)))%Q
    | 4%positive => ((s IDself_destruct_z)
                     + max0(1 + (s IDself_destruct_pool)))%Q
    | 5%positive => ((s IDself_destruct_z)
                     + max0(1 + (s IDself_destruct_pool)))%Q
    | 6%positive => ((s IDself_destruct_z)
                     + max0(1 + (s IDself_destruct_pool)))%Q
    | 7%positive => ((s IDself_destruct_z))%Q
    | 8%positive => ((s IDself_destruct_z)
                     + max0(1 + (s IDself_destruct_pool)))%Q
    | 9%positive => ((1 # 1) + (s IDself_destruct_z)
                     + max0((s IDself_destruct_pool)))%Q
    | 10%positive => ((1 # 1) + (s IDself_destruct_z)
                      + max0((s IDself_destruct_pool)))%Q
    | 11%positive => ((1 # 1) + (s IDself_destruct_z)
                      + max0(1 + (s IDself_destruct_pool)))%Q
    | 12%positive => ((1 # 1) + (s IDself_destruct_z)
                      + max0(1 + (s IDself_destruct_pool)))%Q
    | 13%positive => ((1 # 1) + (s IDself_destruct_z)
                      + max0(1 + (s IDself_destruct_pool)))%Q
    | 14%positive => ((s IDself_destruct_z)
                      + max0(1 + (s IDself_destruct_pool)))%Q
    | _ => (0 # 1)%Q
  end.

Definition self_destruct_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDself_destruct_pool)) ((s IDself_destruct_pool)));
                     (*-1 0*) F_max0_ge_0 ((s IDself_destruct_pool))]
    | 7%positive => []
    | 8%positive => [(*0 1*) F_max0_pre_decrement (1
                                                   + (s IDself_destruct_pool)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem self_destruct_ai_correct:
  forall s p' s', steps (g_start self_destruct) s (g_edges self_destruct) p' s' -> self_destruct_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem self_destruct_pot_correct:
  forall s p' s',
    steps (g_start self_destruct) s (g_edges self_destruct) p' s' ->
    (self_destruct_pot (g_start self_destruct) s >= self_destruct_pot p' s')%Q.
Proof.
  check_lp self_destruct_ai_correct self_destruct_hints.
Qed.

