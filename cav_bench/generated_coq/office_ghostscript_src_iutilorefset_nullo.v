Require Import pasta.Pasta.

Notation IDrefset_null_z := 1%positive.
Notation IDrefset_null__tmp := 2%positive.
Notation IDrefset_null_size := 3%positive.
Notation IDrefset_null_to := 4%positive.
Definition refset_null : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDrefset_null_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDrefset_null__tmp
             (Some (EVar IDrefset_null_size))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDrefset_null__tmp
             (Some (EAdd (EVar IDrefset_null__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDrefset_null__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDrefset_null__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDrefset_null_z (Some (EAdd (ENum (1))
             (EVar IDrefset_null_z)))),4%positive)::nil
|}.

Definition refset_null_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrefset_null_z) <= 0 /\ -1 * (s IDrefset_null_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrefset_null_z) <= 0 /\ 1 * (s IDrefset_null_z) <= 0)%Z
    | 4%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 5%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 6%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 7%positive => (-1 * (s IDrefset_null_z) <= 0 /\ 1 * (s IDrefset_null__tmp) <= 0 /\ -1 * (s IDrefset_null__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDrefset_null__tmp) <= 0 /\ 1 * (s IDrefset_null__tmp) <= 0 /\ -1 * (s IDrefset_null_z) <= 0)%Z
    | 9%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 10%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 11%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | 12%positive => (-1 * (s IDrefset_null_z) <= 0)%Z
    | _ => False
  end.

Definition refset_null_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDrefset_null_size))%Q
    | 2%positive => ((s IDrefset_null_size) + (s IDrefset_null_z))%Q
    | 3%positive => ((s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 4%positive => ((s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 5%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 6%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 7%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 8%positive => ((s IDrefset_null_z))%Q
    | 9%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 10%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 11%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | 12%positive => ((1 # 1) + (s IDrefset_null__tmp) + (s IDrefset_null_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition refset_null_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDrefset_null__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDrefset_null__tmp)) (0))) (F_max0_ge_0 ((s IDrefset_null__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem refset_null_ai_correct:
  forall s p' s', steps (g_start refset_null) s (g_edges refset_null) p' s' -> refset_null_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem refset_null_pot_correct:
  forall s p' s',
    steps (g_start refset_null) s (g_edges refset_null) p' s' ->
    (refset_null_pot (g_start refset_null) s >= refset_null_pot p' s')%Q.
Proof.
  check_lp refset_null_ai_correct refset_null_hints.
Qed.

