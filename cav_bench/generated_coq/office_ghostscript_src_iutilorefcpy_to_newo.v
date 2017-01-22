Require Import pasta.Pasta.

Notation IDrefcpy_to_new_z := 1%positive.
Notation IDrefcpy_to_new__tmp := 2%positive.
Notation IDrefcpy_to_new_from := 3%positive.
Notation IDrefcpy_to_new_size := 4%positive.
Notation IDrefcpy_to_new_to := 5%positive.
Definition refcpy_to_new : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDrefcpy_to_new_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDrefcpy_to_new__tmp
             (Some (EVar IDrefcpy_to_new_size))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDrefcpy_to_new__tmp
             (Some (EAdd (EVar IDrefcpy_to_new__tmp) (ENum (-1))))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDrefcpy_to_new__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDrefcpy_to_new__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDrefcpy_to_new_z (Some (EAdd (ENum (1))
             (EVar IDrefcpy_to_new_z)))),4%positive)::nil
|}.

Definition refcpy_to_new_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrefcpy_to_new_z) <= 0 /\ -1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrefcpy_to_new_z) <= 0 /\ 1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 4%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 5%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 6%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 7%positive => (-1 * (s IDrefcpy_to_new_z) <= 0 /\ 1 * (s IDrefcpy_to_new__tmp) <= 0 /\ -1 * (s IDrefcpy_to_new__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDrefcpy_to_new__tmp) <= 0 /\ 1 * (s IDrefcpy_to_new__tmp) <= 0 /\ -1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 9%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 10%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 11%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | 12%positive => (-1 * (s IDrefcpy_to_new_z) <= 0)%Z
    | _ => False
  end.

Definition refcpy_to_new_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDrefcpy_to_new_size))%Q
    | 2%positive => ((s IDrefcpy_to_new_size) + (s IDrefcpy_to_new_z))%Q
    | 3%positive => ((s IDrefcpy_to_new__tmp) + (s IDrefcpy_to_new_z))%Q
    | 4%positive => ((s IDrefcpy_to_new__tmp) + (s IDrefcpy_to_new_z))%Q
    | 5%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                     + (s IDrefcpy_to_new_z))%Q
    | 6%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                     + (s IDrefcpy_to_new_z))%Q
    | 7%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                     + (s IDrefcpy_to_new_z))%Q
    | 8%positive => ((s IDrefcpy_to_new_z))%Q
    | 9%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                     + (s IDrefcpy_to_new_z))%Q
    | 10%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                      + (s IDrefcpy_to_new_z))%Q
    | 11%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                      + (s IDrefcpy_to_new_z))%Q
    | 12%positive => ((1 # 1) + (s IDrefcpy_to_new__tmp)
                      + (s IDrefcpy_to_new_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition refcpy_to_new_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDrefcpy_to_new__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDrefcpy_to_new__tmp)) (0))) (F_max0_ge_0 ((s IDrefcpy_to_new__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem refcpy_to_new_ai_correct:
  forall s p' s', steps (g_start refcpy_to_new) s (g_edges refcpy_to_new) p' s' -> refcpy_to_new_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem refcpy_to_new_pot_correct:
  forall s p' s',
    steps (g_start refcpy_to_new) s (g_edges refcpy_to_new) p' s' ->
    (refcpy_to_new_pot (g_start refcpy_to_new) s >= refcpy_to_new_pot p' s')%Q.
Proof.
  check_lp refcpy_to_new_ai_correct refcpy_to_new_hints.
Qed.

