Require Import pasta.Pasta.

Notation IDmp_rotate_left_z := 1%positive.
Notation IDmp_rotate_left__tmp := 2%positive.
Notation IDmp_rotate_left_global_precision := 3%positive.
Notation IDmp_rotate_left_mcarry := 4%positive.
Notation IDmp_rotate_left_nextcarry := 5%positive.
Notation IDmp_rotate_left_precision := 6%positive.
Notation IDmp_rotate_left_carry := 7%positive.
Notation IDmp_rotate_left_r1 := 8%positive.
Definition mp_rotate_left : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDmp_rotate_left_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmp_rotate_left__tmp
             (Some (EVar IDmp_rotate_left_carry))),3%positive)::
             (3%positive,(AAssign IDmp_rotate_left_mcarry
             (Some (EVar IDmp_rotate_left__tmp))),4%positive)::
             (4%positive,(AAssign IDmp_rotate_left_precision
             (Some (EVar IDmp_rotate_left_global_precision))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDmp_rotate_left_precision
             (Some (EAdd (EVar IDmp_rotate_left_precision) (ENum (-1))))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDmp_rotate_left_precision) s) <>
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDmp_rotate_left_precision) s) =
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDmp_rotate_left_nextcarry None),
             13%positive)::
             (13%positive,(AAssign IDmp_rotate_left_mcarry
             (Some (EVar IDmp_rotate_left_nextcarry))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDmp_rotate_left_z (Some (EAdd (ENum (1))
             (EVar IDmp_rotate_left_z)))),6%positive)::nil
|}.

Definition mp_rotate_left_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmp_rotate_left_z) <= 0 /\ -1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmp_rotate_left_z) <= 0 /\ 1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 4%positive => (1 * (s IDmp_rotate_left_z) <= 0 /\ -1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmp_rotate_left_z) <= 0 /\ 1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmp_rotate_left_z) <= 0 /\ 1 * (s IDmp_rotate_left_precision) <= 0 /\ -1 * (s IDmp_rotate_left_precision) <= 0)%Z
    | 10%positive => (-1 * (s IDmp_rotate_left_precision) <= 0 /\ 1 * (s IDmp_rotate_left_precision) <= 0 /\ -1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 14%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 15%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmp_rotate_left_z) <= 0)%Z
    | _ => False
  end.

Definition mp_rotate_left_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDmp_rotate_left_global_precision))%Q
    | 2%positive => ((s IDmp_rotate_left_global_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 3%positive => ((s IDmp_rotate_left_global_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 4%positive => ((s IDmp_rotate_left_global_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 5%positive => ((s IDmp_rotate_left_precision) + (s IDmp_rotate_left_z))%Q
    | 6%positive => ((s IDmp_rotate_left_precision) + (s IDmp_rotate_left_z))%Q
    | 7%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 8%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 9%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                     + (s IDmp_rotate_left_z))%Q
    | 10%positive => ((s IDmp_rotate_left_z))%Q
    | 11%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | 12%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | 13%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | 14%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | 15%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | 16%positive => ((1 # 1) + (s IDmp_rotate_left_precision)
                      + (s IDmp_rotate_left_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mp_rotate_left_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmp_rotate_left_precision))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmp_rotate_left_precision)) (0))) (F_max0_ge_0 ((s IDmp_rotate_left_precision)))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem mp_rotate_left_ai_correct:
  forall s p' s', steps (g_start mp_rotate_left) s (g_edges mp_rotate_left) p' s' -> mp_rotate_left_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mp_rotate_left_pot_correct:
  forall s p' s',
    steps (g_start mp_rotate_left) s (g_edges mp_rotate_left) p' s' ->
    (mp_rotate_left_pot (g_start mp_rotate_left) s >= mp_rotate_left_pot p' s')%Q.
Proof.
  check_lp mp_rotate_left_ai_correct mp_rotate_left_hints.
Qed.

