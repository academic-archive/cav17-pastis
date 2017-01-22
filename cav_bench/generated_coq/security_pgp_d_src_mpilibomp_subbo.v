Require Import pasta.Pasta.

Notation IDmp_subb_z := 1%positive.
Notation IDmp_subb__tmp := 2%positive.
Notation IDmp_subb_global_precision := 3%positive.
Notation IDmp_subb_precision := 4%positive.
Notation IDmp_subb_x := 5%positive.
Notation IDmp_subb_borrow := 6%positive.
Notation IDmp_subb_r1 := 7%positive.
Notation IDmp_subb_r2 := 8%positive.
Definition mp_subb : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDmp_subb_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmp_subb__tmp
             (Some (EVar IDmp_subb_borrow))),3%positive)::
             (3%positive,(AAssign IDmp_subb_precision
             (Some (EVar IDmp_subb_global_precision))),4%positive)::
             (4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDmp_subb_precision
             (Some (EAdd (EVar IDmp_subb_precision) (ENum (-1))))),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDmp_subb_precision)
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDmp_subb_precision)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDmp_subb__tmp)
             s) <> (eval (ENum (0)) s))%Z)),16%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDmp_subb__tmp) s) =
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDmp_subb_x None),14%positive)::
             (14%positive,(AAssign IDmp_subb__tmp None),15%positive)::
             (15%positive,ANone,20%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDmp_subb_x None),18%positive)::
             (18%positive,(AAssign IDmp_subb__tmp None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDmp_subb_z (Some (EAdd (ENum (1))
             (EVar IDmp_subb_z)))),5%positive)::nil
|}.

Definition mp_subb_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmp_subb_z) <= 0 /\ -1 * (s IDmp_subb_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmp_subb_z) <= 0 /\ 1 * (s IDmp_subb_z) <= 0)%Z
    | 4%positive => (1 * (s IDmp_subb_z) <= 0 /\ -1 * (s IDmp_subb_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmp_subb_z) <= 0 /\ 1 * (s IDmp_subb_precision) <= 0 /\ -1 * (s IDmp_subb_precision) <= 0)%Z
    | 9%positive => (-1 * (s IDmp_subb_precision) <= 0 /\ 1 * (s IDmp_subb_precision) <= 0 /\ -1 * (s IDmp_subb_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmp_subb_z) <= 0 /\ 1 * (s IDmp_subb__tmp) <= 0 /\ -1 * (s IDmp_subb__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDmp_subb__tmp) <= 0 /\ 1 * (s IDmp_subb__tmp) <= 0 /\ -1 * (s IDmp_subb_z) <= 0)%Z
    | 14%positive => (-1 * (s IDmp_subb_z) <= 0 /\ 1 * (s IDmp_subb__tmp) <= 0 /\ -1 * (s IDmp_subb__tmp) <= 0)%Z
    | 15%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 17%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 19%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 20%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 21%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | 22%positive => (-1 * (s IDmp_subb_z) <= 0)%Z
    | _ => False
  end.

Definition mp_subb_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDmp_subb_global_precision))%Q
    | 2%positive => ((s IDmp_subb_global_precision) + (s IDmp_subb_z))%Q
    | 3%positive => ((s IDmp_subb_global_precision) + (s IDmp_subb_z))%Q
    | 4%positive => ((s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 5%positive => ((s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 6%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 7%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 8%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 9%positive => ((s IDmp_subb_z))%Q
    | 10%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 11%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 12%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 13%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 14%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 15%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 16%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 17%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 18%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 19%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 20%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 21%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | 22%positive => ((1 # 1) + (s IDmp_subb_precision) + (s IDmp_subb_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mp_subb_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmp_subb_precision))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmp_subb_precision)) (0))) (F_max0_ge_0 ((s IDmp_subb_precision)))]
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
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | _ => []
  end.


Theorem mp_subb_ai_correct:
  forall s p' s', steps (g_start mp_subb) s (g_edges mp_subb) p' s' -> mp_subb_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mp_subb_pot_correct:
  forall s p' s',
    steps (g_start mp_subb) s (g_edges mp_subb) p' s' ->
    (mp_subb_pot (g_start mp_subb) s >= mp_subb_pot p' s')%Q.
Proof.
  check_lp mp_subb_ai_correct mp_subb_hints.
Qed.

