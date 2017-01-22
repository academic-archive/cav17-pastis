Require Import pasta.Pasta.

Notation IDmake_reals_z := 1%positive.
Notation IDmake_reals__tmp := 2%positive.
Notation IDmake_reals_count := 3%positive.
Notation IDmake_reals_op := 4%positive.
Notation IDmake_reals_pval := 5%positive.
Definition make_reals : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDmake_reals_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmake_reals__tmp
             (Some (EVar IDmake_reals_count))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDmake_reals__tmp
             (Some (EAdd (EVar IDmake_reals__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmake_reals__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmake_reals__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDmake_reals_z (Some (EAdd (ENum (1))
             (EVar IDmake_reals_z)))),4%positive)::nil
|}.

Definition make_reals_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmake_reals_z) <= 0 /\ -1 * (s IDmake_reals_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmake_reals_z) <= 0 /\ 1 * (s IDmake_reals_z) <= 0)%Z
    | 4%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmake_reals_z) <= 0 /\ 1 * (s IDmake_reals__tmp) <= 0 /\ -1 * (s IDmake_reals__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDmake_reals__tmp) <= 0 /\ 1 * (s IDmake_reals__tmp) <= 0 /\ -1 * (s IDmake_reals_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmake_reals_z) <= 0)%Z
    | _ => False
  end.

Definition make_reals_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDmake_reals_count))%Q
    | 2%positive => ((s IDmake_reals_count) + (s IDmake_reals_z))%Q
    | 3%positive => ((s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 4%positive => ((s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 5%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 6%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 7%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 8%positive => ((s IDmake_reals_z))%Q
    | 9%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 10%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 11%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 12%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | 13%positive => ((1 # 1) + (s IDmake_reals__tmp) + (s IDmake_reals_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition make_reals_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmake_reals__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmake_reals__tmp)) (0))) (F_max0_ge_0 ((s IDmake_reals__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | _ => []
  end.


Theorem make_reals_ai_correct:
  forall s p' s', steps (g_start make_reals) s (g_edges make_reals) p' s' -> make_reals_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem make_reals_pot_correct:
  forall s p' s',
    steps (g_start make_reals) s (g_edges make_reals) p' s' ->
    (make_reals_pot (g_start make_reals) s >= make_reals_pot p' s')%Q.
Proof.
  check_lp make_reals_ai_correct make_reals_hints.
Qed.

