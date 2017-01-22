Require Import pasta.Pasta.

Notation IDdash_pattern_eq_z := 1%positive.
Notation IDdash_pattern_eq__tmp := 2%positive.
Notation IDdash_pattern_eq_i := 3%positive.
Notation IDdash_pattern_eq_set_dref_off8 := 4%positive.
Notation IDdash_pattern_eq_scale := 5%positive.
Notation IDdash_pattern_eq_set := 6%positive.
Notation IDdash_pattern_eq_stored := 7%positive.
Definition dash_pattern_eq : graph := {|
  g_start := 1%positive;
  g_end := 24%positive;
  g_edges := (1%positive,(AAssign IDdash_pattern_eq_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDdash_pattern_eq_set_dref_off8) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDdash_pattern_eq_i)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDdash_pattern_eq_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDdash_pattern_eq_i)
             s) < (eval (EVar IDdash_pattern_eq_set_dref_off8) s))%Z)),
             13%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDdash_pattern_eq_i)
             s) >= (eval (EVar IDdash_pattern_eq_set_dref_off8) s))%Z)),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDdash_pattern_eq__tmp (Some (ENum (1)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,24%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,21%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDdash_pattern_eq_i
             (Some (EAdd (EVar IDdash_pattern_eq_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDdash_pattern_eq_z (Some (EAdd (ENum (1))
             (EVar IDdash_pattern_eq_z)))),20%positive)::
             (20%positive,AWeaken,8%positive)::
             (21%positive,(AAssign IDdash_pattern_eq__tmp (Some (ENum (0)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::nil
|}.

Definition dash_pattern_eq_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 4%positive => (-1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ 1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0)%Z
    | 5%positive => (-1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 6%positive => (-1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ 1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0)%Z
    | 7%positive => (-1 * (s IDdash_pattern_eq_i) <= 0 /\ 1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 8%positive => (-1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 9%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i)+ 1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 10%positive => (-1 * (s IDdash_pattern_eq_i)+ 1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 11%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i)+ 1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ 1 * (s IDdash_pattern_eq__tmp) + -1 <= 0 /\ -1 * (s IDdash_pattern_eq__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDdash_pattern_eq__tmp) + 1 <= 0 /\ 1 * (s IDdash_pattern_eq__tmp) + -1 <= 0 /\ -1 * (s IDdash_pattern_eq_i)+ 1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0)%Z
    | 13%positive => (-1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0)%Z
    | 15%positive => (-1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0)%Z
    | 16%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0)%Z
    | 17%positive => (-1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDdash_pattern_eq_i) + 1 <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0)%Z
    | 19%positive => (-1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDdash_pattern_eq_i) + 1 <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ -1 * (s IDdash_pattern_eq_z) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ 1 * (s IDdash_pattern_eq__tmp) <= 0 /\ -1 * (s IDdash_pattern_eq__tmp) <= 0)%Z
    | 23%positive => (-1 * (s IDdash_pattern_eq__tmp) <= 0 /\ 1 * (s IDdash_pattern_eq__tmp) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ 1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDdash_pattern_eq_i)+ -1 * (s IDdash_pattern_eq_set_dref_off8) <= 0 /\ 1 * (s IDdash_pattern_eq__tmp) + -1 <= 0 /\ -1 * (s IDdash_pattern_eq_z) <= 0 /\ -1 * (s IDdash_pattern_eq_i) <= 0 /\ -1 * (s IDdash_pattern_eq__tmp) <= 0)%Z
    | _ => False
  end.

Definition dash_pattern_eq_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDdash_pattern_eq_set_dref_off8)))%Q
    | 2%positive => ((s IDdash_pattern_eq_z)
                     + max0((s IDdash_pattern_eq_set_dref_off8)))%Q
    | 3%positive => ((s IDdash_pattern_eq_z)
                     + max0((s IDdash_pattern_eq_set_dref_off8)))%Q
    | 4%positive => ((s IDdash_pattern_eq_z)
                     + max0((s IDdash_pattern_eq_set_dref_off8)))%Q
    | 5%positive => ((s IDdash_pattern_eq_z)
                     + max0((s IDdash_pattern_eq_set_dref_off8)))%Q
    | 6%positive => ((s IDdash_pattern_eq_z)
                     + max0(-(s IDdash_pattern_eq_i)
                            + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 7%positive => ((s IDdash_pattern_eq_z)
                     + max0(-(s IDdash_pattern_eq_i)
                            + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 8%positive => ((s IDdash_pattern_eq_z)
                     + max0(-(s IDdash_pattern_eq_i)
                            + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 9%positive => ((s IDdash_pattern_eq_z)
                     + max0(-(s IDdash_pattern_eq_i)
                            + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 10%positive => ((s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 11%positive => ((s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 12%positive => ((s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 13%positive => ((s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 14%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 15%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 16%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 17%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 18%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 19%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 20%positive => ((s IDdash_pattern_eq_z)
                      + max0(-(s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 21%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 22%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 23%positive => ((1 # 1) + (s IDdash_pattern_eq_z)
                      + max0(-1 - (s IDdash_pattern_eq_i)
                             + (s IDdash_pattern_eq_set_dref_off8)))%Q
    | 24%positive => ((s IDdash_pattern_eq_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition dash_pattern_eq_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDdash_pattern_eq_i)
                                                             + (s IDdash_pattern_eq_set_dref_off8)) (-1
                                                                    - (s IDdash_pattern_eq_i)
                                                                    + (s IDdash_pattern_eq_set_dref_off8)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDdash_pattern_eq_i)
                                            + (s IDdash_pattern_eq_set_dref_off8))]
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDdash_pattern_eq_i)
                                                     + (s IDdash_pattern_eq_set_dref_off8)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDdash_pattern_eq_i)
                                            + (s IDdash_pattern_eq_set_dref_off8))]
    | 24%positive => []
    | _ => []
  end.


Theorem dash_pattern_eq_ai_correct:
  forall s p' s', steps (g_start dash_pattern_eq) s (g_edges dash_pattern_eq) p' s' -> dash_pattern_eq_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem dash_pattern_eq_pot_correct:
  forall s p' s',
    steps (g_start dash_pattern_eq) s (g_edges dash_pattern_eq) p' s' ->
    (dash_pattern_eq_pot (g_start dash_pattern_eq) s >= dash_pattern_eq_pot p' s')%Q.
Proof.
  check_lp dash_pattern_eq_ai_correct dash_pattern_eq_hints.
Qed.

