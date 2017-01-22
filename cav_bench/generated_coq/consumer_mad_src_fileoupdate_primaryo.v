Require Import pasta.Pasta.

Notation IDupdate_primary_z := 1%positive.
Notation IDupdate_primary__tmp := 2%positive.
Notation IDupdate_primary_i := 3%positive.
Notation IDupdate_primary_new_dref_off24 := 4%positive.
Notation IDupdate_primary_new := 5%positive.
Notation IDupdate_primary_tag := 6%positive.
Definition update_primary : graph := {|
  g_start := 1%positive;
  g_end := 26%positive;
  g_edges := (1%positive,(AAssign IDupdate_primary_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDupdate_primary_new_dref_off24) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDupdate_primary_i)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,7%positive)::
             (5%positive,ANone,6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDupdate_primary_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDupdate_primary_i)
             s) < (eval (EVar IDupdate_primary_new_dref_off24) s))%Z)),
             15%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDupdate_primary_i)
             s) >= (eval (EVar IDupdate_primary_new_dref_off24) s))%Z)),
             11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDupdate_primary__tmp (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,26%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,23%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDupdate_primary_i
             (Some (EAdd (EVar IDupdate_primary_i) (ENum (1))))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDupdate_primary_z (Some (EAdd (ENum (1))
             (EVar IDupdate_primary_z)))),22%positive)::
             (22%positive,AWeaken,10%positive)::
             (23%positive,(AAssign IDupdate_primary__tmp (Some (ENum (-1)))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::nil
|}.

Definition update_primary_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0)%Z
    | 3%positive => (-1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 4%positive => (-1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0)%Z
    | 5%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 6%positive => (-1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0)%Z
    | 7%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 8%positive => (-1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0)%Z
    | 9%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ 1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 10%positive => (-1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 11%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i)+ 1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 12%positive => (-1 * (s IDupdate_primary_i)+ 1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 13%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i)+ 1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ 1 * (s IDupdate_primary__tmp) <= 0 /\ -1 * (s IDupdate_primary__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDupdate_primary__tmp) <= 0 /\ 1 * (s IDupdate_primary__tmp) <= 0 /\ -1 * (s IDupdate_primary_i)+ 1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0)%Z
    | 15%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0)%Z
    | 16%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0)%Z
    | 17%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0)%Z
    | 19%positive => (-1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDupdate_primary_i) + 1 <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0)%Z
    | 21%positive => (-1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDupdate_primary_i) + 1 <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ -1 * (s IDupdate_primary_z) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ 1 * (s IDupdate_primary__tmp) + 1 <= 0 /\ -1 * (s IDupdate_primary__tmp) + -1 <= 0)%Z
    | 25%positive => (-1 * (s IDupdate_primary__tmp) + -1 <= 0 /\ 1 * (s IDupdate_primary__tmp) + 1 <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ 1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDupdate_primary_i)+ -1 * (s IDupdate_primary_new_dref_off24) <= 0 /\ 1 * (s IDupdate_primary__tmp) <= 0 /\ -1 * (s IDupdate_primary_z) <= 0 /\ -1 * (s IDupdate_primary_i) <= 0 /\ -1 * (s IDupdate_primary__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition update_primary_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDupdate_primary_new_dref_off24)))%Q
    | 2%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 3%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 4%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 5%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 6%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 7%positive => ((s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 8%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 9%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                     + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 10%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 11%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 12%positive => (-(s IDupdate_primary_new_dref_off24)
                      + (s IDupdate_primary_z)
                      + max0(-(s IDupdate_primary_i)
                             + (s IDupdate_primary_new_dref_off24))
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 13%positive => (-(s IDupdate_primary_new_dref_off24)
                      + (s IDupdate_primary_z)
                      + max0(-(s IDupdate_primary_i)
                             + (s IDupdate_primary_new_dref_off24))
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 14%positive => (-(s IDupdate_primary_new_dref_off24)
                      + (s IDupdate_primary_z)
                      + max0(-(s IDupdate_primary_i)
                             + (s IDupdate_primary_new_dref_off24))
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 15%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 16%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 17%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 18%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 19%positive => ((1 # 1) - (s IDupdate_primary_i)
                      + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 20%positive => ((1 # 1) - (s IDupdate_primary_i)
                      + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 21%positive => ((1 # 1) - (s IDupdate_primary_i)
                      + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 22%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 23%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 24%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 25%positive => (-(s IDupdate_primary_i) + (s IDupdate_primary_z)
                      + max0((s IDupdate_primary_new_dref_off24)))%Q
    | 26%positive => ((s IDupdate_primary_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition update_primary_hints (p : node) (s : state) := 
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
    | 11%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDupdate_primary_i)
                                                                    + (s IDupdate_primary_new_dref_off24)) (0))) (F_max0_ge_0 (-
                                                                    (s IDupdate_primary_i)
                                                                    + (s IDupdate_primary_new_dref_off24)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDupdate_primary_i)
                                                             + (s IDupdate_primary_new_dref_off24)) (-1
                                                                    - (s IDupdate_primary_i)
                                                                    + (s IDupdate_primary_new_dref_off24)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDupdate_primary_i)
                                            + (s IDupdate_primary_new_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDupdate_primary_new_dref_off24))) (F_check_ge ((s IDupdate_primary_new_dref_off24)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement (-(s IDupdate_primary_i)
                                                     + (s IDupdate_primary_new_dref_off24)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDupdate_primary_i)
                                            + (s IDupdate_primary_new_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDupdate_primary_new_dref_off24))) (F_check_ge ((s IDupdate_primary_new_dref_off24)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDupdate_primary_i)
                                                                    + (s IDupdate_primary_new_dref_off24)) (0))) (F_max0_ge_0 (-
                                                                    (s IDupdate_primary_i)
                                                                    + (s IDupdate_primary_new_dref_off24)))]
    | 26%positive => []
    | _ => []
  end.


Theorem update_primary_ai_correct:
  forall s p' s', steps (g_start update_primary) s (g_edges update_primary) p' s' -> update_primary_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem update_primary_pot_correct:
  forall s p' s',
    steps (g_start update_primary) s (g_edges update_primary) p' s' ->
    (update_primary_pot (g_start update_primary) s >= update_primary_pot p' s')%Q.
Proof.
  check_lp update_primary_ai_correct update_primary_hints.
Qed.

