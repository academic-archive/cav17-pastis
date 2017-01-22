Require Import pasta.Pasta.

Notation IDgs_interp_make_oper_z := 1%positive.
Notation IDgs_interp_make_oper__tmp := 2%positive.
Notation IDgs_interp_make_oper_i := 3%positive.
Notation IDgs_interp_make_oper_opref_dref_off0_off0 := 4%positive.
Notation IDgs_interp_make_oper_opref_dref_off0_off2 := 5%positive.
Notation IDgs_interp_make_oper_idx := 6%positive.
Notation IDgs_interp_make_oper_opref := 7%positive.
Notation IDgs_interp_make_oper_proc := 8%positive.
Definition gs_interp_make_oper : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDgs_interp_make_oper_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgs_interp_make_oper__tmp
             (Some (EVar IDgs_interp_make_oper_idx))),3%positive)::
             (3%positive,(AAssign IDgs_interp_make_oper_i
             (Some (ENum (10)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDgs_interp_make_oper_i
             (Some (EAdd (EVar IDgs_interp_make_oper_i) (ENum (-1))))),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgs_interp_make_oper_i)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgs_interp_make_oper_i)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,14%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => True)),26%positive)::
             (12%positive,(AGuard (fun s => True)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgs_interp_make_oper_i) s) >=
             (eval (ENum (0)) s))%Z)),20%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgs_interp_make_oper_i) s) <
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDgs_interp_make_oper_opref_dref_off0_off0
             (Some (ENum (3968)))),17%positive)::
             (17%positive,(AAssign IDgs_interp_make_oper_opref_dref_off0_off2
             (Some (EVar IDgs_interp_make_oper__tmp))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,25%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDgs_interp_make_oper_opref_dref_off0_off0
             None),22%positive)::
             (22%positive,(AAssign IDgs_interp_make_oper_opref_dref_off0_off2
             (Some (EAdd (EVar IDgs_interp_make_oper_i) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDgs_interp_make_oper_z
             (Some (EAdd (ENum (1)) (EVar IDgs_interp_make_oper_z)))),
             5%positive)::nil
|}.

Definition gs_interp_make_oper_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_interp_make_oper_z) <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 4%positive => (1 * (s IDgs_interp_make_oper_z) <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -10 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 10 <= 0)%Z
    | 5%positive => (1 * (s IDgs_interp_make_oper_i) + -10 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 6%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) <= 0)%Z
    | 7%positive => (-1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ -1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) <= 0)%Z
    | 9%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDgs_interp_make_oper_i) + 1 <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 11%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDgs_interp_make_oper_i) + 1 <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 15%positive => (False)%Z
    | 16%positive => (False)%Z
    | 17%positive => (False)%Z
    | 18%positive => (False)%Z
    | 19%positive => (False)%Z
    | 20%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) <= 0)%Z
    | 21%positive => (-1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 22%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) <= 0)%Z
    | 23%positive => (-1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + -10 <= 0 /\ -1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + 1 <= 0 /\ 1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + -10 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) <= 0)%Z
    | 25%positive => (-1 * (s IDgs_interp_make_oper_i) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + -10 <= 0 /\ -1 * (s IDgs_interp_make_oper_opref_dref_off0_off2) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDgs_interp_make_oper_i) + 1 <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | 28%positive => (-1 * (s IDgs_interp_make_oper_z) <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDgs_interp_make_oper_i) + 1 <= 0 /\ 1 * (s IDgs_interp_make_oper_i) + -9 <= 0 /\ -1 * (s IDgs_interp_make_oper_z) <= 0)%Z
    | _ => False
  end.

Definition gs_interp_make_oper_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((9 # 1))%Q
    | 2%positive => ((9 # 1) + (s IDgs_interp_make_oper_z))%Q
    | 3%positive => ((9 # 1) + (s IDgs_interp_make_oper_z))%Q
    | 4%positive => (-(46 # 79) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                     + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 5%positive => (-(46 # 79) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                     + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 6%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                     + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 7%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                     + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 8%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                     + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 9%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                     + (s IDgs_interp_make_oper_z)
                     + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                     + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 10%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                      + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 11%positive => (-(9 # 17) + (7 # 132) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                      + (169 # 168) * max0((s IDgs_interp_make_oper_i)))%Q
    | 12%positive => (-(17 # 152) + (17 # 152) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 13%positive => (-(17 # 152) + (17 # 152) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 14%positive => ((s IDgs_interp_make_oper_z))%Q
    | 15%positive => ((s IDgs_interp_make_oper_z))%Q
    | 16%positive => ((s IDgs_interp_make_oper_z))%Q
    | 17%positive => ((s IDgs_interp_make_oper_z))%Q
    | 18%positive => ((s IDgs_interp_make_oper_z))%Q
    | 19%positive => ((s IDgs_interp_make_oper_z))%Q
    | 20%positive => ((s IDgs_interp_make_oper_z))%Q
    | 21%positive => ((s IDgs_interp_make_oper_z))%Q
    | 22%positive => ((s IDgs_interp_make_oper_z))%Q
    | 23%positive => ((s IDgs_interp_make_oper_z))%Q
    | 24%positive => ((s IDgs_interp_make_oper_z))%Q
    | 25%positive => ((s IDgs_interp_make_oper_z))%Q
    | 26%positive => (-(17 # 152) + (17 # 152) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(9 - (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 27%positive => ((33 # 79) + (7 # 132) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 28%positive => ((33 # 79) + (7 # 132) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | 29%positive => ((33 # 79) + (7 # 132) * (s IDgs_interp_make_oper_i)
                      + (s IDgs_interp_make_oper_z)
                      + (169 # 168) * max0(-1 + (s IDgs_interp_make_oper_i))
                      + (1 # 17) * max0(10 - (s IDgs_interp_make_oper_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_interp_make_oper_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1.00588 0*) F_max0_ge_0 ((s IDgs_interp_make_oper_i));
                     (*-0.00588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDgs_interp_make_oper_i))) (F_check_ge (0) (0));
                     (*-0.00588235 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgs_interp_make_oper_i)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgs_interp_make_oper_i)));
                     (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDgs_interp_make_oper_i))) (F_check_ge (9
                                                                    - (s IDgs_interp_make_oper_i)) (0))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*0 1.00588*) F_max0_pre_decrement ((s IDgs_interp_make_oper_i)) (1);
                      (*0 0.0588235*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (10
                                                                    - (s IDgs_interp_make_oper_i)) (0))) (F_max0_ge_0 (10
                                                                    - (s IDgs_interp_make_oper_i)))]
    | 12%positive => []
    | 13%positive => [(*-1.11765 0*) F_max0_ge_0 (-1
                                                  + (s IDgs_interp_make_oper_i));
                      (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (10
                                                                    - (s IDgs_interp_make_oper_i))) (F_check_ge (0) (0));
                      (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                    - (s IDgs_interp_make_oper_i))) (F_check_ge (0) (0));
                      (*0 0.111765*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgs_interp_make_oper_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgs_interp_make_oper_i)))]
    | 14%positive => []
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
    | 25%positive => []
    | 26%positive => [(*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDgs_interp_make_oper_i))) (F_check_ge (9
                                                                    - (s IDgs_interp_make_oper_i)) (0))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | _ => []
  end.


Theorem gs_interp_make_oper_ai_correct:
  forall s p' s', steps (g_start gs_interp_make_oper) s (g_edges gs_interp_make_oper) p' s' -> gs_interp_make_oper_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_interp_make_oper_pot_correct:
  forall s p' s',
    steps (g_start gs_interp_make_oper) s (g_edges gs_interp_make_oper) p' s' ->
    (gs_interp_make_oper_pot (g_start gs_interp_make_oper) s >= gs_interp_make_oper_pot p' s')%Q.
Proof.
  check_lp gs_interp_make_oper_ai_correct gs_interp_make_oper_hints.
Qed.

