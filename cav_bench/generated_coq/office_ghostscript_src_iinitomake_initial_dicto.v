Require Import pasta.Pasta.

Notation IDmake_initial_dict_z := 1%positive.
Notation IDmake_initial_dict_code := 2%positive.
Notation IDmake_initial_dict_dsize := 3%positive.
Notation IDmake_initial_dict_i := 4%positive.
Notation IDmake_initial_dict_space := 5%positive.
Notation IDmake_initial_dict_idicts := 6%positive.
Notation IDmake_initial_dict_iname := 7%positive.
Definition make_initial_dict : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDmake_initial_dict_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDmake_initial_dict_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,7%positive)::
             (4%positive,ANone,5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,31%positive)::
             (7%positive,(AAssign IDmake_initial_dict_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDmake_initial_dict_i) s) <
             (eval (ENum (5)) s))%Z)),14%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDmake_initial_dict_i) s) >=
             (eval (ENum (5)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,31%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDmake_initial_dict_dsize None),
             16%positive)::(16%positive,AWeaken,17%positive)::
             (17%positive,ANone,32%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (19%positive,ANone,26%positive)::
             (20%positive,(AAssign IDmake_initial_dict_space None),
             21%positive)::
             (21%positive,(AAssign IDmake_initial_dict_code None),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDmake_initial_dict_code) s) <
             (eval (ENum (0)) s))%Z)),28%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDmake_initial_dict_code) s) >=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,31%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDmake_initial_dict_i
             (Some (EAdd (EVar IDmake_initial_dict_i) (ENum (1))))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDmake_initial_dict_z
             (Some (EAdd (ENum (1)) (EVar IDmake_initial_dict_z)))),
             37%positive)::(37%positive,AWeaken,10%positive)::nil
|}.

Definition make_initial_dict_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 4%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 6%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 8%positive => (1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 9%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ 1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0)%Z
    | 11%positive => (1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) + 5 <= 0)%Z
    | 12%positive => (-1 * (s IDmake_initial_dict_i) + 5 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0)%Z
    | 13%positive => (1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) + 5 <= 0)%Z
    | 14%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 15%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 16%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 17%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 18%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 19%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 20%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 21%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 22%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 23%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 24%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_code) <= 0)%Z
    | 25%positive => (-1 * (s IDmake_initial_dict_code) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 26%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 27%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 28%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ 1 * (s IDmake_initial_dict_code) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDmake_initial_dict_code) + 1 <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 30%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ 1 * (s IDmake_initial_dict_code) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 32%positive => (-1 * (s IDmake_initial_dict_i) <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDmake_initial_dict_i) + -4 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0 /\ -1 * (s IDmake_initial_dict_i) <= 0)%Z
    | 34%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDmake_initial_dict_i) + 1 <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_z) <= 0)%Z
    | 36%positive => (-1 * (s IDmake_initial_dict_z) <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDmake_initial_dict_i) + 1 <= 0 /\ 1 * (s IDmake_initial_dict_i) + -5 <= 0 /\ -1 * (s IDmake_initial_dict_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition make_initial_dict_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((5 # 1))%Q
    | 2%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 3%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 4%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 5%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 6%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 7%positive => ((5 # 1) + (s IDmake_initial_dict_z))%Q
    | 8%positive => ((s IDmake_initial_dict_z)
                     + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 9%positive => ((s IDmake_initial_dict_z)
                     + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 10%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 11%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 12%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 13%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 14%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 15%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 16%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 17%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 18%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 19%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 20%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 21%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 22%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 23%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 24%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 25%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 26%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 27%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 28%positive => (-(1 # 4) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0(5 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 29%positive => (-(1 # 20) * (s IDmake_initial_dict_i)
                      + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 30%positive => (-(1 # 20) * (s IDmake_initial_dict_i)
                      + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i))
                      + (1 # 4) * max0((s IDmake_initial_dict_i)))%Q
    | 31%positive => ((s IDmake_initial_dict_z))%Q
    | 32%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 33%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(4 - (s IDmake_initial_dict_i)))%Q
    | 34%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 35%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 36%positive => ((1 # 1) + (s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | 37%positive => ((s IDmake_initial_dict_z)
                      + max0(5 - (s IDmake_initial_dict_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition make_initial_dict_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-5 0*) F_one]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (5
                                                             - (s IDmake_initial_dict_i)) (4
                                                                    - (s IDmake_initial_dict_i)));
                      (*-1 0*) F_max0_ge_0 (4 - (s IDmake_initial_dict_i))]
    | 14%positive => [(*0 1*) F_max0_pre_decrement (5
                                                    - (s IDmake_initial_dict_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmake_initial_dict_i)) (0))) (F_max0_ge_0 ((s IDmake_initial_dict_i)));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDmake_initial_dict_i)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDmake_initial_dict_i)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-0.25 0*) F_max0_pre_decrement (5
                                                        - (s IDmake_initial_dict_i)) (1);
                      (*-1.25 0*) F_max0_ge_0 (4 - (s IDmake_initial_dict_i));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmake_initial_dict_i))) (F_check_ge (0) (0))]
    | 28%positive => [(*0 0.2*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                  - (s IDmake_initial_dict_i))) (F_check_ge (0) (0));
                      (*0 0.05*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                                    - 
                                                                    (s IDmake_initial_dict_i))) (F_check_ge (5
                                                                    - (s IDmake_initial_dict_i)) (0))]
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_ge_0 (4 - (s IDmake_initial_dict_i));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmake_initial_dict_i))) (F_check_ge (0) (0));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmake_initial_dict_i))) (F_check_ge ((s IDmake_initial_dict_i)) (0))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | _ => []
  end.


Theorem make_initial_dict_ai_correct:
  forall s p' s', steps (g_start make_initial_dict) s (g_edges make_initial_dict) p' s' -> make_initial_dict_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem make_initial_dict_pot_correct:
  forall s p' s',
    steps (g_start make_initial_dict) s (g_edges make_initial_dict) p' s' ->
    (make_initial_dict_pot (g_start make_initial_dict) s >= make_initial_dict_pot p' s')%Q.
Proof.
  check_lp make_initial_dict_ai_correct make_initial_dict_hints.
Qed.

