Require Import pasta.Pasta.

Notation IDint_to_uchar_z := 1%positive.
Notation IDint_to_uchar__tmp := 2%positive.
Notation IDint_to_uchar_i := 3%positive.
Notation IDint_to_uchar_max_r := 4%positive.
Notation IDint_to_uchar_min_r := 5%positive.
Notation IDint_to_uchar_r_dref_off0 := 6%positive.
Notation IDint_to_uchar_in := 7%positive.
Notation IDint_to_uchar_r := 8%positive.
Notation IDint_to_uchar_size := 9%positive.
Definition int_to_uchar : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDint_to_uchar_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDint_to_uchar__tmp
             (Some (EVar IDint_to_uchar_size))),3%positive)::
             (3%positive,(AAssign IDint_to_uchar_max_r
             (Some (EVar IDint_to_uchar_r_dref_off0))),4%positive)::
             (4%positive,(AAssign IDint_to_uchar_min_r
             (Some (EVar IDint_to_uchar_r_dref_off0))),5%positive)::
             (5%positive,(AAssign IDint_to_uchar_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDint_to_uchar_i)
             s) < (eval (EVar IDint_to_uchar__tmp) s))%Z)),24%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDint_to_uchar_i)
             s) >= (eval (EVar IDint_to_uchar__tmp) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDint_to_uchar_max_r
             (Some (ESub (EVar IDint_to_uchar_max_r)
             (EVar IDint_to_uchar_min_r)))),11%positive)::
             (11%positive,(AAssign IDint_to_uchar_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDint_to_uchar_i)
             s) < (eval (EVar IDint_to_uchar__tmp) s))%Z)),17%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDint_to_uchar_i)
             s) >= (eval (EVar IDint_to_uchar__tmp) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDint_to_uchar_i
             (Some (EAdd (EVar IDint_to_uchar_i) (ENum (1))))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDint_to_uchar_z (Some (EAdd (ENum (1))
             (EVar IDint_to_uchar_z)))),23%positive)::
             (23%positive,AWeaken,14%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,27%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,30%positive)::
             (27%positive,(AAssign IDint_to_uchar_max_r None),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (30%positive,ANone,33%positive)::
             (31%positive,(AAssign IDint_to_uchar_min_r None),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDint_to_uchar_i
             (Some (EAdd (EVar IDint_to_uchar_i) (ENum (1))))),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDint_to_uchar_z (Some (EAdd (ENum (1))
             (EVar IDint_to_uchar_z)))),38%positive)::
             (38%positive,AWeaken,8%positive)::nil
|}.

Definition int_to_uchar_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0)%Z
    | 3%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar_z) <= 0)%Z
    | 4%positive => (1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0)%Z
    | 5%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar_z) <= 0)%Z
    | 6%positive => (1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 7%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar_z) <= 0)%Z
    | 8%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 9%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar__tmp)+ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 10%positive => (1 * (s IDint_to_uchar__tmp)+ -1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 11%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar__tmp)+ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 12%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 13%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0)%Z
    | 14%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 15%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ 1 * (s IDint_to_uchar__tmp)+ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 16%positive => (1 * (s IDint_to_uchar__tmp)+ -1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 17%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 19%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0)%Z
    | 21%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0)%Z
    | 22%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0)%Z
    | 23%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 26%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 29%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 31%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 33%positive => (-1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar_i) <= 0)%Z
    | 35%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) <= 0)%Z
    | 37%positive => (-1 * (s IDint_to_uchar_z) <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDint_to_uchar_i) + 1 <= 0 /\ -1 * (s IDint_to_uchar__tmp)+ 1 * (s IDint_to_uchar_i) <= 0 /\ -1 * (s IDint_to_uchar_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition int_to_uchar_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1) * max0((s IDint_to_uchar_size)))%Q
    | 2%positive => ((s IDint_to_uchar_z)
                     + (2 # 1) * max0((s IDint_to_uchar_size)))%Q
    | 3%positive => ((s IDint_to_uchar_z)
                     + (2 # 1) * max0((s IDint_to_uchar__tmp)))%Q
    | 4%positive => ((s IDint_to_uchar_z)
                     + (2 # 1) * max0((s IDint_to_uchar__tmp)))%Q
    | 5%positive => ((s IDint_to_uchar_z)
                     + (2 # 1) * max0((s IDint_to_uchar__tmp)))%Q
    | 6%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                     + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 7%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                     + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 8%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                     + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 9%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                     + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 10%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp)))%Q
    | 11%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp)))%Q
    | 12%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 13%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 14%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 15%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 16%positive => ((s IDint_to_uchar_z))%Q
    | 17%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 18%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i)))%Q
    | 19%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i)))%Q
    | 20%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 21%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 22%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 23%positive => ((s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 24%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 25%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 26%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 27%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 28%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 29%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 30%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i))
                      + max0((s IDint_to_uchar__tmp)))%Q
    | 31%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i))
                      + max0((s IDint_to_uchar__tmp)))%Q
    | 32%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i))
                      + max0((s IDint_to_uchar__tmp)))%Q
    | 33%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i))
                      + max0((s IDint_to_uchar__tmp)))%Q
    | 34%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0(-1 + (s IDint_to_uchar__tmp)
                             - (s IDint_to_uchar_i))
                      + max0((s IDint_to_uchar__tmp)))%Q
    | 35%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 36%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 37%positive => ((1 # 1) + (s IDint_to_uchar_z)
                      + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | 38%positive => ((s IDint_to_uchar_z) + max0((s IDint_to_uchar__tmp))
                      + max0((s IDint_to_uchar__tmp) - (s IDint_to_uchar_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition int_to_uchar_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDint_to_uchar__tmp)
                                                            - (s IDint_to_uchar_i)) (-1
                                                                    + (s IDint_to_uchar__tmp)
                                                                    - (s IDint_to_uchar_i)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDint_to_uchar__tmp)
                                           - (s IDint_to_uchar_i))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDint_to_uchar__tmp)
                                                             - (s IDint_to_uchar_i)) (-1
                                                                    + (s IDint_to_uchar__tmp)
                                                                    - (s IDint_to_uchar_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDint_to_uchar__tmp)
                                            - (s IDint_to_uchar_i))]
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement ((s IDint_to_uchar__tmp)
                                                    - (s IDint_to_uchar_i)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_pre_decrement ((s IDint_to_uchar__tmp)
                                                     - (s IDint_to_uchar_i)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_pre_decrement ((s IDint_to_uchar__tmp)
                                                     - (s IDint_to_uchar_i)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem int_to_uchar_ai_correct:
  forall s p' s', steps (g_start int_to_uchar) s (g_edges int_to_uchar) p' s' -> int_to_uchar_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem int_to_uchar_pot_correct:
  forall s p' s',
    steps (g_start int_to_uchar) s (g_edges int_to_uchar) p' s' ->
    (int_to_uchar_pot (g_start int_to_uchar) s >= int_to_uchar_pot p' s')%Q.
Proof.
  check_lp int_to_uchar_ai_correct int_to_uchar_hints.
Qed.

