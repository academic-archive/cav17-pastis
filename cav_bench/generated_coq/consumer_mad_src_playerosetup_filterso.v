Require Import pasta.Pasta.

Notation IDsetup_filters_z := 1%positive.
Notation IDsetup_filters__tmp := 2%positive.
Notation IDsetup_filters_sb := 3%positive.
Notation IDsetup_filters_player := 4%positive.
Definition setup_filters : graph := {|
  g_start := 1%positive;
  g_end := 48%positive;
  g_edges := (1%positive,(AAssign IDsetup_filters_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,5%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,8%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,45%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,10%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,18%positive)::
             (10%positive,(AAssign IDsetup_filters_sb (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDsetup_filters_sb)
             s) < (eval (ENum (32)) s))%Z)),38%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDsetup_filters_sb)
             s) >= (eval (ENum (32)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,35%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,20%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,23%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,32%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (23%positive,ANone,26%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,29%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDsetup_filters__tmp (Some (ENum (0)))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,AWeaken,48%positive)::
             (29%positive,(AAssign IDsetup_filters__tmp (Some (ENum (-1)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,48%positive)::
             (32%positive,(AAssign IDsetup_filters__tmp (Some (ENum (-1)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,48%positive)::
             (35%positive,(AAssign IDsetup_filters__tmp (Some (ENum (-1)))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,AWeaken,48%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDsetup_filters_sb
             (Some (EAdd (EVar IDsetup_filters_sb) (ENum (1))))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDsetup_filters_z (Some (EAdd (ENum (1))
             (EVar IDsetup_filters_z)))),44%positive)::
             (44%positive,AWeaken,13%positive)::
             (45%positive,(AAssign IDsetup_filters__tmp (Some (ENum (-1)))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::nil
|}.

Definition setup_filters_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0)%Z
    | 4%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 5%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 6%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0)%Z
    | 7%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 8%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0)%Z
    | 9%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 10%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 11%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) <= 0 /\ -1 * (s IDsetup_filters_sb) <= 0)%Z
    | 12%positive => (-1 * (s IDsetup_filters_sb) <= 0 /\ 1 * (s IDsetup_filters_sb) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 13%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0)%Z
    | 14%positive => (1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 32 <= 0)%Z
    | 15%positive => (-1 * (s IDsetup_filters_sb) + 32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0)%Z
    | 16%positive => (1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 32 <= 0)%Z
    | 17%positive => (-1 * (s IDsetup_filters_sb) + 32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0)%Z
    | 18%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 19%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 20%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 21%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 22%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 23%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 24%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 25%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 26%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 27%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters__tmp) <= 0 /\ -1 * (s IDsetup_filters__tmp) <= 0)%Z
    | 28%positive => (-1 * (s IDsetup_filters__tmp) <= 0 /\ 1 * (s IDsetup_filters__tmp) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 29%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 30%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters__tmp) + -1 <= 0)%Z
    | 31%positive => (-1 * (s IDsetup_filters__tmp) + -1 <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 32%positive => (-1 * (s IDsetup_filters_z) <= 0)%Z
    | 33%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters__tmp) + -1 <= 0)%Z
    | 34%positive => (-1 * (s IDsetup_filters__tmp) + -1 <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 35%positive => (1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 32 <= 0)%Z
    | 36%positive => (-1 * (s IDsetup_filters_sb) + 32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters__tmp) + -1 <= 0)%Z
    | 37%positive => (-1 * (s IDsetup_filters__tmp) + -1 <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 32 <= 0)%Z
    | 38%positive => (-1 * (s IDsetup_filters_sb) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) + -31 <= 0)%Z
    | 39%positive => (1 * (s IDsetup_filters_sb) + -31 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) <= 0)%Z
    | 40%positive => (-1 * (s IDsetup_filters_sb) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_sb) + -31 <= 0)%Z
    | 41%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 1 <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0)%Z
    | 42%positive => (1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_sb) + 1 <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 43%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_sb) + 1 <= 0 /\ 1 * (s IDsetup_filters_sb) + -32 <= 0)%Z
    | 44%positive => (1 * (s IDsetup_filters_sb) + -32 <= 0 /\ -1 * (s IDsetup_filters_sb) + 1 <= 0 /\ -1 * (s IDsetup_filters_z) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 46%positive => (-1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters_z) <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ -1 * (s IDsetup_filters__tmp) + -1 <= 0)%Z
    | 47%positive => (-1 * (s IDsetup_filters__tmp) + -1 <= 0 /\ 1 * (s IDsetup_filters__tmp) + 1 <= 0 /\ 1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0)%Z
    | 48%positive => (1 * (s IDsetup_filters__tmp) <= 0 /\ -1 * (s IDsetup_filters_z) <= 0 /\ -1 * (s IDsetup_filters__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition setup_filters_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((32 # 1))%Q
    | 2%positive => ((32 # 1) + max0((s IDsetup_filters_z)))%Q
    | 3%positive => ((32 # 1) + max0((s IDsetup_filters_z)))%Q
    | 4%positive => ((32 # 1) + max0((s IDsetup_filters_z)))%Q
    | 5%positive => ((32 # 1) + max0((s IDsetup_filters_z)))%Q
    | 6%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 7%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 8%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 9%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 10%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 11%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 12%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 13%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 14%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 15%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 16%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 17%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 18%positive => ((s IDsetup_filters_z))%Q
    | 19%positive => ((s IDsetup_filters_z))%Q
    | 20%positive => ((s IDsetup_filters_z))%Q
    | 21%positive => ((s IDsetup_filters_z))%Q
    | 22%positive => ((s IDsetup_filters_z))%Q
    | 23%positive => ((s IDsetup_filters_z))%Q
    | 24%positive => ((s IDsetup_filters_z))%Q
    | 25%positive => ((s IDsetup_filters_z))%Q
    | 26%positive => ((s IDsetup_filters_z))%Q
    | 27%positive => ((s IDsetup_filters_z))%Q
    | 28%positive => ((s IDsetup_filters_z))%Q
    | 29%positive => ((s IDsetup_filters_z))%Q
    | 30%positive => ((s IDsetup_filters_z))%Q
    | 31%positive => ((s IDsetup_filters_z))%Q
    | 32%positive => ((s IDsetup_filters_z))%Q
    | 33%positive => ((s IDsetup_filters_z))%Q
    | 34%positive => ((s IDsetup_filters_z))%Q
    | 35%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 36%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 37%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 38%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 39%positive => ((1 # 1) + (s IDsetup_filters_z)
                      + max0(31 - (s IDsetup_filters_sb)))%Q
    | 40%positive => ((1 # 1) + (s IDsetup_filters_z)
                      + max0(31 - (s IDsetup_filters_sb)))%Q
    | 41%positive => ((1 # 1) + (s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 42%positive => ((1 # 1) + (s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 43%positive => ((1 # 1) + (s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 44%positive => ((s IDsetup_filters_z)
                      + max0(32 - (s IDsetup_filters_sb)))%Q
    | 45%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 46%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 47%positive => ((32 # 1) + (s IDsetup_filters_z))%Q
    | 48%positive => ((s IDsetup_filters_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition setup_filters_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsetup_filters_z))) (F_check_ge ((s IDsetup_filters_z)) (0))]
    | 5%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsetup_filters_z))) (F_check_ge ((s IDsetup_filters_z)) (0))]
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-32 0*) F_one]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (32
                                                             - (s IDsetup_filters_sb)) (31
                                                                    - (s IDsetup_filters_sb)));
                      (*-1 0*) F_max0_ge_0 (31 - (s IDsetup_filters_sb))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (32
                                                             - (s IDsetup_filters_sb)) (31
                                                                    - (s IDsetup_filters_sb)));
                      (*-1 0*) F_max0_ge_0 (31 - (s IDsetup_filters_sb))]
    | 38%positive => [(*-1 0*) F_max0_pre_decrement (32
                                                     - (s IDsetup_filters_sb)) (1)]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-32 0*) F_one]
    | 48%positive => []
    | _ => []
  end.


Theorem setup_filters_ai_correct:
  forall s p' s', steps (g_start setup_filters) s (g_edges setup_filters) p' s' -> setup_filters_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem setup_filters_pot_correct:
  forall s p' s',
    steps (g_start setup_filters) s (g_edges setup_filters) p' s' ->
    (setup_filters_pot (g_start setup_filters) s >= setup_filters_pot p' s')%Q.
Proof.
  check_lp setup_filters_ai_correct setup_filters_hints.
Qed.

