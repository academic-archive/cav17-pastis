Require Import pasta.Pasta.

Notation IDzcurrenthalftone_z := 1%positive.
Notation IDzcurrenthalftone__tmp := 2%positive.
Notation IDzcurrenthalftone_i := 3%positive.
Notation IDzcurrenthalftone_op := 4%positive.
Definition zcurrenthalftone : graph := {|
  g_start := 1%positive;
  g_end := 46%positive;
  g_edges := (1%positive,(AAssign IDzcurrenthalftone_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,34%positive)::(3%positive,ANone,25%positive)::
             (3%positive,ANone,4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,22%positive)::(6%positive,ANone,7%positive)::
             (7%positive,ANone,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDzcurrenthalftone_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrenthalftone_i) s) <
             (eval (ENum (4)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrenthalftone_i) s) >=
             (eval (ENum (4)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,40%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDzcurrenthalftone_i
             (Some (EAdd (EVar IDzcurrenthalftone_i) (ENum (1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDzcurrenthalftone_z
             (Some (EAdd (ENum (1)) (EVar IDzcurrenthalftone_z)))),
             21%positive)::(21%positive,AWeaken,12%positive)::
             (22%positive,(AAssign IDzcurrenthalftone__tmp
             (Some (ENum (-16)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,46%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,31%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,40%positive)::
             (31%positive,(AAssign IDzcurrenthalftone__tmp
             (Some (ENum (-16)))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,46%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,43%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDzcurrenthalftone__tmp
             (Some (ENum (0)))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,46%positive)::
             (43%positive,(AAssign IDzcurrenthalftone__tmp
             (Some (ENum (-16)))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::nil
|}.

Definition zcurrenthalftone_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 4%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 6%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 8%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 9%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 10%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_i) <= 0 /\ -1 * (s IDzcurrenthalftone_i) <= 0)%Z
    | 11%positive => (-1 * (s IDzcurrenthalftone_i) <= 0 /\ 1 * (s IDzcurrenthalftone_i) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 12%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_i) <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -4 <= 0)%Z
    | 13%positive => (1 * (s IDzcurrenthalftone_i) + -4 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_i) + 4 <= 0)%Z
    | 14%positive => (-1 * (s IDzcurrenthalftone_i) + 4 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -4 <= 0)%Z
    | 15%positive => (-1 * (s IDzcurrenthalftone_i) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDzcurrenthalftone_i) + -3 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_i) <= 0)%Z
    | 17%positive => (-1 * (s IDzcurrenthalftone_i) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -3 <= 0)%Z
    | 18%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_i) + 1 <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -4 <= 0)%Z
    | 19%positive => (1 * (s IDzcurrenthalftone_i) + -4 <= 0 /\ -1 * (s IDzcurrenthalftone_i) + 1 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 20%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_i) + 1 <= 0 /\ 1 * (s IDzcurrenthalftone_i) + -4 <= 0)%Z
    | 21%positive => (1 * (s IDzcurrenthalftone_i) + -4 <= 0 /\ -1 * (s IDzcurrenthalftone_i) + 1 <= 0 /\ -1 * (s IDzcurrenthalftone_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 23%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone__tmp) + -16 <= 0)%Z
    | 24%positive => (-1 * (s IDzcurrenthalftone__tmp) + -16 <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 25%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 26%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 27%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 28%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 29%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 30%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 31%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 32%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone__tmp) + -16 <= 0)%Z
    | 33%positive => (-1 * (s IDzcurrenthalftone__tmp) + -16 <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 34%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 35%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 36%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 37%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 38%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 39%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 40%positive => (-1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 41%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) <= 0 /\ -1 * (s IDzcurrenthalftone__tmp) <= 0)%Z
    | 42%positive => (-1 * (s IDzcurrenthalftone__tmp) <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 43%positive => (-1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 44%positive => (1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone__tmp) + -16 <= 0)%Z
    | 45%positive => (-1 * (s IDzcurrenthalftone__tmp) + -16 <= 0 /\ 1 * (s IDzcurrenthalftone__tmp) + 16 <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ 1 * (s IDzcurrenthalftone_z) <= 0)%Z
    | 46%positive => (1 * (s IDzcurrenthalftone__tmp) <= 0 /\ -1 * (s IDzcurrenthalftone_z) <= 0 /\ -1 * (s IDzcurrenthalftone__tmp) + -16 <= 0)%Z
    | _ => False
  end.

Definition zcurrenthalftone_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 3%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 4%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 5%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 6%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 7%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 8%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 9%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 10%positive => (max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 11%positive => (max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 12%positive => (max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 13%positive => (max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 14%positive => ((s IDzcurrenthalftone_z))%Q
    | 15%positive => (max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 16%positive => ((1 # 1) + max0(3 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 17%positive => ((1 # 1) + max0(3 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 18%positive => ((1 # 1) + max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 19%positive => ((1 # 1) + max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 20%positive => ((1 # 1) + max0(4 - (s IDzcurrenthalftone_i))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 21%positive => ((1 # 1) + max0(-1 + (s IDzcurrenthalftone_z))
                      + max0(4 - (s IDzcurrenthalftone_i)))%Q
    | 22%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 23%positive => ((1 # 4) * max0(-(s IDzcurrenthalftone__tmp))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 24%positive => ((1 # 4) * max0(-(s IDzcurrenthalftone__tmp))
                      + max0((s IDzcurrenthalftone_z)))%Q
    | 25%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 26%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 27%positive => ((s IDzcurrenthalftone_z))%Q
    | 28%positive => ((s IDzcurrenthalftone_z))%Q
    | 29%positive => ((s IDzcurrenthalftone_z))%Q
    | 30%positive => ((s IDzcurrenthalftone_z))%Q
    | 31%positive => ((s IDzcurrenthalftone_z))%Q
    | 32%positive => ((s IDzcurrenthalftone_z))%Q
    | 33%positive => ((s IDzcurrenthalftone_z))%Q
    | 34%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 35%positive => ((4 # 1) + max0((s IDzcurrenthalftone_z)))%Q
    | 36%positive => ((s IDzcurrenthalftone_z))%Q
    | 37%positive => ((s IDzcurrenthalftone_z))%Q
    | 38%positive => ((s IDzcurrenthalftone_z))%Q
    | 39%positive => ((s IDzcurrenthalftone_z))%Q
    | 40%positive => ((s IDzcurrenthalftone_z))%Q
    | 41%positive => ((s IDzcurrenthalftone_z))%Q
    | 42%positive => ((s IDzcurrenthalftone_z))%Q
    | 43%positive => ((s IDzcurrenthalftone_z))%Q
    | 44%positive => ((s IDzcurrenthalftone_z))%Q
    | 45%positive => ((s IDzcurrenthalftone_z))%Q
    | 46%positive => ((s IDzcurrenthalftone_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zcurrenthalftone_hints (p : node) (s : state) := 
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
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzcurrenthalftone_i)) (3
                                                                    - (s IDzcurrenthalftone_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzcurrenthalftone_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzcurrenthalftone_z))) (F_check_ge ((s IDzcurrenthalftone_z)) (0))]
    | 14%positive => []
    | 15%positive => [(*0 1*) F_max0_pre_decrement (4
                                                    - (s IDzcurrenthalftone_i)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzcurrenthalftone_z)) (0))) (F_max0_ge_0 ((s IDzcurrenthalftone_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDzcurrenthalftone_z))) (F_check_ge (-1
                                                                    + (s IDzcurrenthalftone_z)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzcurrenthalftone_z))) (F_check_ge ((s IDzcurrenthalftone_z)) (0));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDzcurrenthalftone__tmp))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => [(*-4 0*) F_one;
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzcurrenthalftone_z))) (F_check_ge ((s IDzcurrenthalftone_z)) (0))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*0 4*) F_one;
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzcurrenthalftone_z))) (F_check_ge ((s IDzcurrenthalftone_z)) (0))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | _ => []
  end.


Theorem zcurrenthalftone_ai_correct:
  forall s p' s', steps (g_start zcurrenthalftone) s (g_edges zcurrenthalftone) p' s' -> zcurrenthalftone_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zcurrenthalftone_pot_correct:
  forall s p' s',
    steps (g_start zcurrenthalftone) s (g_edges zcurrenthalftone) p' s' ->
    (zcurrenthalftone_pot (g_start zcurrenthalftone) s >= zcurrenthalftone_pot p' s')%Q.
Proof.
  check_lp zcurrenthalftone_ai_correct zcurrenthalftone_hints.
Qed.

