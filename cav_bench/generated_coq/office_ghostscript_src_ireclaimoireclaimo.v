Require Import pasta.Pasta.

Notation IDireclaim_z := 1%positive.
Notation IDireclaim__tmp := 2%positive.
Notation IDireclaim__tmp1 := 3%positive.
Notation IDireclaim_global := 4%positive.
Notation IDireclaim_i := 5%positive.
Notation IDireclaim_dmem := 6%positive.
Notation IDireclaim_space := 7%positive.
Definition ireclaim : graph := {|
  g_start := 1%positive;
  g_end := 33%positive;
  g_edges := (1%positive,(AAssign IDireclaim_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDireclaim_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDireclaim__tmp
             (Some (EVar IDireclaim_space))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDireclaim__tmp) s) <
             (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDireclaim__tmp)
             s) >= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,24%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDireclaim_i (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDireclaim_i) s) <
             (eval (ENum (4)) s))%Z)),15%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDireclaim_i) s) >=
             (eval (ENum (4)) s))%Z)),14%positive)::
             (14%positive,AWeaken,22%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,34%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,20%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,35%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,30%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDireclaim_global None),27%positive)::
             (27%positive,(AAssign IDireclaim__tmp1 (Some (ENum (0)))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,33%positive)::
             (30%positive,(AAssign IDireclaim__tmp1 (Some (ENum (-25)))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDireclaim_i
             (Some (EAdd (EVar IDireclaim_i) (ENum (1))))),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDireclaim_z (Some (EAdd (ENum (1))
             (EVar IDireclaim_z)))),39%positive)::
             (39%positive,AWeaken,13%positive)::nil
|}.

Definition ireclaim_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 3%positive => (-1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 4%positive => (-1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 5%positive => (-1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 6%positive => (-1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 7%positive => (-1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDireclaim__tmp) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 9%positive => (-1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 10%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 11%positive => (-1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 12%positive => (-1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 13%positive => (-1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0)%Z
    | 14%positive => (1 * (s IDireclaim_i) + -4 <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) + 4 <= 0)%Z
    | 15%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDireclaim_i) + -3 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 17%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -3 <= 0)%Z
    | 18%positive => (1 * (s IDireclaim_i) + -3 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 19%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -3 <= 0)%Z
    | 20%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDireclaim_i) + -3 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDireclaim_i) + -4 <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 23%positive => (-1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0)%Z
    | 24%positive => (-1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 25%positive => (-1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 26%positive => (-1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0)%Z
    | 27%positive => (-1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 28%positive => (-1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim__tmp1) <= 0 /\ -1 * (s IDireclaim__tmp1) <= 0)%Z
    | 29%positive => (-1 * (s IDireclaim__tmp1) <= 0 /\ 1 * (s IDireclaim__tmp1) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0)%Z
    | 30%positive => (-1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0)%Z
    | 31%positive => (1 * (s IDireclaim_i) + -4 <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim__tmp1) + 25 <= 0 /\ -1 * (s IDireclaim__tmp1) + -25 <= 0)%Z
    | 32%positive => (-1 * (s IDireclaim__tmp1) + -25 <= 0 /\ 1 * (s IDireclaim__tmp1) + 25 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDireclaim__tmp1) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim__tmp1) + -25 <= 0)%Z
    | 34%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -3 <= 0)%Z
    | 35%positive => (1 * (s IDireclaim_i) + -3 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ -1 * (s IDireclaim_i) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0 /\ -1 * (s IDireclaim_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDireclaim_i) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_z) <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0 /\ -1 * (s IDireclaim_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDireclaim_i) + 1 <= 0 /\ 1 * (s IDireclaim_i) + -4 <= 0 /\ 1 * (s IDireclaim__tmp) + 1 <= 0 /\ -1 * (s IDireclaim_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ireclaim_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 3%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 4%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 5%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 6%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 7%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 8%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                     - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 9%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 10%positive => ((4 # 1) + (s IDireclaim_z))%Q
    | 11%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 12%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 13%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 14%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 15%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 16%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 17%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 18%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 19%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 20%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 21%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 22%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 23%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 24%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 25%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 26%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 27%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 28%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 29%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 30%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 31%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 32%positive => ((1 # 3) * (s IDireclaim_i) + (s IDireclaim_z)
                      - (1 # 3) * max0((s IDireclaim_i)))%Q
    | 33%positive => ((s IDireclaim_z))%Q
    | 34%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 35%positive => ((1 # 1) + (s IDireclaim_z) + max0(3 - (s IDireclaim_i)))%Q
    | 36%positive => ((1 # 1) + (s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 37%positive => ((1 # 1) + (s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 38%positive => ((1 # 1) + (s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | 39%positive => ((s IDireclaim_z) + max0(4 - (s IDireclaim_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ireclaim_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-4 0*) F_one;
                     (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDireclaim_i))) (F_check_ge ((s IDireclaim_i)) (0))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDireclaim_i)) (3
                                                                    - (s IDireclaim_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDireclaim_i));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDireclaim_i))) (F_check_ge ((s IDireclaim_i)) (0))]
    | 15%positive => [(*0 1*) F_max0_pre_decrement (4 - (s IDireclaim_i)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1.33333 0*) F_max0_ge_0 (3 - (s IDireclaim_i));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDireclaim_i))) (F_check_ge (0) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDireclaim_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDireclaim_i)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDireclaim_i)) (0))) (F_max0_ge_0 ((s IDireclaim_i)))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDireclaim_i)) (0))) (F_max0_ge_0 ((s IDireclaim_i)))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | _ => []
  end.


Theorem ireclaim_ai_correct:
  forall s p' s', steps (g_start ireclaim) s (g_edges ireclaim) p' s' -> ireclaim_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ireclaim_pot_correct:
  forall s p' s',
    steps (g_start ireclaim) s (g_edges ireclaim) p' s' ->
    (ireclaim_pot (g_start ireclaim) s >= ireclaim_pot p' s')%Q.
Proof.
  check_lp ireclaim_ai_correct ireclaim_hints.
Qed.

