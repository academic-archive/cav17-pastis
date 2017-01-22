Require Import pasta.Pasta.

Notation IDzgstate_z := 1%positive.
Notation IDzgstate__tmp := 2%positive.
Notation IDzgstate_code := 3%positive.
Notation IDzgstate_i := 4%positive.
Notation IDzgstate_op := 5%positive.
Definition zgstate : graph := {|
  g_start := 1%positive;
  g_end := 44%positive;
  g_edges := (1%positive,(AAssign IDzgstate_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDzgstate_code None),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDzgstate_code) s) <
             (eval (ENum (0)) s))%Z)),40%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDzgstate_code) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,37%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,34%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDzgstate_i (Some (ENum (25)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDzgstate_i (Some (EAdd (EVar IDzgstate_i)
             (ENum (-1))))),13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EAdd (EVar IDzgstate_i)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),31%positive)::
             (14%positive,(AGuard (fun s => ((eval (EAdd (EVar IDzgstate_i)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,28%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,25%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDzgstate__tmp (Some (ENum (0)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,44%positive)::
             (28%positive,(AAssign IDzgstate__tmp (Some (ENum (-16)))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,AWeaken,44%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDzgstate_z (Some (EAdd (ENum (1))
             (EVar IDzgstate_z)))),11%positive)::
             (34%positive,(AAssign IDzgstate__tmp (Some (ENum (-25)))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,AWeaken,44%positive)::
             (37%positive,(AAssign IDzgstate__tmp (Some (ENum (-25)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,AWeaken,44%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDzgstate__tmp
             (Some (EVar IDzgstate_code))),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::nil
|}.

Definition zgstate_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0)%Z
    | 4%positive => (1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 6%positive => (-1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 8%positive => (-1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 9%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 10%positive => (-1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_i) + -25 <= 0 /\ -1 * (s IDzgstate_i) + 25 <= 0)%Z
    | 11%positive => (1 * (s IDzgstate_i) + -25 <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 12%positive => (-1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_i) + -25 <= 0)%Z
    | 13%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -24 <= 0)%Z
    | 14%positive => (1 * (s IDzgstate_i) + -24 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 15%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 17%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 19%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 21%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 23%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 26%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate__tmp) <= 0 /\ -1 * (s IDzgstate__tmp) <= 0)%Z
    | 27%positive => (-1 * (s IDzgstate__tmp) <= 0 /\ 1 * (s IDzgstate__tmp) <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 28%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDzgstate_i) + 1 <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate__tmp) + 16 <= 0 /\ -1 * (s IDzgstate__tmp) + -16 <= 0)%Z
    | 30%positive => (-1 * (s IDzgstate__tmp) + -16 <= 0 /\ 1 * (s IDzgstate__tmp) + 16 <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -1 <= 0 /\ -1 * (s IDzgstate_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -24 <= 0)%Z
    | 32%positive => (1 * (s IDzgstate_i) + -24 <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 33%positive => (-1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_i) + -24 <= 0)%Z
    | 34%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 35%positive => (-1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate__tmp) + 25 <= 0 /\ -1 * (s IDzgstate__tmp) + -25 <= 0)%Z
    | 36%positive => (-1 * (s IDzgstate__tmp) + -25 <= 0 /\ 1 * (s IDzgstate__tmp) + 25 <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 37%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 38%positive => (-1 * (s IDzgstate_code) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate__tmp) + 25 <= 0 /\ -1 * (s IDzgstate__tmp) + -25 <= 0)%Z
    | 39%positive => (-1 * (s IDzgstate__tmp) + -25 <= 0 /\ 1 * (s IDzgstate__tmp) + 25 <= 0 /\ -1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_code) <= 0)%Z
    | 40%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_code) + 1 <= 0)%Z
    | 41%positive => (1 * (s IDzgstate_code) + 1 <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 42%positive => (-1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ 1 * (s IDzgstate_code) + 1 <= 0 /\ 1 * (s IDzgstate__tmp) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDzgstate__tmp) + 1 <= 0 /\ 1 * (s IDzgstate_code) + 1 <= 0 /\ 1 * (s IDzgstate_z) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | 44%positive => (1 * (s IDzgstate__tmp) <= 0 /\ -1 * (s IDzgstate_z) <= 0)%Z
    | _ => False
  end.

Definition zgstate_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((23 # 1))%Q
    | 2%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 3%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 4%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 5%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 6%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 7%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 8%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 9%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 10%positive => (-(2 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 11%positive => (-(2 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 12%positive => (-(2 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 13%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 14%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 15%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 16%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 17%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 18%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 19%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 20%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 21%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 22%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 23%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 24%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 25%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 26%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 27%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 28%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 29%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 30%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 31%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 32%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 33%positive => (-(1 # 1) + (s IDzgstate_i) + (s IDzgstate_z))%Q
    | 34%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 35%positive => ((s IDzgstate_z)
                      + (23 # 9) * max0(-16 - (s IDzgstate__tmp)))%Q
    | 36%positive => ((s IDzgstate_z)
                      + (23 # 9) * max0(-16 - (s IDzgstate__tmp)))%Q
    | 37%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 38%positive => ((s IDzgstate_z)
                      + (23 # 9) * max0(-16 - (s IDzgstate__tmp)))%Q
    | 39%positive => ((s IDzgstate_z)
                      + (23 # 9) * max0(-16 - (s IDzgstate__tmp)))%Q
    | 40%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 41%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 42%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 43%positive => ((23 # 1) + (s IDzgstate_z))%Q
    | 44%positive => ((s IDzgstate_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zgstate_hints (p : node) (s : state) := 
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
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzgstate_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzgstate_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzgstate_i)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzgstate_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzgstate_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzgstate_i)))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-2.55556 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    - (s IDzgstate__tmp))) (F_check_ge (0) (0))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-2.55556 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    - (s IDzgstate__tmp))) (F_check_ge (0) (0))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-23 0*) F_one]
    | 44%positive => []
    | _ => []
  end.


Theorem zgstate_ai_correct:
  forall s p' s', steps (g_start zgstate) s (g_edges zgstate) p' s' -> zgstate_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zgstate_pot_correct:
  forall s p' s',
    steps (g_start zgstate) s (g_edges zgstate) p' s' ->
    (zgstate_pot (g_start zgstate) s >= zgstate_pot p' s')%Q.
Proof.
  check_lp zgstate_ai_correct zgstate_hints.
Qed.

