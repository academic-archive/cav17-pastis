Require Import pasta.Pasta.

Notation IDid3_render_paddedstring_z := 1%positive.
Notation IDid3_render_paddedstring__tmp := 2%positive.
Notation IDid3_render_paddedstring_length := 3%positive.
Notation IDid3_render_paddedstring_ptr := 4%positive.
Notation IDid3_render_paddedstring_ucs4 := 5%positive.
Definition id3_render_paddedstring : graph := {|
  g_start := 1%positive;
  g_end := 28%positive;
  g_edges := (1%positive,(AAssign IDid3_render_paddedstring_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDid3_render_paddedstring__tmp
             (Some (EVar IDid3_render_paddedstring_length))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) <=
             (eval (ENum (30)) s))%Z)),10%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) >
             (eval (ENum (30)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,28%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (13%positive,ANone,23%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,22%positive)::
             (17%positive,(AAssign IDid3_render_paddedstring__tmp
             (Some (EAdd (EVar IDid3_render_paddedstring__tmp)
             (ENum (-1))))),18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) <>
             (eval (ENum (0)) s))%Z)),33%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) =
             (eval (ENum (0)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDid3_render_paddedstring__tmp
             (Some (EAdd (EVar IDid3_render_paddedstring__tmp)
             (ENum (-1))))),25%positive)::(25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) <>
             (eval (ENum (0)) s))%Z)),29%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDid3_render_paddedstring__tmp) s) =
             (eval (ENum (0)) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDid3_render_paddedstring_z
             (Some (EAdd (ENum (1)) (EVar IDid3_render_paddedstring_z)))),
             24%positive)::(33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (34%positive,ANone,36%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDid3_render_paddedstring_z
             (Some (EAdd (ENum (1)) (EVar IDid3_render_paddedstring_z)))),
             39%positive)::(39%positive,AWeaken,16%positive)::nil
|}.

Definition id3_render_paddedstring_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 3%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDid3_render_paddedstring__tmp) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 5%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 6%positive => (1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 7%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring__tmp) + 31 <= 0)%Z
    | 8%positive => (-1 * (s IDid3_render_paddedstring__tmp) + 31 <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 9%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring__tmp) + 31 <= 0)%Z
    | 10%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0)%Z
    | 11%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 12%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0)%Z
    | 13%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 14%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0)%Z
    | 15%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ 1 * (s IDid3_render_paddedstring_z) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 16%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0)%Z
    | 17%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 18%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 19%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 20%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 21%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) <= 0 /\ -1 * (s IDid3_render_paddedstring__tmp) <= 0)%Z
    | 22%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 23%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0)%Z
    | 24%positive => (1 * (s IDid3_render_paddedstring__tmp) + -30 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 25%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 26%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 27%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) <= 0 /\ -1 * (s IDid3_render_paddedstring__tmp) <= 0)%Z
    | 28%positive => (-1 * (s IDid3_render_paddedstring__tmp) <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 29%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 30%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 31%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 32%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 33%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 34%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 35%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 36%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 37%positive => (-1 * (s IDid3_render_paddedstring_z) <= 0 /\ 1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0)%Z
    | 38%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) <= 0)%Z
    | 39%positive => (1 * (s IDid3_render_paddedstring__tmp) + -29 <= 0 /\ -1 * (s IDid3_render_paddedstring_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition id3_render_paddedstring_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((29 # 1))%Q
    | 2%positive => ((29 # 1))%Q
    | 3%positive => ((29 # 1))%Q
    | 4%positive => ((29 # 1))%Q
    | 5%positive => ((29 # 1))%Q
    | 6%positive => ((29 # 1))%Q
    | 7%positive => ((29 # 1))%Q
    | 8%positive => (0)%Q
    | 9%positive => (0)%Q
    | 10%positive => ((29 # 1))%Q
    | 11%positive => ((29 # 1))%Q
    | 12%positive => ((29 # 1))%Q
    | 13%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 14%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 15%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 16%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 17%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 18%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(29 - (s IDid3_render_paddedstring__tmp)))%Q
    | 19%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(29 - (s IDid3_render_paddedstring__tmp)))%Q
    | 20%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 21%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 22%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 23%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 24%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 25%positive => ((29 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(29 - (s IDid3_render_paddedstring__tmp)))%Q
    | 26%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 27%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 28%positive => ((s IDid3_render_paddedstring_z))%Q
    | 29%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 30%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 31%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 32%positive => ((30 # 1) + (s IDid3_render_paddedstring_z)
                      - max0(30 - (s IDid3_render_paddedstring__tmp)))%Q
    | 33%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 34%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 35%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 36%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 37%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 38%positive => ((29 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | 39%positive => ((28 # 1)
                      + (1 # 30) * (s IDid3_render_paddedstring__tmp)
                      + (s IDid3_render_paddedstring_z)
                      - (29 # 30) * max0(30
                                         - (s IDid3_render_paddedstring__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition id3_render_paddedstring_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-29 0*) F_one]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDid3_render_paddedstring_z))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDid3_render_paddedstring_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDid3_render_paddedstring_z)))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDid3_render_paddedstring_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDid3_render_paddedstring_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDid3_render_paddedstring_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (30
                                                                - (s IDid3_render_paddedstring__tmp))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-0.966667 0*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                                    - (s IDid3_render_paddedstring__tmp))) (F_check_ge (30
                                                                    - (s IDid3_render_paddedstring__tmp)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (29
                                                                    - (s IDid3_render_paddedstring__tmp)) (0))) (F_max0_ge_0 (29
                                                                    - (s IDid3_render_paddedstring__tmp)))]
    | 20%positive => []
    | 21%positive => [(*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDid3_render_paddedstring__tmp))) (F_check_ge (0) (0));
                      (*-0.0333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_render_paddedstring__tmp)) (0))) (F_max0_ge_0 ((s IDid3_render_paddedstring__tmp)));
                      (*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (30
                                                                    - (s IDid3_render_paddedstring__tmp))) (F_check_ge (0) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                                  - (s IDid3_render_paddedstring__tmp))) (F_check_ge (30
                                                                    - (s IDid3_render_paddedstring__tmp)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (29
                                                                    - (s IDid3_render_paddedstring__tmp)) (0))) (F_max0_ge_0 (29
                                                                    - (s IDid3_render_paddedstring__tmp)))]
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDid3_render_paddedstring__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_render_paddedstring__tmp)) (0))) (F_max0_ge_0 ((s IDid3_render_paddedstring__tmp)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (30
                                                                    - (s IDid3_render_paddedstring__tmp)) (0))) (F_max0_ge_0 (30
                                                                    - (s IDid3_render_paddedstring__tmp)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                                    - (s IDid3_render_paddedstring__tmp))) (F_check_ge (30
                                                                    - (s IDid3_render_paddedstring__tmp)) (0))]
    | _ => []
  end.


Theorem id3_render_paddedstring_ai_correct:
  forall s p' s', steps (g_start id3_render_paddedstring) s (g_edges id3_render_paddedstring) p' s' -> id3_render_paddedstring_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem id3_render_paddedstring_pot_correct:
  forall s p' s',
    steps (g_start id3_render_paddedstring) s (g_edges id3_render_paddedstring) p' s' ->
    (id3_render_paddedstring_pot (g_start id3_render_paddedstring) s >= id3_render_paddedstring_pot p' s')%Q.
Proof.
  check_lp id3_render_paddedstring_ai_correct id3_render_paddedstring_hints.
Qed.

