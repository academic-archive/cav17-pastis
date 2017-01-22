Require Import pasta.Pasta.

Notation IDcompressSignature_z := 1%positive.
Notation IDcompressSignature__tmp := 2%positive.
Notation IDcompressSignature_i := 3%positive.
Notation IDcompressSignature_header := 4%positive.
Definition compressSignature : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDcompressSignature_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcompressSignature_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcompressSignature_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcompressSignature_i) s) <
             (eval (ENum (10)) s))%Z)),30%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcompressSignature_i) s) >=
             (eval (ENum (10)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,13%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDcompressSignature__tmp
             (Some (EVar IDcompressSignature_i))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,35%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (14%positive,ANone,24%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,24%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,22%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (20%positive,ANone,24%positive)::
             (21%positive,AWeaken,23%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,27%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDcompressSignature__tmp
             (Some (ENum (-1)))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,35%positive)::
             (27%positive,(AAssign IDcompressSignature__tmp
             (Some (EAdd (EVar IDcompressSignature_i) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,35%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,36%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcompressSignature__tmp
             (Some (EVar IDcompressSignature_i))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDcompressSignature_i
             (Some (EAdd (EVar IDcompressSignature_i) (ENum (1))))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDcompressSignature_z
             (Some (EAdd (ENum (1)) (EVar IDcompressSignature_z)))),
             41%positive)::(41%positive,AWeaken,7%positive)::nil
|}.

Definition compressSignature_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcompressSignature_i) <= 0 /\ 1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcompressSignature_i) <= 0 /\ 1 * (s IDcompressSignature_i) <= 0 /\ 1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 8%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 9%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 10%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 11%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ 1 * (s IDcompressSignature__tmp) + -10 <= 0 /\ -1 * (s IDcompressSignature__tmp) + 10 <= 0)%Z
    | 12%positive => (-1 * (s IDcompressSignature__tmp) + 10 <= 0 /\ 1 * (s IDcompressSignature__tmp) + -10 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 13%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 14%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 15%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 16%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 17%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 18%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 19%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 20%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 21%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 22%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 23%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0)%Z
    | 24%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 25%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ 1 * (s IDcompressSignature__tmp) + 1 <= 0 /\ -1 * (s IDcompressSignature__tmp) + -1 <= 0)%Z
    | 26%positive => (-1 * (s IDcompressSignature__tmp) + -1 <= 0 /\ 1 * (s IDcompressSignature__tmp) + 1 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 27%positive => (1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 28%positive => (-1 * (s IDcompressSignature_i) + 10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ 1 * (s IDcompressSignature__tmp) + -11 <= 0 /\ -1 * (s IDcompressSignature__tmp) + 11 <= 0)%Z
    | 29%positive => (-1 * (s IDcompressSignature__tmp) + 11 <= 0 /\ 1 * (s IDcompressSignature__tmp) + -11 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) + 10 <= 0)%Z
    | 30%positive => (-1 * (s IDcompressSignature_i) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -9 <= 0)%Z
    | 31%positive => (1 * (s IDcompressSignature_i) + -9 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0)%Z
    | 32%positive => (-1 * (s IDcompressSignature_i) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -9 <= 0)%Z
    | 33%positive => (1 * (s IDcompressSignature_i) + -9 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0 /\ 1 * (s IDcompressSignature__tmp) + -9 <= 0 /\ -1 * (s IDcompressSignature__tmp) <= 0)%Z
    | 34%positive => (-1 * (s IDcompressSignature__tmp) <= 0 /\ 1 * (s IDcompressSignature__tmp) + -9 <= 0 /\ -1 * (s IDcompressSignature_i) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -9 <= 0)%Z
    | 35%positive => (-1 * (s IDcompressSignature__tmp) + -1 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ 1 * (s IDcompressSignature__tmp) + -11 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0)%Z
    | 36%positive => (-1 * (s IDcompressSignature_i) <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -9 <= 0)%Z
    | 37%positive => (1 * (s IDcompressSignature_i) + -9 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0 /\ -1 * (s IDcompressSignature_i) <= 0)%Z
    | 38%positive => (-1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDcompressSignature_i) + 1 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) <= 0)%Z
    | 40%positive => (-1 * (s IDcompressSignature_z) <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDcompressSignature_i) + 1 <= 0 /\ 1 * (s IDcompressSignature_i) + -10 <= 0 /\ -1 * (s IDcompressSignature_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition compressSignature_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((5 # 1) * max0(2 + (s IDcompressSignature_i)))%Q
    | 2%positive => ((s IDcompressSignature_z)
                     + (5 # 1) * max0(2 + (s IDcompressSignature_i)))%Q
    | 3%positive => ((s IDcompressSignature_z)
                     + (5 # 1) * max0(2 + (s IDcompressSignature_i)))%Q
    | 4%positive => ((10 # 1) + (s IDcompressSignature_z))%Q
    | 5%positive => ((s IDcompressSignature_z)
                     + max0(10 - (s IDcompressSignature_i)))%Q
    | 6%positive => ((s IDcompressSignature_z)
                     + max0(10 - (s IDcompressSignature_i)))%Q
    | 7%positive => ((s IDcompressSignature_z)
                     + max0(10 - (s IDcompressSignature_i)))%Q
    | 8%positive => ((s IDcompressSignature_z)
                     + max0(10 - (s IDcompressSignature_i)))%Q
    | 9%positive => ((s IDcompressSignature_z)
                     + max0(10 - (s IDcompressSignature_i)))%Q
    | 10%positive => ((s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 11%positive => ((s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature__tmp))
                      - max0(9 - (s IDcompressSignature_i))
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 12%positive => ((s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature__tmp))
                      - max0(9 - (s IDcompressSignature_i))
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 13%positive => ((s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 14%positive => ((s IDcompressSignature_z))%Q
    | 15%positive => ((s IDcompressSignature_z))%Q
    | 16%positive => ((s IDcompressSignature_z))%Q
    | 17%positive => ((s IDcompressSignature_z))%Q
    | 18%positive => ((s IDcompressSignature_z))%Q
    | 19%positive => ((s IDcompressSignature_z))%Q
    | 20%positive => ((s IDcompressSignature_z))%Q
    | 21%positive => ((s IDcompressSignature_z))%Q
    | 22%positive => ((s IDcompressSignature_z))%Q
    | 23%positive => ((s IDcompressSignature_z))%Q
    | 24%positive => ((s IDcompressSignature_z))%Q
    | 25%positive => ((s IDcompressSignature_z))%Q
    | 26%positive => ((s IDcompressSignature_z))%Q
    | 27%positive => ((s IDcompressSignature_z))%Q
    | 28%positive => ((s IDcompressSignature_z))%Q
    | 29%positive => ((s IDcompressSignature_z))%Q
    | 30%positive => ((s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 31%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature_i)))%Q
    | 32%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature_i)))%Q
    | 33%positive => ((1 # 1) + (1 # 2) * (s IDcompressSignature__tmp)
                      - (1 # 2) * (s IDcompressSignature_i)
                      + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature__tmp)))%Q
    | 34%positive => ((1 # 1) + (1 # 2) * (s IDcompressSignature__tmp)
                      - (1 # 2) * (s IDcompressSignature_i)
                      + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature__tmp)))%Q
    | 35%positive => ((s IDcompressSignature_z))%Q
    | 36%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature_i)))%Q
    | 37%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(9 - (s IDcompressSignature_i)))%Q
    | 38%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 39%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 40%positive => ((1 # 1) + (s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | 41%positive => ((s IDcompressSignature_z)
                      + max0(10 - (s IDcompressSignature_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compressSignature_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => [(*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcompressSignature_i))) (F_check_ge (0) (0));
                     (*-5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompressSignature_i)) (0))) (F_max0_ge_0 ((s IDcompressSignature_i)));
                     (*-5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                  + (s IDcompressSignature_i))) (F_check_ge (2
                                                                    + (s IDcompressSignature_i)) (0))]
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*0 1*) F_max0_monotonic (F_check_ge (10
                                                            - (s IDcompressSignature_i)) (9
                                                                    - (s IDcompressSignature_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                 - (s IDcompressSignature__tmp))) (F_check_ge (0) (0))]
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (10
                                                             - (s IDcompressSignature_i)) (9
                                                                    - (s IDcompressSignature_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                 - (s IDcompressSignature_i))) (F_check_ge (0) (0))]
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
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_pre_decrement (10
                                                     - (s IDcompressSignature_i)) (1)]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (11
                                                                   - 
                                                                   (s IDcompressSignature_i))) (F_check_ge (0) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                                    - (s IDcompressSignature_i)) (0))) (F_max0_ge_0 (11
                                                                    - (s IDcompressSignature_i)));
                      (*0.5 1*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                  - (s IDcompressSignature__tmp))) (F_check_ge (0) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - 
                                                                    (s IDcompressSignature__tmp))) (F_check_ge (9
                                                                    - (s IDcompressSignature__tmp)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | _ => []
  end.


Theorem compressSignature_ai_correct:
  forall s p' s', steps (g_start compressSignature) s (g_edges compressSignature) p' s' -> compressSignature_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compressSignature_pot_correct:
  forall s p' s',
    steps (g_start compressSignature) s (g_edges compressSignature) p' s' ->
    (compressSignature_pot (g_start compressSignature) s >= compressSignature_pot p' s')%Q.
Proof.
  check_lp compressSignature_ai_correct compressSignature_hints.
Qed.

