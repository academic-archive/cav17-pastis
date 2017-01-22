Require Import pasta.Pasta.

Notation IDjpeg_suppress_tables_z := 1%positive.
Notation IDjpeg_suppress_tables__tmp := 2%positive.
Notation IDjpeg_suppress_tables_i := 3%positive.
Notation IDjpeg_suppress_tables_cinfo := 4%positive.
Notation IDjpeg_suppress_tables_suppress := 5%positive.
Definition jpeg_suppress_tables : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDjpeg_suppress_tables_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDjpeg_suppress_tables__tmp
             (Some (EVar IDjpeg_suppress_tables_suppress))),3%positive)::
             (3%positive,(AAssign IDjpeg_suppress_tables_i
             (Some (ENum (0)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_suppress_tables_i) s) <
             (eval (ENum (4)) s))%Z)),27%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_suppress_tables_i) s) >=
             (eval (ENum (4)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDjpeg_suppress_tables_i
             (Some (ENum (0)))),9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_suppress_tables_i) s) <
             (eval (ENum (4)) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_suppress_tables_i) s) >=
             (eval (ENum (4)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,17%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,19%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (19%positive,ANone,21%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDjpeg_suppress_tables_i
             (Some (EAdd (EVar IDjpeg_suppress_tables_i) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDjpeg_suppress_tables_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_suppress_tables_z)))),
             26%positive)::(26%positive,AWeaken,11%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (28%positive,ANone,30%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDjpeg_suppress_tables_i
             (Some (EAdd (EVar IDjpeg_suppress_tables_i) (ENum (1))))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDjpeg_suppress_tables_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_suppress_tables_z)))),
             35%positive)::(35%positive,AWeaken,6%positive)::nil
|}.

Definition jpeg_suppress_tables_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 4 <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_suppress_tables_i) + 4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_suppress_tables_i) + 4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 18%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 20%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 21%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 22%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 23%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 25%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 29%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 30%positive => (1 * (s IDjpeg_suppress_tables_i) + -3 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) <= 0)%Z
    | 31%positive => (-1 * (s IDjpeg_suppress_tables_i) <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -3 <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) <= 0)%Z
    | 34%positive => (-1 * (s IDjpeg_suppress_tables_z) <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ 1 * (s IDjpeg_suppress_tables_i) + -4 <= 0)%Z
    | 35%positive => (1 * (s IDjpeg_suppress_tables_i) + -4 <= 0 /\ -1 * (s IDjpeg_suppress_tables_i) + 1 <= 0 /\ -1 * (s IDjpeg_suppress_tables_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_suppress_tables_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDjpeg_suppress_tables_z))%Q
    | 3%positive => ((8 # 1) + (s IDjpeg_suppress_tables_z))%Q
    | 4%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                     + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 5%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                     + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 6%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                     + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 7%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                     + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 8%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z))%Q
    | 9%positive => ((s IDjpeg_suppress_tables_z)
                     + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 10%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 11%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 12%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 13%positive => ((s IDjpeg_suppress_tables_z))%Q
    | 14%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 15%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 16%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 17%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 18%positive => ((s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 19%positive => ((4 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 20%positive => ((4 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 21%positive => ((4 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 22%positive => ((4 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 23%positive => ((5 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 24%positive => ((5 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 25%positive => ((5 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 26%positive => ((4 # 1) - (s IDjpeg_suppress_tables_i)
                      + (s IDjpeg_suppress_tables_z))%Q
    | 27%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 28%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(3 - (s IDjpeg_suppress_tables_i)))%Q
    | 29%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(3 - (s IDjpeg_suppress_tables_i)))%Q
    | 30%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(3 - (s IDjpeg_suppress_tables_i)))%Q
    | 31%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(3 - (s IDjpeg_suppress_tables_i)))%Q
    | 32%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 33%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 34%positive => ((5 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | 35%positive => ((4 # 1) + (s IDjpeg_suppress_tables_z)
                      + max0(4 - (s IDjpeg_suppress_tables_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_suppress_tables_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDjpeg_suppress_tables_i)) (3
                                                                    - (s IDjpeg_suppress_tables_i)));
                     (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                               - (s IDjpeg_suppress_tables_i))) (F_check_ge (0) (0))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDjpeg_suppress_tables_i)) (3
                                                                    - (s IDjpeg_suppress_tables_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDjpeg_suppress_tables_i))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDjpeg_suppress_tables_i))) (F_check_ge (4
                                                                    - (s IDjpeg_suppress_tables_i)) (0))]
    | 17%positive => []
    | 18%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                  - (s IDjpeg_suppress_tables_i))) (F_check_ge (4
                                                                    - (s IDjpeg_suppress_tables_i)) (0))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDjpeg_suppress_tables_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDjpeg_suppress_tables_i)))]
    | 27%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDjpeg_suppress_tables_i)) (1)]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem jpeg_suppress_tables_ai_correct:
  forall s p' s', steps (g_start jpeg_suppress_tables) s (g_edges jpeg_suppress_tables) p' s' -> jpeg_suppress_tables_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_suppress_tables_pot_correct:
  forall s p' s',
    steps (g_start jpeg_suppress_tables) s (g_edges jpeg_suppress_tables) p' s' ->
    (jpeg_suppress_tables_pot (g_start jpeg_suppress_tables) s >= jpeg_suppress_tables_pot p' s')%Q.
Proof.
  check_lp jpeg_suppress_tables_ai_correct jpeg_suppress_tables_hints.
Qed.

