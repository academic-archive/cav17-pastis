Require Import pasta.Pasta.

Notation IDread_samples_mp3_z := 1%positive.
Notation IDread_samples_mp3__tmp := 2%positive.
Notation IDread_samples_mp3__tmp1 := 3%positive.
Notation IDread_samples_mp3_j := 4%positive.
Notation IDread_samples_mp3_out := 5%positive.
Notation IDread_samples_mp3_gfp := 6%positive.
Notation IDread_samples_mp3_mpg123pcm := 7%positive.
Notation IDread_samples_mp3_musicin := 8%positive.
Notation IDread_samples_mp3_stereo := 9%positive.
Definition read_samples_mp3 : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDread_samples_mp3_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_samples_mp3__tmp1
             (Some (EVar IDread_samples_mp3_stereo))),3%positive)::
             (3%positive,(AAssign IDread_samples_mp3_out (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDread_samples_mp3_out None),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_out) s) =
             (eval (ENum (-1)) s))%Z)),8%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_out) s) <>
             (eval (ENum (-1)) s))%Z)),7%positive)::
             (7%positive,AWeaken,16%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDread_samples_mp3_j (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_j) s) <
             (eval (ENum (1152)) s))%Z)),26%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_j) s) >=
             (eval (ENum (1152)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_out) s) =
             (eval (ENum (-1)) s))%Z)),21%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_mp3_out) s) <>
             (eval (ENum (-1)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDread_samples_mp3__tmp
             (Some (EVar IDread_samples_mp3_out))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,25%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDread_samples_mp3__tmp
             (Some (ENum (0)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDread_samples_mp3_j
             (Some (EAdd (EVar IDread_samples_mp3_j) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDread_samples_mp3_z
             (Some (EAdd (ENum (1)) (EVar IDread_samples_mp3_z)))),
             32%positive)::(32%positive,AWeaken,12%positive)::nil
|}.

Definition read_samples_mp3_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) <= 0 /\ -1 * (s IDread_samples_mp3_out) <= 0)%Z
    | 5%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 6%positive => (1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 7%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 8%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0)%Z
    | 9%positive => (-1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 10%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_j) <= 0 /\ -1 * (s IDread_samples_mp3_j) <= 0)%Z
    | 11%positive => (-1 * (s IDread_samples_mp3_j) <= 0 /\ 1 * (s IDread_samples_mp3_j) <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ 1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 12%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_j) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1152 <= 0)%Z
    | 13%positive => (1 * (s IDread_samples_mp3_j) + -1152 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1152 <= 0)%Z
    | 14%positive => (-1 * (s IDread_samples_mp3_j) + 1152 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1152 <= 0)%Z
    | 15%positive => (1 * (s IDread_samples_mp3_j) + -1152 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1152 <= 0)%Z
    | 16%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 17%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 18%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 19%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 20%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 21%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 23%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3__tmp) <= 0 /\ -1 * (s IDread_samples_mp3__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDread_samples_mp3__tmp) <= 0 /\ 1 * (s IDread_samples_mp3__tmp) <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 25%positive => (-1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 26%positive => (-1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_j) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1151 <= 0)%Z
    | 27%positive => (1 * (s IDread_samples_mp3_j) + -1151 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ -1 * (s IDread_samples_mp3_j) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0)%Z
    | 28%positive => (-1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_j) <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1151 <= 0)%Z
    | 29%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1 <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1152 <= 0)%Z
    | 30%positive => (1 * (s IDread_samples_mp3_j) + -1152 <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) <= 0)%Z
    | 31%positive => (-1 * (s IDread_samples_mp3_z) <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1 <= 0 /\ 1 * (s IDread_samples_mp3_j) + -1152 <= 0)%Z
    | 32%positive => (1 * (s IDread_samples_mp3_j) + -1152 <= 0 /\ -1 * (s IDread_samples_mp3_j) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_out) + -1 <= 0 /\ 1 * (s IDread_samples_mp3_out) + 1 <= 0 /\ -1 * (s IDread_samples_mp3_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition read_samples_mp3_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1152 # 1))%Q
    | 2%positive => ((1152 # 1))%Q
    | 3%positive => ((1152 # 1))%Q
    | 4%positive => ((1152 # 1))%Q
    | 5%positive => ((1152 # 1))%Q
    | 6%positive => ((1152 # 1))%Q
    | 7%positive => ((1152 # 1))%Q
    | 8%positive => ((1152 # 1))%Q
    | 9%positive => ((1152 # 1))%Q
    | 10%positive => ((1152 # 1) - (s IDread_samples_mp3_j))%Q
    | 11%positive => ((1152 # 1) - (s IDread_samples_mp3_j))%Q
    | 12%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 13%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 14%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 15%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 16%positive => ((s IDread_samples_mp3_z))%Q
    | 17%positive => ((s IDread_samples_mp3_z))%Q
    | 18%positive => ((s IDread_samples_mp3_z))%Q
    | 19%positive => ((s IDread_samples_mp3_z))%Q
    | 20%positive => ((s IDread_samples_mp3_z))%Q
    | 21%positive => ((s IDread_samples_mp3_z))%Q
    | 22%positive => ((s IDread_samples_mp3_z))%Q
    | 23%positive => ((s IDread_samples_mp3_z))%Q
    | 24%positive => ((s IDread_samples_mp3_z))%Q
    | 25%positive => ((s IDread_samples_mp3_z))%Q
    | 26%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 27%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 28%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 29%positive => ((1153 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 30%positive => ((1153 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 31%positive => ((1153 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | 32%positive => ((1152 # 1) - (s IDread_samples_mp3_j)
                      + (s IDread_samples_mp3_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_samples_mp3_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1152 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_mp3_z))) (F_check_ge (0) (0));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_mp3_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_mp3_z)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_mp3_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_mp3_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_mp3_z)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1152
                                                             - (s IDread_samples_mp3_j)) (1151
                                                                    - (s IDread_samples_mp3_j)));
                      (*-1 0*) F_max0_ge_0 (1151 - (s IDread_samples_mp3_j));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1152
                                                                    - (s IDread_samples_mp3_j)) (0))) (F_max0_ge_0 (1152
                                                                    - (s IDread_samples_mp3_j)))]
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
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | _ => []
  end.


Theorem read_samples_mp3_ai_correct:
  forall s p' s', steps (g_start read_samples_mp3) s (g_edges read_samples_mp3) p' s' -> read_samples_mp3_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_samples_mp3_pot_correct:
  forall s p' s',
    steps (g_start read_samples_mp3) s (g_edges read_samples_mp3) p' s' ->
    (read_samples_mp3_pot (g_start read_samples_mp3) s >= read_samples_mp3_pot p' s')%Q.
Proof.
  check_lp read_samples_mp3_ai_correct read_samples_mp3_hints.
Qed.

