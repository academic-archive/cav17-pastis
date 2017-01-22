Require Import pasta.Pasta.

Notation IDaudio_play_z := 1%positive.
Notation IDaudio_play__tmp := 2%positive.
Notation IDaudio_play_dev_fd := 3%positive.
Notation IDaudio_play_i := 4%positive.
Notation IDaudio_play_linear_fd := 5%positive.
Notation IDaudio_play_data := 6%positive.
Notation IDaudio_play_n := 7%positive.
Definition audio_play : graph := {|
  g_start := 1%positive;
  g_end := 37%positive;
  g_edges := (1%positive,(AAssign IDaudio_play_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDaudio_play__tmp
             (Some (EVar IDaudio_play_n))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDaudio_play__tmp)
             s) > (eval (ENum (0)) s))%Z)),6%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDaudio_play__tmp)
             s) <= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,27%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,36%positive)::(7%positive,ANone,8%positive)::
             (8%positive,(AAssign IDaudio_play_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDaudio_play_i) s) <
             (eval (EVar IDaudio_play__tmp) s))%Z)),29%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDaudio_play_i)
             s) >= (eval (EVar IDaudio_play__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDaudio_play_linear_fd) s) >=
             (eval (ENum (0)) s))%Z)),15%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDaudio_play_linear_fd) s) <
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,20%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,18%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDaudio_play_dev_fd)
             s) >= (eval (ENum (0)) s))%Z)),22%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDaudio_play_dev_fd)
             s) < (eval (ENum (0)) s))%Z)),21%positive)::
             (21%positive,AWeaken,26%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (23%positive,ANone,25%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,37%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDaudio_play_i
             (Some (EAdd (EVar IDaudio_play_i) (ENum (1))))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDaudio_play_z (Some (EAdd (ENum (1))
             (EVar IDaudio_play_z)))),35%positive)::
             (35%positive,AWeaken,11%positive)::
             (36%positive,AWeaken,37%positive)::nil
|}.

Definition audio_play_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_z) <= 0)%Z
    | 3%positive => (-1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0)%Z
    | 4%positive => (1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_z) <= 0)%Z
    | 5%positive => (-1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0)%Z
    | 7%positive => (-1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_z) <= 0)%Z
    | 8%positive => (-1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_i) <= 0)%Z
    | 10%positive => (-1 * (s IDaudio_play_i) <= 0 /\ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 12%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0)%Z
    | 13%positive => (1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 14%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ 1 * (s IDaudio_play_linear_fd) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_linear_fd) <= 0)%Z
    | 16%positive => (-1 * (s IDaudio_play_linear_fd) <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 17%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_linear_fd) <= 0)%Z
    | 18%positive => (-1 * (s IDaudio_play_linear_fd) <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 19%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_linear_fd) <= 0)%Z
    | 20%positive => (1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 21%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ 1 * (s IDaudio_play_dev_fd) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_dev_fd) <= 0)%Z
    | 23%positive => (-1 * (s IDaudio_play_dev_fd) <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 24%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_dev_fd) <= 0)%Z
    | 25%positive => (-1 * (s IDaudio_play_dev_fd) <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 26%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ 1 * (s IDaudio_play__tmp)+ -1 * (s IDaudio_play_i) <= 0)%Z
    | 27%positive => (-1 * (s IDaudio_play_z) <= 0)%Z
    | 28%positive => (-1 * (s IDaudio_play_z) <= 0)%Z
    | 29%positive => (-1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) + 1 <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_i) <= 0)%Z
    | 31%positive => (-1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_i) + 1 <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 33%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_i) + 1 <= 0 /\ -1 * (s IDaudio_play_z) <= 0)%Z
    | 34%positive => (-1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play_i) + 1 <= 0 /\ -1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0)%Z
    | 35%positive => (-1 * (s IDaudio_play__tmp)+ 1 * (s IDaudio_play_i) <= 0 /\ -1 * (s IDaudio_play_i) + 1 <= 0 /\ -1 * (s IDaudio_play_z) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDaudio_play_z) <= 0 /\ 1 * (s IDaudio_play_z) <= 0 /\ -1 * (s IDaudio_play__tmp) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDaudio_play_z) <= 0)%Z
    | _ => False
  end.

Definition audio_play_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDaudio_play_n)))%Q
    | 2%positive => ((s IDaudio_play_z) + max0((s IDaudio_play_n)))%Q
    | 3%positive => ((s IDaudio_play_z) + max0((s IDaudio_play__tmp)))%Q
    | 4%positive => ((s IDaudio_play_z) + max0((s IDaudio_play__tmp)))%Q
    | 5%positive => ((s IDaudio_play_z) + max0((s IDaudio_play__tmp)))%Q
    | 6%positive => ((s IDaudio_play_z) + max0((s IDaudio_play__tmp)))%Q
    | 7%positive => ((s IDaudio_play__tmp) + (s IDaudio_play_z))%Q
    | 8%positive => ((s IDaudio_play__tmp) + (s IDaudio_play_z))%Q
    | 9%positive => ((s IDaudio_play__tmp) + (s IDaudio_play_z)
                     - max0((s IDaudio_play__tmp))
                     + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 10%positive => ((s IDaudio_play__tmp) + (s IDaudio_play_z)
                      - max0((s IDaudio_play__tmp))
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 11%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 12%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 13%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 14%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 15%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 16%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 17%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 18%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 19%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 20%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 21%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 22%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 23%positive => ((s IDaudio_play_z))%Q
    | 24%positive => ((s IDaudio_play_z))%Q
    | 25%positive => ((s IDaudio_play_z))%Q
    | 26%positive => ((s IDaudio_play_z))%Q
    | 27%positive => ((s IDaudio_play_z))%Q
    | 28%positive => ((s IDaudio_play_z))%Q
    | 29%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 30%positive => ((1 # 1) + (s IDaudio_play_z)
                      + max0(-1 + (s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 31%positive => ((1 # 1) + (s IDaudio_play_z)
                      + max0(-1 + (s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 32%positive => ((1 # 1) + (s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 33%positive => ((1 # 1) + (s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 34%positive => ((1 # 1) + (s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 35%positive => ((s IDaudio_play_z)
                      + max0((s IDaudio_play__tmp) - (s IDaudio_play_i)))%Q
    | 36%positive => ((s IDaudio_play__tmp) + (s IDaudio_play_z))%Q
    | 37%positive => ((s IDaudio_play_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition audio_play_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*-1 0*) F_max0_ge_0 ((s IDaudio_play__tmp))]
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDaudio_play__tmp))) (F_check_ge ((s IDaudio_play__tmp)) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDaudio_play__tmp)) (0))) (F_max0_ge_0 ((s IDaudio_play__tmp)))]
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
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDaudio_play__tmp)
                                                             - (s IDaudio_play_i)) (-1
                                                                    + (s IDaudio_play__tmp)
                                                                    - (s IDaudio_play_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDaudio_play__tmp)
                                            - (s IDaudio_play_i))]
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDaudio_play__tmp)
                                                             - (s IDaudio_play_i)) (-1
                                                                    + (s IDaudio_play__tmp)
                                                                    - (s IDaudio_play_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDaudio_play__tmp)
                                            - (s IDaudio_play_i))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_pre_decrement ((s IDaudio_play__tmp)
                                                     - (s IDaudio_play_i)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDaudio_play__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDaudio_play__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDaudio_play__tmp)))]
    | 37%positive => []
    | _ => []
  end.


Theorem audio_play_ai_correct:
  forall s p' s', steps (g_start audio_play) s (g_edges audio_play) p' s' -> audio_play_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem audio_play_pot_correct:
  forall s p' s',
    steps (g_start audio_play) s (g_edges audio_play) p' s' ->
    (audio_play_pot (g_start audio_play) s >= audio_play_pot p' s')%Q.
Proof.
  check_lp audio_play_ai_correct audio_play_hints.
Qed.

