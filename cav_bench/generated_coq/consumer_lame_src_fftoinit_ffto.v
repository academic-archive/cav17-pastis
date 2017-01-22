Require Import pasta.Pasta.

Notation IDinit_fft_z := 1%positive.
Notation IDinit_fft_i := 2%positive.
Definition init_fft : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDinit_fft_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDinit_fft_i (Some (ENum (0)))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) <
             (eval (ENum (4)) s))%Z)),32%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) >=
             (eval (ENum (4)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDinit_fft_i (Some (ENum (0)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) <
             (eval (ENum (512)) s))%Z)),25%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) >=
             (eval (ENum (512)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDinit_fft_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) <
             (eval (ENum (128)) s))%Z)),18%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinit_fft_i) s) >=
             (eval (ENum (128)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDinit_fft_i
             (Some (EAdd (EVar IDinit_fft_i) (ENum (1))))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDinit_fft_z (Some (EAdd (ENum (1))
             (EVar IDinit_fft_z)))),24%positive)::
             (24%positive,AWeaken,15%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDinit_fft_i
             (Some (EAdd (EVar IDinit_fft_i) (ENum (1))))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDinit_fft_z (Some (EAdd (ENum (1))
             (EVar IDinit_fft_z)))),31%positive)::
             (31%positive,AWeaken,10%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDinit_fft_i
             (Some (EAdd (EVar IDinit_fft_i) (ENum (1))))),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDinit_fft_z (Some (EAdd (ENum (1))
             (EVar IDinit_fft_z)))),38%positive)::
             (38%positive,AWeaken,5%positive)::nil
|}.

Definition init_fft_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 4%positive => (-1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 5%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) + -4 <= 0)%Z
    | 6%positive => (1 * (s IDinit_fft_i) + -4 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 4 <= 0)%Z
    | 7%positive => (-1 * (s IDinit_fft_i) + 4 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -4 <= 0)%Z
    | 8%positive => (-1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 9%positive => (-1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 10%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) + -512 <= 0)%Z
    | 11%positive => (1 * (s IDinit_fft_i) + -512 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 512 <= 0)%Z
    | 12%positive => (-1 * (s IDinit_fft_i) + 512 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -512 <= 0)%Z
    | 13%positive => (-1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 14%positive => (-1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 15%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0 /\ 1 * (s IDinit_fft_i) + -128 <= 0)%Z
    | 16%positive => (1 * (s IDinit_fft_i) + -128 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 128 <= 0)%Z
    | 17%positive => (-1 * (s IDinit_fft_i) + 128 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -128 <= 0)%Z
    | 18%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -127 <= 0)%Z
    | 19%positive => (1 * (s IDinit_fft_i) + -127 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 20%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -127 <= 0)%Z
    | 21%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -128 <= 0)%Z
    | 22%positive => (1 * (s IDinit_fft_i) + -128 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 23%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -128 <= 0)%Z
    | 24%positive => (1 * (s IDinit_fft_i) + -128 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -511 <= 0)%Z
    | 26%positive => (1 * (s IDinit_fft_i) + -511 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 27%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -511 <= 0)%Z
    | 28%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -512 <= 0)%Z
    | 29%positive => (1 * (s IDinit_fft_i) + -512 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 30%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -512 <= 0)%Z
    | 31%positive => (1 * (s IDinit_fft_i) + -512 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -3 <= 0)%Z
    | 33%positive => (1 * (s IDinit_fft_i) + -3 <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) <= 0)%Z
    | 34%positive => (-1 * (s IDinit_fft_i) <= 0 /\ -1 * (s IDinit_fft_z) <= 0 /\ 1 * (s IDinit_fft_i) + -3 <= 0)%Z
    | 35%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDinit_fft_i) + -4 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) <= 0)%Z
    | 37%positive => (-1 * (s IDinit_fft_z) <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ 1 * (s IDinit_fft_i) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDinit_fft_i) + -4 <= 0 /\ -1 * (s IDinit_fft_i) + 1 <= 0 /\ -1 * (s IDinit_fft_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition init_fft_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((644 # 1))%Q
    | 2%positive => ((644 # 1) + (s IDinit_fft_z))%Q
    | 3%positive => ((640 # 1) + (s IDinit_fft_z)
                     + max0(4 - (s IDinit_fft_i)))%Q
    | 4%positive => ((640 # 1) + (s IDinit_fft_z)
                     + max0(4 - (s IDinit_fft_i)))%Q
    | 5%positive => ((640 # 1) + (s IDinit_fft_z)
                     + max0(4 - (s IDinit_fft_i)))%Q
    | 6%positive => ((640 # 1) + (s IDinit_fft_z)
                     + max0(4 - (s IDinit_fft_i)))%Q
    | 7%positive => ((640 # 1) + (s IDinit_fft_z))%Q
    | 8%positive => ((128 # 1) + (s IDinit_fft_z)
                     + max0(512 - (s IDinit_fft_i)))%Q
    | 9%positive => ((128 # 1) + (s IDinit_fft_z)
                     + max0(512 - (s IDinit_fft_i)))%Q
    | 10%positive => ((128 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 11%positive => ((128 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 12%positive => ((128 # 1) + (s IDinit_fft_z))%Q
    | 13%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 14%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 15%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 16%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 17%positive => ((s IDinit_fft_z))%Q
    | 18%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 19%positive => ((1 # 1) + (s IDinit_fft_z)
                      + max0(127 - (s IDinit_fft_i)))%Q
    | 20%positive => ((1 # 1) + (s IDinit_fft_z)
                      + max0(127 - (s IDinit_fft_i)))%Q
    | 21%positive => ((1 # 1) + (s IDinit_fft_z)
                      + max0(128 - (s IDinit_fft_i)))%Q
    | 22%positive => ((1 # 1) + (s IDinit_fft_z)
                      + max0(128 - (s IDinit_fft_i)))%Q
    | 23%positive => ((1 # 1) + (s IDinit_fft_z)
                      + max0(128 - (s IDinit_fft_i)))%Q
    | 24%positive => ((s IDinit_fft_z) + max0(128 - (s IDinit_fft_i)))%Q
    | 25%positive => ((128 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 26%positive => ((129 # 1) + (s IDinit_fft_z)
                      + max0(511 - (s IDinit_fft_i)))%Q
    | 27%positive => ((129 # 1) + (s IDinit_fft_z)
                      + max0(511 - (s IDinit_fft_i)))%Q
    | 28%positive => ((129 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 29%positive => ((129 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 30%positive => ((129 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 31%positive => ((128 # 1) + (s IDinit_fft_z)
                      + max0(512 - (s IDinit_fft_i)))%Q
    | 32%positive => ((640 # 1) + (s IDinit_fft_z)
                      + max0(4 - (s IDinit_fft_i)))%Q
    | 33%positive => ((641 # 1) + (s IDinit_fft_z)
                      + max0(3 - (s IDinit_fft_i)))%Q
    | 34%positive => ((641 # 1) + (s IDinit_fft_z)
                      + max0(3 - (s IDinit_fft_i)))%Q
    | 35%positive => ((641 # 1) + (s IDinit_fft_z)
                      + max0(4 - (s IDinit_fft_i)))%Q
    | 36%positive => ((641 # 1) + (s IDinit_fft_z)
                      + max0(4 - (s IDinit_fft_i)))%Q
    | 37%positive => ((641 # 1) + (s IDinit_fft_z)
                      + max0(4 - (s IDinit_fft_i)))%Q
    | 38%positive => ((640 # 1) + (s IDinit_fft_z)
                      + max0(4 - (s IDinit_fft_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition init_fft_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDinit_fft_i)) (3
                                                                    - (s IDinit_fft_i)));
                     (*-1 0*) F_max0_ge_0 (3 - (s IDinit_fft_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                             - (s IDinit_fft_i)) (511
                                                                    - (s IDinit_fft_i)));
                      (*-1 0*) F_max0_ge_0 (511 - (s IDinit_fft_i))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (128
                                                             - (s IDinit_fft_i)) (127
                                                                    - (s IDinit_fft_i)));
                      (*-1 0*) F_max0_ge_0 (127 - (s IDinit_fft_i))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_pre_decrement (128 - (s IDinit_fft_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (512 - (s IDinit_fft_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_max0_pre_decrement (4 - (s IDinit_fft_i)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem init_fft_ai_correct:
  forall s p' s', steps (g_start init_fft) s (g_edges init_fft) p' s' -> init_fft_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem init_fft_pot_correct:
  forall s p' s',
    steps (g_start init_fft) s (g_edges init_fft) p' s' ->
    (init_fft_pot (g_start init_fft) s >= init_fft_pot p' s')%Q.
Proof.
  check_lp init_fft_ai_correct init_fft_hints.
Qed.

