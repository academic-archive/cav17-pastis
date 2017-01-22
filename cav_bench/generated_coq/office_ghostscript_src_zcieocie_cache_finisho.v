Require Import pasta.Pasta.

Notation IDcie_cache_finish_z := 1%positive.
Notation IDcie_cache_finish__tmp := 2%positive.
Notation IDcie_cache_finish_code := 3%positive.
Notation IDcie_cache_finish_i := 4%positive.
Notation IDcie_cache_finish_op := 5%positive.
Definition cie_cache_finish : graph := {|
  g_start := 1%positive;
  g_end := 39%positive;
  g_edges := (1%positive,(AAssign IDcie_cache_finish_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDcie_cache_finish_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,ANone,36%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDcie_cache_finish_code None),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_code) s) <
             (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_code) s) >=
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,18%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDcie_cache_finish_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_i) s) <
             (eval (ENum (512)) s))%Z)),21%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_i) s) >=
             (eval (ENum (512)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDcie_cache_finish__tmp
             (Some (ENum (14)))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,39%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDcie_cache_finish_code None),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_code) s) <
             (eval (ENum (0)) s))%Z)),32%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_finish_code) s) >=
             (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDcie_cache_finish_i
             (Some (EAdd (EVar IDcie_cache_finish_i) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDcie_cache_finish_z
             (Some (EAdd (ENum (1)) (EVar IDcie_cache_finish_z)))),
             31%positive)::(31%positive,AWeaken,15%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDcie_cache_finish__tmp
             (Some (EVar IDcie_cache_finish_code))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,39%positive)::
             (36%positive,(AAssign IDcie_cache_finish__tmp
             (Some (ENum (-104)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::nil
|}.

Definition cie_cache_finish_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_code) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 13%positive => (1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -512 <= 0)%Z
    | 16%positive => (1 * (s IDcie_cache_finish_i) + -512 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) + 512 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_cache_finish_i) + 512 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -512 <= 0)%Z
    | 18%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish__tmp) + -14 <= 0 /\ -1 * (s IDcie_cache_finish__tmp) + 14 <= 0)%Z
    | 20%positive => (-1 * (s IDcie_cache_finish__tmp) + 14 <= 0 /\ 1 * (s IDcie_cache_finish__tmp) + -14 <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | 21%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0)%Z
    | 22%positive => (1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0)%Z
    | 24%positive => (1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 25%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_code) <= 0)%Z
    | 26%positive => (-1 * (s IDcie_cache_finish_code) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 27%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_code) <= 0)%Z
    | 28%positive => (-1 * (s IDcie_cache_finish_code) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_i) + -512 <= 0)%Z
    | 29%positive => (1 * (s IDcie_cache_finish_i) + -512 <= 0 /\ -1 * (s IDcie_cache_finish_i) + 1 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_code) <= 0)%Z
    | 30%positive => (-1 * (s IDcie_cache_finish_code) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_i) + -512 <= 0)%Z
    | 31%positive => (1 * (s IDcie_cache_finish_i) + -512 <= 0 /\ -1 * (s IDcie_cache_finish_i) + 1 <= 0 /\ -1 * (s IDcie_cache_finish_code) <= 0 /\ -1 * (s IDcie_cache_finish_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 34%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ 1 * (s IDcie_cache_finish__tmp) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDcie_cache_finish__tmp) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_code) + 1 <= 0 /\ 1 * (s IDcie_cache_finish_i) + -511 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 36%positive => (-1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 37%positive => (-1 * (s IDcie_cache_finish_i) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish__tmp) + 104 <= 0 /\ -1 * (s IDcie_cache_finish__tmp) + -104 <= 0)%Z
    | 38%positive => (-1 * (s IDcie_cache_finish__tmp) + -104 <= 0 /\ 1 * (s IDcie_cache_finish__tmp) + 104 <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0 /\ 1 * (s IDcie_cache_finish_z) <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0)%Z
    | 39%positive => (1 * (s IDcie_cache_finish__tmp) + -14 <= 0 /\ -1 * (s IDcie_cache_finish_i) <= 0 /\ -1 * (s IDcie_cache_finish_z) <= 0)%Z
    | _ => False
  end.

Definition cie_cache_finish_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1))%Q
    | 2%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 3%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 4%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 5%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 6%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 7%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 8%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 9%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 10%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 11%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 12%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 13%positive => (max0(512 - (s IDcie_cache_finish_i))
                      + max0((s IDcie_cache_finish_z)))%Q
    | 14%positive => (max0(512 - (s IDcie_cache_finish_i))
                      + max0((s IDcie_cache_finish_z)))%Q
    | 15%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 16%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 17%positive => (max0((s IDcie_cache_finish_z)))%Q
    | 18%positive => (max0((s IDcie_cache_finish_z)))%Q
    | 19%positive => (max0((s IDcie_cache_finish_z)))%Q
    | 20%positive => (max0((s IDcie_cache_finish_z)))%Q
    | 21%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 22%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 23%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 24%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 25%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 26%positive => ((1 # 1) + (s IDcie_cache_finish_z)
                      + max0(511 - (s IDcie_cache_finish_i)))%Q
    | 27%positive => ((1 # 1) + (s IDcie_cache_finish_z)
                      + max0(511 - (s IDcie_cache_finish_i)))%Q
    | 28%positive => ((1 # 1) + (s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 29%positive => ((1 # 1) + (s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 30%positive => ((1 # 1) + (s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 31%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 32%positive => ((s IDcie_cache_finish_z)
                      + max0(512 - (s IDcie_cache_finish_i)))%Q
    | 33%positive => ((s IDcie_cache_finish_z))%Q
    | 34%positive => ((s IDcie_cache_finish_z))%Q
    | 35%positive => ((s IDcie_cache_finish_z))%Q
    | 36%positive => ((512 # 1) + max0((s IDcie_cache_finish_z)))%Q
    | 37%positive => ((512 # 103) * max0(-1 - (s IDcie_cache_finish__tmp))
                      + max0((s IDcie_cache_finish_z)))%Q
    | 38%positive => ((512 # 103) * max0(-1 - (s IDcie_cache_finish__tmp))
                      + max0((s IDcie_cache_finish_z)))%Q
    | 39%positive => ((s IDcie_cache_finish_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_cache_finish_hints (p : node) (s : state) := 
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
    | 10%positive => [(*0 512*) F_one]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_cache_finish_z))) (F_check_ge ((s IDcie_cache_finish_z)) (0))]
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                             - (s IDcie_cache_finish_i)) (511
                                                                    - (s IDcie_cache_finish_i)));
                      (*-1 0*) F_max0_ge_0 (511 - (s IDcie_cache_finish_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcie_cache_finish_z)) (0))) (F_max0_ge_0 ((s IDcie_cache_finish_z)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_cache_finish_z))) (F_check_ge ((s IDcie_cache_finish_z)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (512
                                                     - (s IDcie_cache_finish_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (512
                                                                 - (s IDcie_cache_finish_i))) (F_check_ge (0) (0))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_cache_finish_z))) (F_check_ge ((s IDcie_cache_finish_z)) (0));
                      (*-4.97087 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDcie_cache_finish__tmp))) (F_check_ge (0) (0))]
    | 39%positive => []
    | _ => []
  end.


Theorem cie_cache_finish_ai_correct:
  forall s p' s', steps (g_start cie_cache_finish) s (g_edges cie_cache_finish) p' s' -> cie_cache_finish_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_cache_finish_pot_correct:
  forall s p' s',
    steps (g_start cie_cache_finish) s (g_edges cie_cache_finish) p' s' ->
    (cie_cache_finish_pot (g_start cie_cache_finish) s >= cie_cache_finish_pot p' s')%Q.
Proof.
  check_lp cie_cache_finish_ai_correct cie_cache_finish_hints.
Qed.

