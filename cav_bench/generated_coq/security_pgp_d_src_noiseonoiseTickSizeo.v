Require Import pasta.Pasta.

Notation IDnoiseTickSize_z := 1%positive.
Notation IDnoiseTickSize__tmp := 2%positive.
Notation IDnoiseTickSize_i := 3%positive.
Notation IDnoiseTickSize_j := 4%positive.
Notation IDnoiseTickSize_t := 5%positive.
Notation IDnoiseTickSize_verbose := 6%positive.
Definition noiseTickSize : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDnoiseTickSize_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDnoiseTickSize_j (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDnoiseTickSize_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,7%positive)::
             (6%positive,ANone,10%positive)::
             (7%positive,(AAssign IDnoiseTickSize_i
             (Some (EAdd (EVar IDnoiseTickSize_i) (ENum (1))))),8%positive)::
             (8%positive,(AAssign IDnoiseTickSize_j (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDnoiseTickSize_j
             (Some (EAdd (EVar IDnoiseTickSize_j) (ENum (1))))),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDnoiseTickSize_j)
             s) > (eval (ENum (10000)) s))%Z)),31%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDnoiseTickSize_j)
             s) <= (eval (ENum (10000)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDnoiseTickSize_i)
             s) < (eval (ENum (15)) s))%Z)),27%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDnoiseTickSize_i)
             s) >= (eval (ENum (15)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDnoiseTickSize_t None),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDnoiseTickSize_verbose) s) <>
             (eval (ENum (0)) s))%Z)),22%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDnoiseTickSize_verbose) s) =
             (eval (ENum (0)) s))%Z)),21%positive)::
             (21%positive,AWeaken,24%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDnoiseTickSize__tmp
             (Some (EVar IDnoiseTickSize_t))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,35%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDnoiseTickSize_z (Some (EAdd (ENum (1))
             (EVar IDnoiseTickSize_z)))),30%positive)::
             (30%positive,AWeaken,6%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AAssign IDnoiseTickSize__tmp (Some (ENum (2)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::nil
|}.

Definition noiseTickSize_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0)%Z
    | 3%positive => (-1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_j) <= 0)%Z
    | 4%positive => (-1 * (s IDnoiseTickSize_j) <= 0 /\ 1 * (s IDnoiseTickSize_j) <= 0 /\ 1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0)%Z
    | 5%positive => (-1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_j) <= 0)%Z
    | 6%positive => (-1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -14 <= 0)%Z
    | 7%positive => (1 * (s IDnoiseTickSize_i) + -14 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) <= 0)%Z
    | 8%positive => (-1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDnoiseTickSize_i) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_j) <= 0)%Z
    | 10%positive => (1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_j) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 11%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 13%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0)%Z
    | 14%positive => (1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 15%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0)%Z
    | 16%positive => (1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 17%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0)%Z
    | 18%positive => (-1 * (s IDnoiseTickSize_i) + 15 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 19%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0)%Z
    | 20%positive => (-1 * (s IDnoiseTickSize_i) + 15 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 21%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0 /\ 1 * (s IDnoiseTickSize_verbose) <= 0 /\ -1 * (s IDnoiseTickSize_verbose) <= 0)%Z
    | 22%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0)%Z
    | 23%positive => (-1 * (s IDnoiseTickSize_i) + 15 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 24%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0)%Z
    | 25%positive => (-1 * (s IDnoiseTickSize_i) + 15 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 26%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_i) + 15 <= 0)%Z
    | 27%positive => (-1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -14 <= 0)%Z
    | 28%positive => (1 * (s IDnoiseTickSize_i) + -14 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0)%Z
    | 29%positive => (-1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -14 <= 0)%Z
    | 30%positive => (1 * (s IDnoiseTickSize_i) + -14 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10000 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 10001 <= 0)%Z
    | 32%positive => (-1 * (s IDnoiseTickSize_j) + 10001 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 33%positive => (1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 10001 <= 0 /\ 1 * (s IDnoiseTickSize__tmp) + -2 <= 0 /\ -1 * (s IDnoiseTickSize__tmp) + 2 <= 0)%Z
    | 34%positive => (-1 * (s IDnoiseTickSize__tmp) + 2 <= 0 /\ 1 * (s IDnoiseTickSize__tmp) + -2 <= 0 /\ -1 * (s IDnoiseTickSize_j) + 10001 <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0)%Z
    | 35%positive => (-1 * (s IDnoiseTickSize_j) + 1 <= 0 /\ 1 * (s IDnoiseTickSize_i) + -15 <= 0 /\ -1 * (s IDnoiseTickSize_z) <= 0 /\ -1 * (s IDnoiseTickSize_i) <= 0 /\ 1 * (s IDnoiseTickSize_j) + -10001 <= 0)%Z
    | _ => False
  end.

Definition noiseTickSize_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (-(0 # 1))%Q
    | 2%positive => (-(0 # 1) + (s IDnoiseTickSize_z))%Q
    | 3%positive => (-(0 # 1) + (s IDnoiseTickSize_z))%Q
    | 4%positive => (-(0 # 1) + (s IDnoiseTickSize_z))%Q
    | 5%positive => (-(0 # 1) + (s IDnoiseTickSize_z))%Q
    | 6%positive => ((s IDnoiseTickSize_z))%Q
    | 7%positive => ((s IDnoiseTickSize_z))%Q
    | 8%positive => ((s IDnoiseTickSize_z))%Q
    | 9%positive => ((s IDnoiseTickSize_z))%Q
    | 10%positive => ((s IDnoiseTickSize_z))%Q
    | 11%positive => ((s IDnoiseTickSize_z))%Q
    | 12%positive => ((s IDnoiseTickSize_z))%Q
    | 13%positive => ((s IDnoiseTickSize_z))%Q
    | 14%positive => ((1 # 15) * (s IDnoiseTickSize_i) * max0((s IDnoiseTickSize_z))
                      + (s IDnoiseTickSize_z)
                      + (1 # 15) * max0(15 - (s IDnoiseTickSize_i)) * max0((s IDnoiseTickSize_z))
                      - max0((s IDnoiseTickSize_z)))%Q
    | 15%positive => ((1 # 15) * (s IDnoiseTickSize_i) * max0((s IDnoiseTickSize_z))
                      + (s IDnoiseTickSize_z)
                      + (1 # 15) * max0(15 - (s IDnoiseTickSize_i)) * max0((s IDnoiseTickSize_z))
                      - max0((s IDnoiseTickSize_z)))%Q
    | 16%positive => ((s IDnoiseTickSize_z))%Q
    | 17%positive => ((s IDnoiseTickSize_z))%Q
    | 18%positive => ((s IDnoiseTickSize_z))%Q
    | 19%positive => ((s IDnoiseTickSize_z))%Q
    | 20%positive => ((s IDnoiseTickSize_z))%Q
    | 21%positive => ((s IDnoiseTickSize_z))%Q
    | 22%positive => ((s IDnoiseTickSize_z))%Q
    | 23%positive => ((s IDnoiseTickSize_z))%Q
    | 24%positive => ((s IDnoiseTickSize_z))%Q
    | 25%positive => ((s IDnoiseTickSize_z))%Q
    | 26%positive => ((s IDnoiseTickSize_z))%Q
    | 27%positive => ((s IDnoiseTickSize_z))%Q
    | 28%positive => ((s IDnoiseTickSize_z))%Q
    | 29%positive => ((s IDnoiseTickSize_z))%Q
    | 30%positive => (-(1 # 1) + (s IDnoiseTickSize_z))%Q
    | 31%positive => ((s IDnoiseTickSize_z))%Q
    | 32%positive => ((s IDnoiseTickSize_z))%Q
    | 33%positive => ((s IDnoiseTickSize_z))%Q
    | 34%positive => ((s IDnoiseTickSize_z))%Q
    | 35%positive => ((s IDnoiseTickSize_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition noiseTickSize_hints (p : node) (s : state) := 
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
    | 13%positive => [(*0 0.0666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (15
                                                                    - (s IDnoiseTickSize_i)) (0))) (F_max0_ge_0 (15
                                                                    - (s IDnoiseTickSize_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDnoiseTickSize_z))) (F_check_ge (0) (0)))]
    | 14%positive => []
    | 15%positive => [(*-0.0666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (15
                                                                    - (s IDnoiseTickSize_i))) (F_check_ge (15
                                                                    - (s IDnoiseTickSize_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDnoiseTickSize_z))) (F_check_ge (0) (0)))]
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
    | 30%positive => [(*-0.00019999 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDnoiseTickSize_j))) (F_check_ge ((s IDnoiseTickSize_j)) (0))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem noiseTickSize_ai_correct:
  forall s p' s', steps (g_start noiseTickSize) s (g_edges noiseTickSize) p' s' -> noiseTickSize_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem noiseTickSize_pot_correct:
  forall s p' s',
    steps (g_start noiseTickSize) s (g_edges noiseTickSize) p' s' ->
    (noiseTickSize_pot (g_start noiseTickSize) s >= noiseTickSize_pot p' s')%Q.
Proof.
  check_lp noiseTickSize_ai_correct noiseTickSize_hints.
Qed.

