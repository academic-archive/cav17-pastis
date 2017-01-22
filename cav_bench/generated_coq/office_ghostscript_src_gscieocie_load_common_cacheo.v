Require Import pasta.Pasta.

Notation IDcie_load_common_cache_z := 1%positive.
Notation IDcie_load_common_cache__tmp := 2%positive.
Notation IDcie_load_common_cache_i := 3%positive.
Notation IDcie_load_common_cache_j := 4%positive.
Notation IDcie_load_common_cache_cname := 5%positive.
Notation IDcie_load_common_cache_pcie := 6%positive.
Notation IDcie_load_common_cache_pgs := 7%positive.
Definition cie_load_common_cache : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDcie_load_common_cache_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcie_load_common_cache_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_load_common_cache_i) s) <
             (eval (ENum (512)) s))%Z)),20%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcie_load_common_cache_i) s) >=
             (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,16%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,13%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDcie_load_common_cache__tmp
             (Some (ENum (0)))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,19%positive)::
             (13%positive,(AAssign IDcie_load_common_cache__tmp
             (Some (ENum (-25)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,19%positive)::
             (16%positive,(AAssign IDcie_load_common_cache__tmp
             (Some (ENum (0)))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDcie_load_common_cache_j
             (Some (ENum (0)))),22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDcie_load_common_cache_j) s) <
             (eval (ENum (3)) s))%Z)),32%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDcie_load_common_cache_j) s) >=
             (eval (ENum (3)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDcie_load_common_cache_i
             (Some (EAdd (EVar IDcie_load_common_cache_i) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDcie_load_common_cache_z
             (Some (EAdd (ENum (1)) (EVar IDcie_load_common_cache_z)))),
             31%positive)::(31%positive,AWeaken,5%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDcie_load_common_cache_j
             (Some (EAdd (EVar IDcie_load_common_cache_j) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDcie_load_common_cache_z
             (Some (EAdd (ENum (1)) (EVar IDcie_load_common_cache_z)))),
             38%positive)::(38%positive,AWeaken,24%positive)::nil
|}.

Definition cie_load_common_cache_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_load_common_cache_i) <= 0 /\ 1 * (s IDcie_load_common_cache_i) <= 0 /\ 1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 9%positive => (-1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 11%positive => (-1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) <= 0 /\ -1 * (s IDcie_load_common_cache__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_load_common_cache__tmp) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 13%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 14%positive => (-1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) + 25 <= 0 /\ -1 * (s IDcie_load_common_cache__tmp) + -25 <= 0)%Z
    | 15%positive => (-1 * (s IDcie_load_common_cache__tmp) + -25 <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) + 25 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 16%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) <= 0 /\ -1 * (s IDcie_load_common_cache__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDcie_load_common_cache__tmp) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0)%Z
    | 19%positive => (-1 * (s IDcie_load_common_cache__tmp) + -25 <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 512 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_i) + -511 <= 0)%Z
    | 21%positive => (1 * (s IDcie_load_common_cache_i) + -511 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0)%Z
    | 22%positive => (-1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_i) + -511 <= 0 /\ 1 * (s IDcie_load_common_cache_j) <= 0 /\ -1 * (s IDcie_load_common_cache_j) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_load_common_cache_j) <= 0 /\ 1 * (s IDcie_load_common_cache_j) <= 0 /\ 1 * (s IDcie_load_common_cache_i) + -511 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0)%Z
    | 24%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_j) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0)%Z
    | 25%positive => (1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 3 <= 0)%Z
    | 26%positive => (-1 * (s IDcie_load_common_cache_j) + 3 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0)%Z
    | 27%positive => (1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 3 <= 0)%Z
    | 28%positive => (-1 * (s IDcie_load_common_cache_j) + 3 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDcie_load_common_cache_i) + 1 <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 3 <= 0)%Z
    | 30%positive => (-1 * (s IDcie_load_common_cache_j) + 3 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDcie_load_common_cache_i) + 1 <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 3 <= 0 /\ -1 * (s IDcie_load_common_cache_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_j) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -2 <= 0)%Z
    | 33%positive => (1 * (s IDcie_load_common_cache_j) + -2 <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_j) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0)%Z
    | 34%positive => (-1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_j) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -2 <= 0)%Z
    | 35%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 1 <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0)%Z
    | 36%positive => (1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 1 <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) <= 0)%Z
    | 37%positive => (-1 * (s IDcie_load_common_cache_z) <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 1 <= 0 /\ 1 * (s IDcie_load_common_cache_j) + -3 <= 0)%Z
    | 38%positive => (1 * (s IDcie_load_common_cache_j) + -3 <= 0 /\ -1 * (s IDcie_load_common_cache_j) + 1 <= 0 /\ -1 * (s IDcie_load_common_cache_i) <= 0 /\ -1 * (s IDcie_load_common_cache_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cie_load_common_cache_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2048 # 1))%Q
    | 2%positive => ((2048 # 1) + (s IDcie_load_common_cache_z))%Q
    | 3%positive => ((s IDcie_load_common_cache_z)
                     + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 4%positive => ((s IDcie_load_common_cache_z)
                     + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 5%positive => ((s IDcie_load_common_cache_z)
                     + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 6%positive => ((s IDcie_load_common_cache_z)
                     + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 7%positive => ((s IDcie_load_common_cache_z))%Q
    | 8%positive => ((s IDcie_load_common_cache_z))%Q
    | 9%positive => ((s IDcie_load_common_cache_z))%Q
    | 10%positive => ((s IDcie_load_common_cache_z))%Q
    | 11%positive => ((s IDcie_load_common_cache_z))%Q
    | 12%positive => ((s IDcie_load_common_cache_z))%Q
    | 13%positive => ((s IDcie_load_common_cache_z))%Q
    | 14%positive => ((s IDcie_load_common_cache_z))%Q
    | 15%positive => ((s IDcie_load_common_cache_z))%Q
    | 16%positive => ((s IDcie_load_common_cache_z))%Q
    | 17%positive => ((s IDcie_load_common_cache_z))%Q
    | 18%positive => ((s IDcie_load_common_cache_z))%Q
    | 19%positive => ((s IDcie_load_common_cache_z))%Q
    | 20%positive => ((s IDcie_load_common_cache_z)
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 21%positive => ((s IDcie_load_common_cache_z)
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 22%positive => (-(3 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 23%positive => (-(3 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 24%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 25%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 26%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 27%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 28%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 29%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 30%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 31%positive => ((s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(512 - (s IDcie_load_common_cache_i)))%Q
    | 32%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 33%positive => ((2 # 1) + (s IDcie_load_common_cache_z)
                      + max0(2 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 34%positive => ((2 # 1) + (s IDcie_load_common_cache_z)
                      + max0(2 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 35%positive => ((2 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 36%positive => ((2 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 37%positive => ((2 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | 38%positive => ((1 # 1) + (s IDcie_load_common_cache_z)
                      + max0(3 - (s IDcie_load_common_cache_j))
                      + (4 # 1) * max0(511 - (s IDcie_load_common_cache_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_load_common_cache_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 4*) F_max0_ge_0 (512 - (s IDcie_load_common_cache_i))]
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
    | 23%positive => [(*0 4*) F_max0_pre_decrement (512
                                                    - (s IDcie_load_common_cache_i)) (1)]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                 - (s IDcie_load_common_cache_j))) (F_check_ge (0) (0))]
    | 32%positive => [(*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDcie_load_common_cache_j)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem cie_load_common_cache_ai_correct:
  forall s p' s', steps (g_start cie_load_common_cache) s (g_edges cie_load_common_cache) p' s' -> cie_load_common_cache_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_load_common_cache_pot_correct:
  forall s p' s',
    steps (g_start cie_load_common_cache) s (g_edges cie_load_common_cache) p' s' ->
    (cie_load_common_cache_pot (g_start cie_load_common_cache) s >= cie_load_common_cache_pot p' s')%Q.
Proof.
  check_lp cie_load_common_cache_ai_correct cie_load_common_cache_hints.
Qed.

