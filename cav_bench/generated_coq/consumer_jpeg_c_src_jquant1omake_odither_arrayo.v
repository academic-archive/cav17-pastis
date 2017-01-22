Require Import pasta.Pasta.

Notation IDmake_odither_array_z := 1%positive.
Notation IDmake_odither_array__tmp := 2%positive.
Notation IDmake_odither_array_den := 3%positive.
Notation IDmake_odither_array_j := 4%positive.
Notation IDmake_odither_array_k := 5%positive.
Notation IDmake_odither_array_num := 6%positive.
Notation IDmake_odither_array_cinfo := 7%positive.
Notation IDmake_odither_array_ncolors := 8%positive.
Definition make_odither_array : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDmake_odither_array_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmake_odither_array__tmp
             (Some (EVar IDmake_odither_array_ncolors))),3%positive)::
             (3%positive,(AAssign IDmake_odither_array_den
             (Some (EMul (ENum (512)) (ESub (EVar IDmake_odither_array__tmp)
             (ENum (1)))))),4%positive)::
             (4%positive,(AAssign IDmake_odither_array_j (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_j) s) <
             (eval (ENum (16)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_j) s) >=
             (eval (ENum (16)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDmake_odither_array_k (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_k) s) <
             (eval (ENum (16)) s))%Z)),22%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_k) s) >=
             (eval (ENum (16)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDmake_odither_array_j
             (Some (EAdd (EVar IDmake_odither_array_j) (ENum (1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDmake_odither_array_z
             (Some (EAdd (ENum (1)) (EVar IDmake_odither_array_z)))),
             21%positive)::(21%positive,AWeaken,7%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDmake_odither_array_num None),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_num) s) <
             (eval (ENum (0)) s))%Z)),28%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDmake_odither_array_num) s) >=
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,30%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDmake_odither_array_k
             (Some (EAdd (EVar IDmake_odither_array_k) (ENum (1))))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDmake_odither_array_z
             (Some (EAdd (ENum (1)) (EVar IDmake_odither_array_z)))),
             35%positive)::(35%positive,AWeaken,14%positive)::nil
|}.

Definition make_odither_array_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_z) <= 0)%Z
    | 4%positive => (1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 6%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ 1 * (s IDmake_odither_array_j) <= 0 /\ 1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 8%positive => (-1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) + 16 <= 0)%Z
    | 9%positive => (-1 * (s IDmake_odither_array_j) + 16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_j) + -15 <= 0)%Z
    | 11%positive => (1 * (s IDmake_odither_array_j) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 12%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_j) + -15 <= 0 /\ 1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0)%Z
    | 13%positive => (-1 * (s IDmake_odither_array_k) <= 0 /\ 1 * (s IDmake_odither_array_k) <= 0 /\ 1 * (s IDmake_odither_array_j) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 14%positive => (-1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0)%Z
    | 15%positive => (1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) + 16 <= 0)%Z
    | 16%positive => (-1 * (s IDmake_odither_array_k) + 16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0)%Z
    | 17%positive => (1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) + 16 <= 0)%Z
    | 18%positive => (-1 * (s IDmake_odither_array_k) + 16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_j) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDmake_odither_array_j) + 1 <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) + 16 <= 0)%Z
    | 20%positive => (-1 * (s IDmake_odither_array_k) + 16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_j) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDmake_odither_array_j) + 1 <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_k) + 16 <= 0 /\ -1 * (s IDmake_odither_array_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0)%Z
    | 23%positive => (1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 24%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0)%Z
    | 25%positive => (1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 26%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_num) <= 0)%Z
    | 27%positive => (-1 * (s IDmake_odither_array_num) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 28%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0 /\ 1 * (s IDmake_odither_array_num) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDmake_odither_array_num) + 1 <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 30%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -15 <= 0)%Z
    | 31%positive => (1 * (s IDmake_odither_array_k) + -15 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_k) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 32%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_k) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDmake_odither_array_k) + 1 <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0)%Z
    | 34%positive => (-1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_k) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDmake_odither_array_k) + 1 <= 0 /\ 1 * (s IDmake_odither_array_k) + -16 <= 0 /\ -1 * (s IDmake_odither_array_j) <= 0 /\ -1 * (s IDmake_odither_array_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition make_odither_array_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((272 # 1))%Q
    | 2%positive => ((272 # 1) + (s IDmake_odither_array_z))%Q
    | 3%positive => ((272 # 1) + (s IDmake_odither_array_z))%Q
    | 4%positive => ((272 # 1) + (s IDmake_odither_array_z))%Q
    | 5%positive => ((s IDmake_odither_array_z)
                     + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 6%positive => ((s IDmake_odither_array_z)
                     + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 7%positive => ((s IDmake_odither_array_z)
                     + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 8%positive => ((s IDmake_odither_array_z)
                     + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 9%positive => ((s IDmake_odither_array_z))%Q
    | 10%positive => ((s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 11%positive => ((s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 12%positive => (-(16 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 13%positive => (-(16 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 14%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 15%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 16%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j)))%Q
    | 17%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j)))%Q
    | 18%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 19%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 20%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 21%positive => ((s IDmake_odither_array_z)
                      + (17 # 1) * max0(16 - (s IDmake_odither_array_j)))%Q
    | 22%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 23%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 24%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 25%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 26%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 27%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 28%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 29%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 30%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 31%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(15 - (s IDmake_odither_array_k)))%Q
    | 32%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 33%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 34%positive => ((2 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | 35%positive => ((1 # 1) + (s IDmake_odither_array_z)
                      + (17 # 1) * max0(15 - (s IDmake_odither_array_j))
                      + max0(16 - (s IDmake_odither_array_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition make_odither_array_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-17 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDmake_odither_array_j)) (15
                                                                    - (s IDmake_odither_array_j)));
                     (*-17 0*) F_max0_ge_0 (15 - (s IDmake_odither_array_j))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-17 0*) F_max0_pre_decrement (16
                                                      - (s IDmake_odither_array_j)) (1)]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_ge_0 (16 - (s IDmake_odither_array_k))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*0 1*) F_max0_pre_decrement (16
                                                    - (s IDmake_odither_array_k)) (1)]
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
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem make_odither_array_ai_correct:
  forall s p' s', steps (g_start make_odither_array) s (g_edges make_odither_array) p' s' -> make_odither_array_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem make_odither_array_pot_correct:
  forall s p' s',
    steps (g_start make_odither_array) s (g_edges make_odither_array) p' s' ->
    (make_odither_array_pot (g_start make_odither_array) s >= make_odither_array_pot p' s')%Q.
Proof.
  check_lp make_odither_array_ai_correct make_odither_array_hints.
Qed.

