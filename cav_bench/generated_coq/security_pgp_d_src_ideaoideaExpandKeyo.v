Require Import pasta.Pasta.

Notation IDideaExpandKey_z := 1%positive.
Notation IDideaExpandKey_i := 2%positive.
Notation IDideaExpandKey_j := 3%positive.
Notation IDideaExpandKey_EK := 4%positive.
Notation IDideaExpandKey_userkey := 5%positive.
Definition ideaExpandKey : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDideaExpandKey_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDideaExpandKey_j (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDideaExpandKey_j)
             s) < (eval (ENum (8)) s))%Z)),22%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDideaExpandKey_j)
             s) >= (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDideaExpandKey_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDideaExpandKey_j)
             s) < (eval (ENum (52)) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDideaExpandKey_j)
             s) >= (eval (ENum (52)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDideaExpandKey_i
             (Some (EAdd (EVar IDideaExpandKey_i) (ENum (1))))),15%positive)::
             (15%positive,(AAssign IDideaExpandKey_i None),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDideaExpandKey_j
             (Some (EAdd (EVar IDideaExpandKey_j) (ENum (1))))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDideaExpandKey_z (Some (EAdd (ENum (1))
             (EVar IDideaExpandKey_z)))),21%positive)::
             (21%positive,AWeaken,10%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDideaExpandKey_j
             (Some (EAdd (EVar IDideaExpandKey_j) (ENum (1))))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDideaExpandKey_z (Some (EAdd (ENum (1))
             (EVar IDideaExpandKey_z)))),28%positive)::
             (28%positive,AWeaken,5%positive)::nil
|}.

Definition ideaExpandKey_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) <= 0 /\ -1 * (s IDideaExpandKey_j) <= 0)%Z
    | 4%positive => (-1 * (s IDideaExpandKey_j) <= 0 /\ 1 * (s IDideaExpandKey_j) <= 0 /\ 1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) <= 0 /\ 1 * (s IDideaExpandKey_j) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDideaExpandKey_j) + -8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDideaExpandKey_j) + 8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDideaExpandKey_j) + -8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0 /\ 1 * (s IDideaExpandKey_i) <= 0 /\ -1 * (s IDideaExpandKey_i) <= 0)%Z
    | 9%positive => (-1 * (s IDideaExpandKey_i) <= 0 /\ 1 * (s IDideaExpandKey_i) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -8 <= 0)%Z
    | 10%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0 /\ 1 * (s IDideaExpandKey_j) + -52 <= 0)%Z
    | 11%positive => (1 * (s IDideaExpandKey_j) + -52 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 52 <= 0)%Z
    | 12%positive => (-1 * (s IDideaExpandKey_j) + 52 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -52 <= 0)%Z
    | 13%positive => (-1 * (s IDideaExpandKey_j) + 8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -51 <= 0)%Z
    | 14%positive => (1 * (s IDideaExpandKey_j) + -51 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0)%Z
    | 15%positive => (-1 * (s IDideaExpandKey_j) + 8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -51 <= 0)%Z
    | 16%positive => (1 * (s IDideaExpandKey_j) + -51 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 8 <= 0)%Z
    | 17%positive => (-1 * (s IDideaExpandKey_j) + 8 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -51 <= 0)%Z
    | 18%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 9 <= 0 /\ 1 * (s IDideaExpandKey_j) + -52 <= 0)%Z
    | 19%positive => (1 * (s IDideaExpandKey_j) + -52 <= 0 /\ -1 * (s IDideaExpandKey_j) + 9 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0)%Z
    | 20%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 9 <= 0 /\ 1 * (s IDideaExpandKey_j) + -52 <= 0)%Z
    | 21%positive => (1 * (s IDideaExpandKey_j) + -52 <= 0 /\ -1 * (s IDideaExpandKey_j) + 9 <= 0 /\ -1 * (s IDideaExpandKey_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDideaExpandKey_j) <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDideaExpandKey_j) + -7 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) <= 0)%Z
    | 24%positive => (-1 * (s IDideaExpandKey_j) <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0 /\ 1 * (s IDideaExpandKey_j) + -7 <= 0)%Z
    | 25%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 1 <= 0 /\ 1 * (s IDideaExpandKey_j) + -8 <= 0)%Z
    | 26%positive => (1 * (s IDideaExpandKey_j) + -8 <= 0 /\ -1 * (s IDideaExpandKey_j) + 1 <= 0 /\ -1 * (s IDideaExpandKey_z) <= 0)%Z
    | 27%positive => (-1 * (s IDideaExpandKey_z) <= 0 /\ -1 * (s IDideaExpandKey_j) + 1 <= 0 /\ 1 * (s IDideaExpandKey_j) + -8 <= 0)%Z
    | 28%positive => (1 * (s IDideaExpandKey_j) + -8 <= 0 /\ -1 * (s IDideaExpandKey_j) + 1 <= 0 /\ -1 * (s IDideaExpandKey_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ideaExpandKey_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((52 # 1))%Q
    | 2%positive => ((52 # 1) + (s IDideaExpandKey_z))%Q
    | 3%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 4%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 5%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 6%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 7%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 8%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 9%positive => ((s IDideaExpandKey_z) + max0(52 - (s IDideaExpandKey_j)))%Q
    | 10%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 11%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 12%positive => ((s IDideaExpandKey_z))%Q
    | 13%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 14%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 15%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 16%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 17%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 18%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 19%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 20%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 21%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 22%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 23%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 24%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(51 - (s IDideaExpandKey_j)))%Q
    | 25%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 26%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 27%positive => ((1 # 1) + (s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | 28%positive => ((s IDideaExpandKey_z)
                      + max0(52 - (s IDideaExpandKey_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaExpandKey_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (52
                                                             - (s IDideaExpandKey_j)) (51
                                                                    - (s IDideaExpandKey_j)));
                      (*-1 0*) F_max0_ge_0 (51 - (s IDideaExpandKey_j))]
    | 12%positive => []
    | 13%positive => [(*0 1*) F_max0_pre_decrement (52
                                                    - (s IDideaExpandKey_j)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*3.76378e-10 1*) F_max0_pre_decrement (52
                                                              - (s IDideaExpandKey_j)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | _ => []
  end.


Theorem ideaExpandKey_ai_correct:
  forall s p' s', steps (g_start ideaExpandKey) s (g_edges ideaExpandKey) p' s' -> ideaExpandKey_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaExpandKey_pot_correct:
  forall s p' s',
    steps (g_start ideaExpandKey) s (g_edges ideaExpandKey) p' s' ->
    (ideaExpandKey_pot (g_start ideaExpandKey) s >= ideaExpandKey_pot p' s')%Q.
Proof.
  check_lp ideaExpandKey_ai_correct ideaExpandKey_hints.
Qed.

