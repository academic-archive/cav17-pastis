Require Import pasta.Pasta.

Notation IDprintKeyHash_z := 1%positive.
Notation IDprintKeyHash__tmp := 2%positive.
Notation IDprintKeyHash_i := 3%positive.
Notation IDprintKeyHash_hash := 4%positive.
Notation IDprintKeyHash_indent := 5%positive.
Definition printKeyHash : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDprintKeyHash_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDprintKeyHash__tmp
             (Some (EVar IDprintKeyHash_indent))),3%positive)::
             (3%positive,(AAssign IDprintKeyHash_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDprintKeyHash_i)
             s) < (eval (ENum (8)) s))%Z)),21%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDprintKeyHash_i)
             s) >= (eval (ENum (8)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDprintKeyHash_i (Some (ENum (8)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDprintKeyHash_i)
             s) < (eval (ENum (16)) s))%Z)),14%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDprintKeyHash_i)
             s) >= (eval (ENum (16)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDprintKeyHash_i
             (Some (EAdd (EVar IDprintKeyHash_i) (ENum (1))))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDprintKeyHash_z (Some (EAdd (ENum (1))
             (EVar IDprintKeyHash_z)))),20%positive)::
             (20%positive,AWeaken,11%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDprintKeyHash_i
             (Some (EAdd (EVar IDprintKeyHash_i) (ENum (1))))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDprintKeyHash_z (Some (EAdd (ENum (1))
             (EVar IDprintKeyHash_z)))),27%positive)::
             (27%positive,AWeaken,6%positive)::nil
|}.

Definition printKeyHash_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0)%Z
    | 3%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_z) <= 0)%Z
    | 4%positive => (1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) <= 0 /\ -1 * (s IDprintKeyHash_i) <= 0)%Z
    | 5%positive => (-1 * (s IDprintKeyHash_i) <= 0 /\ 1 * (s IDprintKeyHash_i) <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_z) <= 0)%Z
    | 6%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0)%Z
    | 7%positive => (1 * (s IDprintKeyHash_i) + -8 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 8 <= 0)%Z
    | 8%positive => (-1 * (s IDprintKeyHash_i) + 8 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0)%Z
    | 9%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0 /\ -1 * (s IDprintKeyHash_i) + 8 <= 0)%Z
    | 10%positive => (-1 * (s IDprintKeyHash_i) + 8 <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0)%Z
    | 11%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 8 <= 0 /\ 1 * (s IDprintKeyHash_i) + -16 <= 0)%Z
    | 12%positive => (1 * (s IDprintKeyHash_i) + -16 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 16 <= 0)%Z
    | 13%positive => (-1 * (s IDprintKeyHash_i) + 16 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -16 <= 0)%Z
    | 14%positive => (-1 * (s IDprintKeyHash_i) + 8 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -15 <= 0)%Z
    | 15%positive => (1 * (s IDprintKeyHash_i) + -15 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 8 <= 0)%Z
    | 16%positive => (-1 * (s IDprintKeyHash_i) + 8 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -15 <= 0)%Z
    | 17%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 9 <= 0 /\ 1 * (s IDprintKeyHash_i) + -16 <= 0)%Z
    | 18%positive => (1 * (s IDprintKeyHash_i) + -16 <= 0 /\ -1 * (s IDprintKeyHash_i) + 9 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0)%Z
    | 19%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 9 <= 0 /\ 1 * (s IDprintKeyHash_i) + -16 <= 0)%Z
    | 20%positive => (1 * (s IDprintKeyHash_i) + -16 <= 0 /\ -1 * (s IDprintKeyHash_i) + 9 <= 0 /\ -1 * (s IDprintKeyHash_z) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDprintKeyHash_i) <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDprintKeyHash_i) + -7 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) <= 0)%Z
    | 23%positive => (-1 * (s IDprintKeyHash_i) <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0 /\ 1 * (s IDprintKeyHash_i) + -7 <= 0)%Z
    | 24%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 1 <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0)%Z
    | 25%positive => (1 * (s IDprintKeyHash_i) + -8 <= 0 /\ -1 * (s IDprintKeyHash_i) + 1 <= 0 /\ -1 * (s IDprintKeyHash_z) <= 0)%Z
    | 26%positive => (-1 * (s IDprintKeyHash_z) <= 0 /\ -1 * (s IDprintKeyHash_i) + 1 <= 0 /\ 1 * (s IDprintKeyHash_i) + -8 <= 0)%Z
    | 27%positive => (1 * (s IDprintKeyHash_i) + -8 <= 0 /\ -1 * (s IDprintKeyHash_i) + 1 <= 0 /\ -1 * (s IDprintKeyHash_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition printKeyHash_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDprintKeyHash_z))%Q
    | 3%positive => ((16 # 1) + (s IDprintKeyHash_z))%Q
    | 4%positive => ((8 # 1) + (s IDprintKeyHash_z)
                     + max0(8 - (s IDprintKeyHash_i)))%Q
    | 5%positive => ((8 # 1) + (s IDprintKeyHash_z)
                     + max0(8 - (s IDprintKeyHash_i)))%Q
    | 6%positive => ((8 # 1) + (s IDprintKeyHash_z)
                     + max0(8 - (s IDprintKeyHash_i)))%Q
    | 7%positive => ((8 # 1) + (s IDprintKeyHash_z)
                     + max0(8 - (s IDprintKeyHash_i)))%Q
    | 8%positive => ((8 # 1) + (s IDprintKeyHash_z))%Q
    | 9%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 10%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 11%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 12%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 13%positive => ((s IDprintKeyHash_z))%Q
    | 14%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 15%positive => ((1 # 1) + (s IDprintKeyHash_z)
                      + max0(15 - (s IDprintKeyHash_i)))%Q
    | 16%positive => ((1 # 1) + (s IDprintKeyHash_z)
                      + max0(15 - (s IDprintKeyHash_i)))%Q
    | 17%positive => ((1 # 1) + (s IDprintKeyHash_z)
                      + max0(16 - (s IDprintKeyHash_i)))%Q
    | 18%positive => ((1 # 1) + (s IDprintKeyHash_z)
                      + max0(16 - (s IDprintKeyHash_i)))%Q
    | 19%positive => ((1 # 1) + (s IDprintKeyHash_z)
                      + max0(16 - (s IDprintKeyHash_i)))%Q
    | 20%positive => ((s IDprintKeyHash_z) + max0(16 - (s IDprintKeyHash_i)))%Q
    | 21%positive => ((8 # 1) + (s IDprintKeyHash_z)
                      + max0(8 - (s IDprintKeyHash_i)))%Q
    | 22%positive => ((9 # 1) + (s IDprintKeyHash_z)
                      + max0(7 - (s IDprintKeyHash_i)))%Q
    | 23%positive => ((9 # 1) + (s IDprintKeyHash_z)
                      + max0(7 - (s IDprintKeyHash_i)))%Q
    | 24%positive => ((9 # 1) + (s IDprintKeyHash_z)
                      + max0(8 - (s IDprintKeyHash_i)))%Q
    | 25%positive => ((9 # 1) + (s IDprintKeyHash_z)
                      + max0(8 - (s IDprintKeyHash_i)))%Q
    | 26%positive => ((9 # 1) + (s IDprintKeyHash_z)
                      + max0(8 - (s IDprintKeyHash_i)))%Q
    | 27%positive => ((8 # 1) + (s IDprintKeyHash_z)
                      + max0(8 - (s IDprintKeyHash_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition printKeyHash_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDprintKeyHash_i)) (7
                                                                    - (s IDprintKeyHash_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDprintKeyHash_i))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDprintKeyHash_i)) (15
                                                                    - (s IDprintKeyHash_i)));
                      (*-1 0*) F_max0_ge_0 (15 - (s IDprintKeyHash_i))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement (16
                                                     - (s IDprintKeyHash_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_pre_decrement (8 - (s IDprintKeyHash_i)) (1)]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | _ => []
  end.


Theorem printKeyHash_ai_correct:
  forall s p' s', steps (g_start printKeyHash) s (g_edges printKeyHash) p' s' -> printKeyHash_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem printKeyHash_pot_correct:
  forall s p' s',
    steps (g_start printKeyHash) s (g_edges printKeyHash) p' s' ->
    (printKeyHash_pot (g_start printKeyHash) s >= printKeyHash_pot p' s')%Q.
Proof.
  check_lp printKeyHash_ai_correct printKeyHash_hints.
Qed.

