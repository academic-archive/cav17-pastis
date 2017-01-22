Require Import pasta.Pasta.

Notation IDsend_all_trees_z := 1%positive.
Notation IDsend_all_trees__tmp := 2%positive.
Notation IDsend_all_trees__tmp1 := 3%positive.
Notation IDsend_all_trees__tmp2 := 4%positive.
Notation IDsend_all_trees_rank := 5%positive.
Notation IDsend_all_trees_blcodes := 6%positive.
Notation IDsend_all_trees_dcodes := 7%positive.
Notation IDsend_all_trees_lcodes := 8%positive.
Definition send_all_trees : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDsend_all_trees_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsend_all_trees__tmp2
             (Some (EVar IDsend_all_trees_lcodes))),3%positive)::
             (3%positive,(AAssign IDsend_all_trees__tmp1
             (Some (EVar IDsend_all_trees_dcodes))),4%positive)::
             (4%positive,(AAssign IDsend_all_trees__tmp
             (Some (EVar IDsend_all_trees_blcodes))),5%positive)::
             (5%positive,(AAssign IDsend_all_trees_rank (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDsend_all_trees_rank) s) <
             (eval (EVar IDsend_all_trees__tmp) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDsend_all_trees_rank) s) >=
             (eval (EVar IDsend_all_trees__tmp) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDsend_all_trees_rank
             (Some (EAdd (EVar IDsend_all_trees_rank) (ENum (1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDsend_all_trees_z (Some (EAdd (ENum (1))
             (EVar IDsend_all_trees_z)))),17%positive)::
             (17%positive,AWeaken,8%positive)::nil
|}.

Definition send_all_trees_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsend_all_trees_z) <= 0 /\ 1 * (s IDsend_all_trees_z) <= 0)%Z
    | 4%positive => (1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsend_all_trees_z) <= 0 /\ 1 * (s IDsend_all_trees_z) <= 0)%Z
    | 6%positive => (1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ 1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 7%positive => (-1 * (s IDsend_all_trees_rank) <= 0 /\ 1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ 1 * (s IDsend_all_trees_z) <= 0)%Z
    | 8%positive => (-1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 9%positive => (-1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ 1 * (s IDsend_all_trees__tmp)+ -1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 10%positive => (1 * (s IDsend_all_trees__tmp)+ -1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 11%positive => (-1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) + 1 <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 13%positive => (-1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_rank) + 1 <= 0 /\ -1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 15%positive => (-1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_rank) + 1 <= 0 /\ -1 * (s IDsend_all_trees_z) <= 0)%Z
    | 16%positive => (-1 * (s IDsend_all_trees_z) <= 0 /\ -1 * (s IDsend_all_trees_rank) + 1 <= 0 /\ -1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) <= 0)%Z
    | 17%positive => (-1 * (s IDsend_all_trees__tmp)+ 1 * (s IDsend_all_trees_rank) <= 0 /\ -1 * (s IDsend_all_trees_rank) + 1 <= 0 /\ -1 * (s IDsend_all_trees_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition send_all_trees_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDsend_all_trees_blcodes)))%Q
    | 2%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees_blcodes)))%Q
    | 3%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees_blcodes)))%Q
    | 4%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees_blcodes)))%Q
    | 5%positive => ((s IDsend_all_trees_z) + max0((s IDsend_all_trees__tmp)))%Q
    | 6%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees__tmp)
                            - (s IDsend_all_trees_rank)))%Q
    | 7%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees__tmp)
                            - (s IDsend_all_trees_rank)))%Q
    | 8%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees__tmp)
                            - (s IDsend_all_trees_rank)))%Q
    | 9%positive => ((s IDsend_all_trees_z)
                     + max0((s IDsend_all_trees__tmp)
                            - (s IDsend_all_trees_rank)))%Q
    | 10%positive => ((s IDsend_all_trees_z))%Q
    | 11%positive => ((s IDsend_all_trees_z)
                      + max0((s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 12%positive => ((1 # 1) + (s IDsend_all_trees_z)
                      + max0(-1 + (s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 13%positive => ((1 # 1) + (s IDsend_all_trees_z)
                      + max0(-1 + (s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 14%positive => ((1 # 1) + (s IDsend_all_trees_z)
                      + max0((s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 15%positive => ((1 # 1) + (s IDsend_all_trees_z)
                      + max0((s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 16%positive => ((1 # 1) + (s IDsend_all_trees_z)
                      + max0((s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | 17%positive => ((s IDsend_all_trees_z)
                      + max0((s IDsend_all_trees__tmp)
                             - (s IDsend_all_trees_rank)))%Q
    | _ => (0 # 1)%Q
  end.

Definition send_all_trees_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsend_all_trees__tmp)
                                                            - (s IDsend_all_trees_rank)) (-1
                                                                    + (s IDsend_all_trees__tmp)
                                                                    - (s IDsend_all_trees_rank)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDsend_all_trees__tmp)
                                                                - (s IDsend_all_trees_rank))) (F_check_ge (0) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsend_all_trees__tmp)
                                                     - (s IDsend_all_trees_rank)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem send_all_trees_ai_correct:
  forall s p' s', steps (g_start send_all_trees) s (g_edges send_all_trees) p' s' -> send_all_trees_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem send_all_trees_pot_correct:
  forall s p' s',
    steps (g_start send_all_trees) s (g_edges send_all_trees) p' s' ->
    (send_all_trees_pot (g_start send_all_trees) s >= send_all_trees_pot p' s')%Q.
Proof.
  check_lp send_all_trees_ai_correct send_all_trees_hints.
Qed.

