Require Import pasta.Pasta.

Notation IDfind_biggest_volume_z := 1%positive.
Notation IDfind_biggest_volume__tmp := 2%positive.
Notation IDfind_biggest_volume_i := 3%positive.
Notation IDfind_biggest_volume_maxv := 4%positive.
Notation IDfind_biggest_volume_boxlist := 5%positive.
Notation IDfind_biggest_volume_numboxes := 6%positive.
Definition find_biggest_volume : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDfind_biggest_volume_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfind_biggest_volume__tmp
             (Some (EVar IDfind_biggest_volume_numboxes))),3%positive)::
             (3%positive,(AAssign IDfind_biggest_volume_maxv
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDfind_biggest_volume_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDfind_biggest_volume_i) s) <
             (eval (EVar IDfind_biggest_volume__tmp) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDfind_biggest_volume_i) s) >=
             (eval (EVar IDfind_biggest_volume__tmp) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (11%positive,ANone,14%positive)::
             (12%positive,(AAssign IDfind_biggest_volume_maxv None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDfind_biggest_volume_i
             (Some (EAdd (EVar IDfind_biggest_volume_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDfind_biggest_volume_z
             (Some (EAdd (ENum (1)) (EVar IDfind_biggest_volume_z)))),
             19%positive)::(19%positive,AWeaken,7%positive)::nil
|}.

Definition find_biggest_volume_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume_z) <= 0)%Z
    | 4%positive => (1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume_maxv) <= 0 /\ -1 * (s IDfind_biggest_volume_maxv) <= 0)%Z
    | 5%positive => (-1 * (s IDfind_biggest_volume_maxv) <= 0 /\ 1 * (s IDfind_biggest_volume_maxv) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 6%positive => (-1 * (s IDfind_biggest_volume_i) <= 0 /\ 1 * (s IDfind_biggest_volume_i) <= 0 /\ 1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume_maxv) <= 0 /\ -1 * (s IDfind_biggest_volume_maxv) <= 0)%Z
    | 7%positive => (-1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 8%positive => (-1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ 1 * (s IDfind_biggest_volume__tmp)+ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 9%positive => (1 * (s IDfind_biggest_volume__tmp)+ -1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 10%positive => (-1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 12%positive => (-1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 14%positive => (-1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) + 1 <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume_i) <= 0)%Z
    | 16%positive => (-1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDfind_biggest_volume_i) + 1 <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) <= 0)%Z
    | 18%positive => (-1 * (s IDfind_biggest_volume_z) <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDfind_biggest_volume_i) + 1 <= 0 /\ -1 * (s IDfind_biggest_volume__tmp)+ 1 * (s IDfind_biggest_volume_i) <= 0 /\ -1 * (s IDfind_biggest_volume_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition find_biggest_volume_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDfind_biggest_volume_numboxes)))%Q
    | 2%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume_numboxes)))%Q
    | 3%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)))%Q
    | 4%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)))%Q
    | 5%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)
                            - (s IDfind_biggest_volume_i)))%Q
    | 6%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)
                            - (s IDfind_biggest_volume_i)))%Q
    | 7%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)
                            - (s IDfind_biggest_volume_i)))%Q
    | 8%positive => ((s IDfind_biggest_volume_z)
                     + max0((s IDfind_biggest_volume__tmp)
                            - (s IDfind_biggest_volume_i)))%Q
    | 9%positive => ((s IDfind_biggest_volume_z))%Q
    | 10%positive => ((s IDfind_biggest_volume_z)
                      + max0((s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 11%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0(-1 + (s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 12%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0(-1 + (s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 13%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0(-1 + (s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 14%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0(-1 + (s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 15%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0(-1 + (s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 16%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0((s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 17%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0((s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 18%positive => ((1 # 1) + (s IDfind_biggest_volume_z)
                      + max0((s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | 19%positive => ((s IDfind_biggest_volume_z)
                      + max0((s IDfind_biggest_volume__tmp)
                             - (s IDfind_biggest_volume_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition find_biggest_volume_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfind_biggest_volume__tmp)
                                                            - (s IDfind_biggest_volume_i)) (-1
                                                                    + (s IDfind_biggest_volume__tmp)
                                                                    - (s IDfind_biggest_volume_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDfind_biggest_volume__tmp)
                                                                - (s IDfind_biggest_volume_i))) (F_check_ge (0) (0))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfind_biggest_volume__tmp)
                                                     - (s IDfind_biggest_volume_i)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem find_biggest_volume_ai_correct:
  forall s p' s', steps (g_start find_biggest_volume) s (g_edges find_biggest_volume) p' s' -> find_biggest_volume_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem find_biggest_volume_pot_correct:
  forall s p' s',
    steps (g_start find_biggest_volume) s (g_edges find_biggest_volume) p' s' ->
    (find_biggest_volume_pot (g_start find_biggest_volume) s >= find_biggest_volume_pot p' s')%Q.
Proof.
  check_lp find_biggest_volume_ai_correct find_biggest_volume_hints.
Qed.

