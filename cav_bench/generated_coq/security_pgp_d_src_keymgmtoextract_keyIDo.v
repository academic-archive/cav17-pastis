Require Import pasta.Pasta.

Notation IDextract_keyID_z := 1%positive.
Notation IDextract_keyID_i := 2%positive.
Notation IDextract_keyID_j := 3%positive.
Notation IDextract_keyID_keyID := 4%positive.
Notation IDextract_keyID_n := 5%positive.
Definition extract_keyID : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDextract_keyID_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDextract_keyID_i None),3%positive)::
             (3%positive,(AAssign IDextract_keyID_i
             (Some (ESub (EAdd (EVar IDextract_keyID_i) (ENum (2)))
             (ENum (8))))),4%positive)::
             (4%positive,(AAssign IDextract_keyID_j (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDextract_keyID_j)
             s) < (eval (ENum (8)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDextract_keyID_j)
             s) >= (eval (ENum (8)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDextract_keyID_i
             (Some (EAdd (EVar IDextract_keyID_i) (ENum (1))))),12%positive)::
             (12%positive,(AAssign IDextract_keyID_j
             (Some (EAdd (EVar IDextract_keyID_j) (ENum (1))))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDextract_keyID_z (Some (EAdd (ENum (1))
             (EVar IDextract_keyID_z)))),16%positive)::
             (16%positive,AWeaken,7%positive)::nil
|}.

Definition extract_keyID_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_z) <= 0)%Z
    | 3%positive => (-1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_z) <= 0)%Z
    | 4%positive => (1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_z) <= 0)%Z
    | 5%positive => (-1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_j) <= 0 /\ -1 * (s IDextract_keyID_j) <= 0)%Z
    | 6%positive => (-1 * (s IDextract_keyID_j) <= 0 /\ 1 * (s IDextract_keyID_j) <= 0 /\ 1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_z) <= 0)%Z
    | 7%positive => (-1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_j) <= 0 /\ 1 * (s IDextract_keyID_j) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDextract_keyID_j) + -8 <= 0 /\ -1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_j) + 8 <= 0)%Z
    | 9%positive => (-1 * (s IDextract_keyID_j) + 8 <= 0 /\ -1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_j) + -8 <= 0)%Z
    | 10%positive => (-1 * (s IDextract_keyID_j) <= 0 /\ -1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_j) + -7 <= 0)%Z
    | 11%positive => (1 * (s IDextract_keyID_j) + -7 <= 0 /\ -1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_j) <= 0)%Z
    | 12%positive => (-1 * (s IDextract_keyID_j) <= 0 /\ -1 * (s IDextract_keyID_z) <= 0 /\ 1 * (s IDextract_keyID_j) + -7 <= 0)%Z
    | 13%positive => (-1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_j) + 1 <= 0 /\ 1 * (s IDextract_keyID_j) + -8 <= 0)%Z
    | 14%positive => (1 * (s IDextract_keyID_j) + -8 <= 0 /\ -1 * (s IDextract_keyID_j) + 1 <= 0 /\ -1 * (s IDextract_keyID_z) <= 0)%Z
    | 15%positive => (-1 * (s IDextract_keyID_z) <= 0 /\ -1 * (s IDextract_keyID_j) + 1 <= 0 /\ 1 * (s IDextract_keyID_j) + -8 <= 0)%Z
    | 16%positive => (1 * (s IDextract_keyID_j) + -8 <= 0 /\ -1 * (s IDextract_keyID_j) + 1 <= 0 /\ -1 * (s IDextract_keyID_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition extract_keyID_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDextract_keyID_z))%Q
    | 3%positive => ((8 # 1) + (s IDextract_keyID_z))%Q
    | 4%positive => ((8 # 1) + (s IDextract_keyID_z))%Q
    | 5%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | 6%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | 7%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | 8%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | 9%positive => ((s IDextract_keyID_z))%Q
    | 10%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | 11%positive => ((1 # 1) + (s IDextract_keyID_z)
                      + max0(7 - (s IDextract_keyID_j)))%Q
    | 12%positive => ((1 # 1) + (s IDextract_keyID_z)
                      + max0(7 - (s IDextract_keyID_j)))%Q
    | 13%positive => ((1 # 1) + (s IDextract_keyID_z)
                      + max0(8 - (s IDextract_keyID_j)))%Q
    | 14%positive => ((1 # 1) + (s IDextract_keyID_z)
                      + max0(8 - (s IDextract_keyID_j)))%Q
    | 15%positive => ((1 # 1) + (s IDextract_keyID_z)
                      + max0(8 - (s IDextract_keyID_j)))%Q
    | 16%positive => ((s IDextract_keyID_z) + max0(8 - (s IDextract_keyID_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition extract_keyID_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDextract_keyID_j)) (7
                                                                    - (s IDextract_keyID_j)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                - (s IDextract_keyID_j))) (F_check_ge (0) (0))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDextract_keyID_j)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem extract_keyID_ai_correct:
  forall s p' s', steps (g_start extract_keyID) s (g_edges extract_keyID) p' s' -> extract_keyID_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem extract_keyID_pot_correct:
  forall s p' s',
    steps (g_start extract_keyID) s (g_edges extract_keyID) p' s' ->
    (extract_keyID_pot (g_start extract_keyID) s >= extract_keyID_pot p' s')%Q.
Proof.
  check_lp extract_keyID_ai_correct extract_keyID_hints.
Qed.

