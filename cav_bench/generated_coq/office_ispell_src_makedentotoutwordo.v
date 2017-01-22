Require Import pasta.Pasta.

Notation IDtoutword_z := 1%positive.
Notation IDtoutword_bit := 2%positive.
Notation IDtoutword_has_marker := 3%positive.
Notation IDtoutword_cent := 4%positive.
Notation IDtoutword_toutfile := 5%positive.
Notation IDtoutword_word := 6%positive.
Definition toutword : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDtoutword_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDtoutword_has_marker (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDtoutword_bit (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDtoutword_bit) s) <
             (eval (ENum (26)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDtoutword_bit) s) >=
             (eval (ENum (26)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDtoutword_bit
             (Some (EAdd (EVar IDtoutword_bit) (ENum (1))))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDtoutword_z (Some (EAdd (ENum (1))
             (EVar IDtoutword_z)))),17%positive)::
             (17%positive,AWeaken,6%positive)::nil
|}.

Definition toutword_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_z) <= 0)%Z
    | 3%positive => (-1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0)%Z
    | 4%positive => (-1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_bit) <= 0)%Z
    | 5%positive => (-1 * (s IDtoutword_bit) <= 0 /\ 1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0)%Z
    | 6%positive => (-1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_bit) + -26 <= 0)%Z
    | 7%positive => (1 * (s IDtoutword_bit) + -26 <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_bit) + 26 <= 0)%Z
    | 8%positive => (-1 * (s IDtoutword_bit) + 26 <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_bit) + -26 <= 0)%Z
    | 9%positive => (1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_bit) + -25 <= 0)%Z
    | 10%positive => (1 * (s IDtoutword_bit) + -25 <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0)%Z
    | 11%positive => (1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_bit) + -25 <= 0)%Z
    | 12%positive => (1 * (s IDtoutword_bit) + -25 <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0)%Z
    | 13%positive => (1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_bit) <= 0 /\ -1 * (s IDtoutword_z) <= 0 /\ 1 * (s IDtoutword_bit) + -25 <= 0)%Z
    | 14%positive => (-1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_bit) + 1 <= 0 /\ 1 * (s IDtoutword_bit) + -26 <= 0)%Z
    | 15%positive => (1 * (s IDtoutword_bit) + -26 <= 0 /\ -1 * (s IDtoutword_bit) + 1 <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_z) <= 0)%Z
    | 16%positive => (-1 * (s IDtoutword_z) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_bit) + 1 <= 0 /\ 1 * (s IDtoutword_bit) + -26 <= 0)%Z
    | 17%positive => (1 * (s IDtoutword_bit) + -26 <= 0 /\ -1 * (s IDtoutword_bit) + 1 <= 0 /\ 1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_has_marker) <= 0 /\ -1 * (s IDtoutword_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition toutword_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((26 # 1))%Q
    | 2%positive => ((26 # 1) + (s IDtoutword_z))%Q
    | 3%positive => ((26 # 1) + (s IDtoutword_z))%Q
    | 4%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | 5%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | 6%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | 7%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | 8%positive => ((s IDtoutword_z))%Q
    | 9%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | 10%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(25 - (s IDtoutword_bit)))%Q
    | 11%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(25 - (s IDtoutword_bit)))%Q
    | 12%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(25 - (s IDtoutword_bit)))%Q
    | 13%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(25 - (s IDtoutword_bit)))%Q
    | 14%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(26 - (s IDtoutword_bit)))%Q
    | 15%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(26 - (s IDtoutword_bit)))%Q
    | 16%positive => ((1 # 1) + (s IDtoutword_z)
                      + max0(26 - (s IDtoutword_bit)))%Q
    | 17%positive => ((s IDtoutword_z) + max0(26 - (s IDtoutword_bit)))%Q
    | _ => (0 # 1)%Q
  end.

Definition toutword_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (26
                                                            - (s IDtoutword_bit)) (25
                                                                    - (s IDtoutword_bit)));
                     (*-1 0*) F_max0_ge_0 (25 - (s IDtoutword_bit))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement (26 - (s IDtoutword_bit)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem toutword_ai_correct:
  forall s p' s', steps (g_start toutword) s (g_edges toutword) p' s' -> toutword_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem toutword_pot_correct:
  forall s p' s',
    steps (g_start toutword) s (g_edges toutword) p' s' ->
    (toutword_pot (g_start toutword) s >= toutword_pot p' s')%Q.
Proof.
  check_lp toutword_ai_correct toutword_hints.
Qed.

