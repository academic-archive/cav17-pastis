Require Import pasta.Pasta.

Notation IDset_trans_z := 1%positive.
Notation IDset_trans__tmp := 2%positive.
Notation IDset_trans__tmp1 := 3%positive.
Notation IDset_trans_i := 4%positive.
Notation IDset_trans_a := 5%positive.
Notation IDset_trans_b := 6%positive.
Notation IDset_trans_e := 7%positive.
Notation IDset_trans_ext := 8%positive.
Notation IDset_trans_t := 9%positive.
Definition set_trans : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDset_trans_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDset_trans__tmp
             (Some (EVar IDset_trans_ext))),3%positive)::
             (3%positive,(AAssign IDset_trans__tmp1
             (Some (EVar IDset_trans_e))),4%positive)::
             (4%positive,(AAssign IDset_trans_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDset_trans_i) s) <
             (eval (ENum (19)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDset_trans_i) s) >=
             (eval (ENum (19)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDset_trans__tmp)
             s) <> (eval (ENum (0)) s))%Z)),15%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDset_trans__tmp)
             s) = (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,18%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,20%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,21%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDset_trans_i
             (Some (EAdd (EVar IDset_trans_i) (ENum (1))))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDset_trans_z (Some (EAdd (ENum (1))
             (EVar IDset_trans_z)))),26%positive)::
             (26%positive,AWeaken,7%positive)::nil
|}.

Definition set_trans_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_z) <= 0)%Z
    | 3%positive => (-1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_z) <= 0)%Z
    | 4%positive => (1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_z) <= 0)%Z
    | 5%positive => (-1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 6%positive => (-1 * (s IDset_trans_i) <= 0 /\ 1 * (s IDset_trans_i) <= 0 /\ 1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_z) <= 0)%Z
    | 7%positive => (-1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0 /\ 1 * (s IDset_trans_i) + -19 <= 0)%Z
    | 8%positive => (1 * (s IDset_trans_i) + -19 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) + 19 <= 0)%Z
    | 9%positive => (-1 * (s IDset_trans_i) + 19 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -19 <= 0)%Z
    | 10%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 11%positive => (1 * (s IDset_trans_i) + -18 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 12%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0 /\ 1 * (s IDset_trans__tmp) <= 0 /\ -1 * (s IDset_trans__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDset_trans__tmp) <= 0 /\ 1 * (s IDset_trans__tmp) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 14%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0 /\ 1 * (s IDset_trans__tmp) <= 0 /\ -1 * (s IDset_trans__tmp) <= 0)%Z
    | 15%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 16%positive => (1 * (s IDset_trans_i) + -18 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 17%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 18%positive => (1 * (s IDset_trans_i) + -18 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 19%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 20%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 21%positive => (1 * (s IDset_trans_i) + -18 <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) <= 0)%Z
    | 22%positive => (-1 * (s IDset_trans_i) <= 0 /\ -1 * (s IDset_trans_z) <= 0 /\ 1 * (s IDset_trans_i) + -18 <= 0)%Z
    | 23%positive => (-1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) + 1 <= 0 /\ 1 * (s IDset_trans_i) + -19 <= 0)%Z
    | 24%positive => (1 * (s IDset_trans_i) + -19 <= 0 /\ -1 * (s IDset_trans_i) + 1 <= 0 /\ -1 * (s IDset_trans_z) <= 0)%Z
    | 25%positive => (-1 * (s IDset_trans_z) <= 0 /\ -1 * (s IDset_trans_i) + 1 <= 0 /\ 1 * (s IDset_trans_i) + -19 <= 0)%Z
    | 26%positive => (1 * (s IDset_trans_i) + -19 <= 0 /\ -1 * (s IDset_trans_i) + 1 <= 0 /\ -1 * (s IDset_trans_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition set_trans_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((19 # 1))%Q
    | 2%positive => ((19 # 1) + (s IDset_trans_z))%Q
    | 3%positive => ((19 # 1) + (s IDset_trans_z))%Q
    | 4%positive => ((19 # 1) + (s IDset_trans_z))%Q
    | 5%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 6%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 7%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 8%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 9%positive => ((s IDset_trans_z))%Q
    | 10%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 11%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 12%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 13%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 14%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 15%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 16%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 17%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | 18%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(18 - (s IDset_trans_i)))%Q
    | 19%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(18 - (s IDset_trans_i)))%Q
    | 20%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(18 - (s IDset_trans_i)))%Q
    | 21%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(18 - (s IDset_trans_i)))%Q
    | 22%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(18 - (s IDset_trans_i)))%Q
    | 23%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(19 - (s IDset_trans_i)))%Q
    | 24%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(19 - (s IDset_trans_i)))%Q
    | 25%positive => ((1 # 1) + (s IDset_trans_z)
                      + max0(19 - (s IDset_trans_i)))%Q
    | 26%positive => ((s IDset_trans_z) + max0(19 - (s IDset_trans_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition set_trans_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (19
                                                            - (s IDset_trans_i)) (18
                                                                    - (s IDset_trans_i)));
                     (*-1 0*) F_max0_ge_0 (18 - (s IDset_trans_i))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*0 1*) F_max0_pre_decrement (19 - (s IDset_trans_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement (19 - (s IDset_trans_i)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem set_trans_ai_correct:
  forall s p' s', steps (g_start set_trans) s (g_edges set_trans) p' s' -> set_trans_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem set_trans_pot_correct:
  forall s p' s',
    steps (g_start set_trans) s (g_edges set_trans) p' s' ->
    (set_trans_pot (g_start set_trans) s >= set_trans_pot p' s')%Q.
Proof.
  check_lp set_trans_ai_correct set_trans_hints.
Qed.

