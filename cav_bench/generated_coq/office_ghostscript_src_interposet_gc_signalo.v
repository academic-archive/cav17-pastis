Require Import pasta.Pasta.

Notation IDset_gc_signal_z := 1%positive.
Notation IDset_gc_signal__tmp := 2%positive.
Notation IDset_gc_signal_i := 3%positive.
Notation IDset_gc_signal_psignal := 4%positive.
Notation IDset_gc_signal_value := 5%positive.
Definition set_gc_signal : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDset_gc_signal_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDset_gc_signal_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDset_gc_signal__tmp
             (Some (EVar IDset_gc_signal_value))),5%positive)::
             (5%positive,(AAssign IDset_gc_signal_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDset_gc_signal_i)
             s) < (eval (ENum (4)) s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDset_gc_signal_i)
             s) >= (eval (ENum (4)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,14%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDset_gc_signal_i
             (Some (EAdd (EVar IDset_gc_signal_i) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDset_gc_signal_z (Some (EAdd (ENum (1))
             (EVar IDset_gc_signal_z)))),19%positive)::
             (19%positive,AWeaken,8%positive)::nil
|}.

Definition set_gc_signal_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0)%Z
    | 3%positive => (-1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0)%Z
    | 4%positive => (-1 * (s IDset_gc_signal_i) <= 0 /\ 1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0)%Z
    | 5%positive => (-1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0)%Z
    | 6%positive => (1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_i) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0)%Z
    | 7%positive => (-1 * (s IDset_gc_signal_i) <= 0 /\ 1 * (s IDset_gc_signal_i) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_z) <= 0)%Z
    | 8%positive => (-1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0 /\ 1 * (s IDset_gc_signal_i) + -4 <= 0)%Z
    | 9%positive => (1 * (s IDset_gc_signal_i) + -4 <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) + 4 <= 0)%Z
    | 10%positive => (-1 * (s IDset_gc_signal_i) + 4 <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_i) + -4 <= 0)%Z
    | 11%positive => (-1 * (s IDset_gc_signal_i) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_i) + -3 <= 0)%Z
    | 12%positive => (1 * (s IDset_gc_signal_i) + -3 <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0)%Z
    | 13%positive => (-1 * (s IDset_gc_signal_i) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_i) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDset_gc_signal_i) + -3 <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) <= 0)%Z
    | 15%positive => (-1 * (s IDset_gc_signal_i) <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0 /\ 1 * (s IDset_gc_signal_i) + -3 <= 0)%Z
    | 16%positive => (-1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) + 1 <= 0 /\ 1 * (s IDset_gc_signal_i) + -4 <= 0)%Z
    | 17%positive => (1 * (s IDset_gc_signal_i) + -4 <= 0 /\ -1 * (s IDset_gc_signal_i) + 1 <= 0 /\ -1 * (s IDset_gc_signal_z) <= 0)%Z
    | 18%positive => (-1 * (s IDset_gc_signal_z) <= 0 /\ -1 * (s IDset_gc_signal_i) + 1 <= 0 /\ 1 * (s IDset_gc_signal_i) + -4 <= 0)%Z
    | 19%positive => (1 * (s IDset_gc_signal_i) + -4 <= 0 /\ -1 * (s IDset_gc_signal_i) + 1 <= 0 /\ -1 * (s IDset_gc_signal_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition set_gc_signal_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDset_gc_signal_z))%Q
    | 3%positive => ((4 # 1) + (s IDset_gc_signal_z))%Q
    | 4%positive => ((4 # 1) + (s IDset_gc_signal_z))%Q
    | 5%positive => ((4 # 1) + (s IDset_gc_signal_z))%Q
    | 6%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | 7%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | 8%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | 9%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | 10%positive => ((s IDset_gc_signal_z))%Q
    | 11%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | 12%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(3 - (s IDset_gc_signal_i)))%Q
    | 13%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(3 - (s IDset_gc_signal_i)))%Q
    | 14%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(3 - (s IDset_gc_signal_i)))%Q
    | 15%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(3 - (s IDset_gc_signal_i)))%Q
    | 16%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(4 - (s IDset_gc_signal_i)))%Q
    | 17%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(4 - (s IDset_gc_signal_i)))%Q
    | 18%positive => ((1 # 1) + (s IDset_gc_signal_z)
                      + max0(4 - (s IDset_gc_signal_i)))%Q
    | 19%positive => ((s IDset_gc_signal_z) + max0(4 - (s IDset_gc_signal_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition set_gc_signal_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDset_gc_signal_i)) (3
                                                                    - (s IDset_gc_signal_i)));
                     (*-1 0*) F_max0_ge_0 (3 - (s IDset_gc_signal_i))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDset_gc_signal_i)) (1)]
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


Theorem set_gc_signal_ai_correct:
  forall s p' s', steps (g_start set_gc_signal) s (g_edges set_gc_signal) p' s' -> set_gc_signal_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem set_gc_signal_pot_correct:
  forall s p' s',
    steps (g_start set_gc_signal) s (g_edges set_gc_signal) p' s' ->
    (set_gc_signal_pot (g_start set_gc_signal) s >= set_gc_signal_pot p' s')%Q.
Proof.
  check_lp set_gc_signal_ai_correct set_gc_signal_hints.
Qed.

