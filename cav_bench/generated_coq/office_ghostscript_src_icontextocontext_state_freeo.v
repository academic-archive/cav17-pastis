Require Import pasta.Pasta.

Notation IDcontext_state_free_z := 1%positive.
Notation IDcontext_state_free_i := 2%positive.
Notation IDcontext_state_free_mem := 3%positive.
Notation IDcontext_state_free_pcst := 4%positive.
Definition context_state_free : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDcontext_state_free_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcontext_state_free_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcontext_state_free_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcontext_state_free_i) s) <
             (eval (ENum (4)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcontext_state_free_i) s) >=
             (eval (ENum (4)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (11%positive,ANone,15%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,15%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcontext_state_free_i
             (Some (EAdd (EVar IDcontext_state_free_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcontext_state_free_z
             (Some (EAdd (ENum (1)) (EVar IDcontext_state_free_z)))),
             20%positive)::(20%positive,AWeaken,7%positive)::nil
|}.

Definition context_state_free_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ 1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ 1 * (s IDcontext_state_free_i) <= 0 /\ 1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0 /\ 1 * (s IDcontext_state_free_i) + -4 <= 0)%Z
    | 8%positive => (1 * (s IDcontext_state_free_i) + -4 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) + 4 <= 0)%Z
    | 9%positive => (-1 * (s IDcontext_state_free_i) + 4 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) + -4 <= 0)%Z
    | 10%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) + -3 <= 0)%Z
    | 11%positive => (1 * (s IDcontext_state_free_i) + -3 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDcontext_state_free_i) + -3 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDcontext_state_free_i) + -3 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcontext_state_free_i) <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0 /\ 1 * (s IDcontext_state_free_i) + -3 <= 0)%Z
    | 17%positive => (-1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) + 1 <= 0 /\ 1 * (s IDcontext_state_free_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDcontext_state_free_i) + -4 <= 0 /\ -1 * (s IDcontext_state_free_i) + 1 <= 0 /\ -1 * (s IDcontext_state_free_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcontext_state_free_z) <= 0 /\ -1 * (s IDcontext_state_free_i) + 1 <= 0 /\ 1 * (s IDcontext_state_free_i) + -4 <= 0)%Z
    | 20%positive => (1 * (s IDcontext_state_free_i) + -4 <= 0 /\ -1 * (s IDcontext_state_free_i) + 1 <= 0 /\ -1 * (s IDcontext_state_free_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition context_state_free_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDcontext_state_free_z))%Q
    | 3%positive => ((4 # 1) + (s IDcontext_state_free_z))%Q
    | 4%positive => ((4 # 1) + (s IDcontext_state_free_z))%Q
    | 5%positive => ((s IDcontext_state_free_z)
                     + max0(4 - (s IDcontext_state_free_i)))%Q
    | 6%positive => ((s IDcontext_state_free_z)
                     + max0(4 - (s IDcontext_state_free_i)))%Q
    | 7%positive => ((s IDcontext_state_free_z)
                     + max0(4 - (s IDcontext_state_free_i)))%Q
    | 8%positive => ((s IDcontext_state_free_z)
                     + max0(4 - (s IDcontext_state_free_i)))%Q
    | 9%positive => ((s IDcontext_state_free_z))%Q
    | 10%positive => ((s IDcontext_state_free_z)
                      + max0(4 - (s IDcontext_state_free_i)))%Q
    | 11%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 12%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 13%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 14%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 15%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 16%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 17%positive => ((5 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 18%positive => ((5 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 19%positive => ((5 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | 20%positive => ((4 # 1) - (s IDcontext_state_free_i)
                      + (s IDcontext_state_free_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition context_state_free_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDcontext_state_free_i)) (3
                                                                    - (s IDcontext_state_free_i)));
                     (*-1 0*) F_max0_ge_0 (3 - (s IDcontext_state_free_i))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDcontext_state_free_i))) (F_check_ge (4
                                                                    - (s IDcontext_state_free_i)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDcontext_state_free_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDcontext_state_free_i)))]
    | _ => []
  end.


Theorem context_state_free_ai_correct:
  forall s p' s', steps (g_start context_state_free) s (g_edges context_state_free) p' s' -> context_state_free_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem context_state_free_pot_correct:
  forall s p' s',
    steps (g_start context_state_free) s (g_edges context_state_free) p' s' ->
    (context_state_free_pot (g_start context_state_free) s >= context_state_free_pot p' s')%Q.
Proof.
  check_lp context_state_free_ai_correct context_state_free_hints.
Qed.

