Require Import pasta.Pasta.

Notation IDialloc_reset_free_z := 1%positive.
Notation IDialloc_reset_free_i := 2%positive.
Notation IDialloc_reset_free_mem := 3%positive.
Definition ialloc_reset_free : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDialloc_reset_free_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDialloc_reset_free_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDialloc_reset_free_i) s) <
             (eval (ENum (201)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDialloc_reset_free_i) s) >=
             (eval (ENum (201)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDialloc_reset_free_i
             (Some (EAdd (EVar IDialloc_reset_free_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDialloc_reset_free_z
             (Some (EAdd (ENum (1)) (EVar IDialloc_reset_free_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition ialloc_reset_free_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0)%Z
    | 3%positive => (-1 * (s IDialloc_reset_free_z) <= 0 /\ 1 * (s IDialloc_reset_free_z) <= 0 /\ 1 * (s IDialloc_reset_free_i) <= 0 /\ -1 * (s IDialloc_reset_free_i) <= 0)%Z
    | 4%positive => (-1 * (s IDialloc_reset_free_i) <= 0 /\ 1 * (s IDialloc_reset_free_i) <= 0 /\ 1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0)%Z
    | 5%positive => (-1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_i) <= 0 /\ 1 * (s IDialloc_reset_free_i) + -201 <= 0)%Z
    | 6%positive => (1 * (s IDialloc_reset_free_i) + -201 <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_i) + 201 <= 0)%Z
    | 7%positive => (-1 * (s IDialloc_reset_free_i) + 201 <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0 /\ 1 * (s IDialloc_reset_free_i) + -201 <= 0)%Z
    | 8%positive => (-1 * (s IDialloc_reset_free_i) <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0 /\ 1 * (s IDialloc_reset_free_i) + -200 <= 0)%Z
    | 9%positive => (1 * (s IDialloc_reset_free_i) + -200 <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_i) <= 0)%Z
    | 10%positive => (-1 * (s IDialloc_reset_free_i) <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0 /\ 1 * (s IDialloc_reset_free_i) + -200 <= 0)%Z
    | 11%positive => (-1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_i) + 1 <= 0 /\ 1 * (s IDialloc_reset_free_i) + -201 <= 0)%Z
    | 12%positive => (1 * (s IDialloc_reset_free_i) + -201 <= 0 /\ -1 * (s IDialloc_reset_free_i) + 1 <= 0 /\ -1 * (s IDialloc_reset_free_z) <= 0)%Z
    | 13%positive => (-1 * (s IDialloc_reset_free_z) <= 0 /\ -1 * (s IDialloc_reset_free_i) + 1 <= 0 /\ 1 * (s IDialloc_reset_free_i) + -201 <= 0)%Z
    | 14%positive => (1 * (s IDialloc_reset_free_i) + -201 <= 0 /\ -1 * (s IDialloc_reset_free_i) + 1 <= 0 /\ -1 * (s IDialloc_reset_free_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ialloc_reset_free_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((201 # 1))%Q
    | 2%positive => ((201 # 1) + (s IDialloc_reset_free_z))%Q
    | 3%positive => ((s IDialloc_reset_free_z)
                     + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 4%positive => ((s IDialloc_reset_free_z)
                     + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 5%positive => ((s IDialloc_reset_free_z)
                     + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 6%positive => ((s IDialloc_reset_free_z)
                     + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 7%positive => ((s IDialloc_reset_free_z))%Q
    | 8%positive => ((s IDialloc_reset_free_z)
                     + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 9%positive => ((1 # 1) + (s IDialloc_reset_free_z)
                     + max0(200 - (s IDialloc_reset_free_i)))%Q
    | 10%positive => ((1 # 1) + (s IDialloc_reset_free_z)
                      + max0(200 - (s IDialloc_reset_free_i)))%Q
    | 11%positive => ((1 # 1) + (s IDialloc_reset_free_z)
                      + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 12%positive => ((1 # 1) + (s IDialloc_reset_free_z)
                      + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 13%positive => ((1 # 1) + (s IDialloc_reset_free_z)
                      + max0(201 - (s IDialloc_reset_free_i)))%Q
    | 14%positive => ((s IDialloc_reset_free_z)
                      + max0(201 - (s IDialloc_reset_free_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ialloc_reset_free_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (201
                                                            - (s IDialloc_reset_free_i)) (200
                                                                    - (s IDialloc_reset_free_i)));
                     (*-1 0*) F_max0_ge_0 (200 - (s IDialloc_reset_free_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (201
                                                    - (s IDialloc_reset_free_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem ialloc_reset_free_ai_correct:
  forall s p' s', steps (g_start ialloc_reset_free) s (g_edges ialloc_reset_free) p' s' -> ialloc_reset_free_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ialloc_reset_free_pot_correct:
  forall s p' s',
    steps (g_start ialloc_reset_free) s (g_edges ialloc_reset_free) p' s' ->
    (ialloc_reset_free_pot (g_start ialloc_reset_free) s >= ialloc_reset_free_pot p' s')%Q.
Proof.
  check_lp ialloc_reset_free_ai_correct ialloc_reset_free_hints.
Qed.

