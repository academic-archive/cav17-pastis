Require Import pasta.Pasta.

Notation IDPascalToC_z := 1%positive.
Notation IDPascalToC_i := 2%positive.
Notation IDPascalToC_j := 3%positive.
Notation IDPascalToC_s_dref_off0 := 4%positive.
Notation IDPascalToC_s := 5%positive.
Definition PascalToC : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDPascalToC_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDPascalToC_i (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDPascalToC_j
             (Some (EVar IDPascalToC_s_dref_off0))),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDPascalToC_i) s) <
             (eval (EVar IDPascalToC_j) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDPascalToC_i) s) >=
             (eval (EVar IDPascalToC_j) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDPascalToC_i
             (Some (EAdd (EVar IDPascalToC_i) (ENum (1))))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDPascalToC_z (Some (EAdd (ENum (1))
             (EVar IDPascalToC_z)))),15%positive)::
             (15%positive,AWeaken,6%positive)::nil
|}.

Definition PascalToC_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 3%positive => (-1 * (s IDPascalToC_z) <= 0 /\ 1 * (s IDPascalToC_z) <= 0 /\ 1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_i) <= 0)%Z
    | 4%positive => (-1 * (s IDPascalToC_i) <= 0 /\ 1 * (s IDPascalToC_i) <= 0 /\ 1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 5%positive => (-1 * (s IDPascalToC_z) <= 0 /\ 1 * (s IDPascalToC_z) <= 0 /\ 1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_i) <= 0)%Z
    | 6%positive => (-1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 7%positive => (-1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_i)+ 1 * (s IDPascalToC_j) <= 0)%Z
    | 8%positive => (-1 * (s IDPascalToC_i)+ 1 * (s IDPascalToC_j) <= 0 /\ -1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 9%positive => (-1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_i) <= 0 /\ 1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) + 1 <= 0)%Z
    | 10%positive => (1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) + 1 <= 0 /\ -1 * (s IDPascalToC_i) <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 11%positive => (-1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_i) <= 0 /\ 1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_i) + 1 <= 0 /\ 1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) <= 0)%Z
    | 13%positive => (1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) <= 0 /\ -1 * (s IDPascalToC_i) + 1 <= 0 /\ -1 * (s IDPascalToC_z) <= 0)%Z
    | 14%positive => (-1 * (s IDPascalToC_z) <= 0 /\ -1 * (s IDPascalToC_i) + 1 <= 0 /\ 1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) <= 0)%Z
    | 15%positive => (1 * (s IDPascalToC_i)+ -1 * (s IDPascalToC_j) <= 0 /\ -1 * (s IDPascalToC_i) + 1 <= 0 /\ -1 * (s IDPascalToC_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition PascalToC_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDPascalToC_s_dref_off0)))%Q
    | 2%positive => ((s IDPascalToC_z) + max0((s IDPascalToC_s_dref_off0)))%Q
    | 3%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_s_dref_off0)))%Q
    | 4%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 5%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 6%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 7%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 8%positive => ((s IDPascalToC_z))%Q
    | 9%positive => ((s IDPascalToC_z)
                     + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 10%positive => ((1 # 1) + (s IDPascalToC_z)
                      + max0(-1 - (s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 11%positive => ((1 # 1) + (s IDPascalToC_z)
                      + max0(-1 - (s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 12%positive => ((1 # 1) + (s IDPascalToC_z)
                      + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 13%positive => ((1 # 1) + (s IDPascalToC_z)
                      + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 14%positive => ((1 # 1) + (s IDPascalToC_z)
                      + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | 15%positive => ((s IDPascalToC_z)
                      + max0(-(s IDPascalToC_i) + (s IDPascalToC_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition PascalToC_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDPascalToC_i)
                                                            + (s IDPascalToC_j)) (-1
                                                                    - (s IDPascalToC_i)
                                                                    + (s IDPascalToC_j)));
                     (*-1 0*) F_max0_ge_0 (-1 - (s IDPascalToC_i)
                                           + (s IDPascalToC_j))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDPascalToC_i)
                                                    + (s IDPascalToC_j)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem PascalToC_ai_correct:
  forall s p' s', steps (g_start PascalToC) s (g_edges PascalToC) p' s' -> PascalToC_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem PascalToC_pot_correct:
  forall s p' s',
    steps (g_start PascalToC) s (g_edges PascalToC) p' s' ->
    (PascalToC_pot (g_start PascalToC) s >= PascalToC_pot p' s')%Q.
Proof.
  check_lp PascalToC_ai_correct PascalToC_hints.
Qed.

