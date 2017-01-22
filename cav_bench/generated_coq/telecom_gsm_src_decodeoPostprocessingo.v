Require Import pasta.Pasta.

Notation IDPostprocessing_z := 1%positive.
Notation IDPostprocessing_S_dref_off650 := 2%positive.
Notation IDPostprocessing_k := 3%positive.
Notation IDPostprocessing_ltmp := 4%positive.
Notation IDPostprocessing_msr := 5%positive.
Notation IDPostprocessing_tmp := 6%positive.
Notation IDPostprocessing_S := 7%positive.
Notation IDPostprocessing_s := 8%positive.
Definition Postprocessing : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDPostprocessing_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDPostprocessing_msr
             (Some (EVar IDPostprocessing_S_dref_off650))),3%positive)::
             (3%positive,(AAssign IDPostprocessing_k (Some (ENum (160)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDPostprocessing_k
             (Some (EAdd (EVar IDPostprocessing_k) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDPostprocessing_k)
             s) <> (eval (ENum (0)) s))%Z)),12%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDPostprocessing_k)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDPostprocessing_S_dref_off650
             (Some (EVar IDPostprocessing_msr))),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDPostprocessing_tmp None),14%positive)::
             (14%positive,(AAssign IDPostprocessing_ltmp None),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,19%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDPostprocessing_msr None),20%positive)::
             (20%positive,(AAssign IDPostprocessing_ltmp
             (Some (EAdd (EVar IDPostprocessing_msr)
             (EVar IDPostprocessing_msr)))),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,25%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDPostprocessing_z (Some (EAdd (ENum (1))
             (EVar IDPostprocessing_z)))),5%positive)::nil
|}.

Definition Postprocessing_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDPostprocessing_z) <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 3%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_z) <= 0)%Z
    | 4%positive => (1 * (s IDPostprocessing_z) <= 0 /\ -1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -160 <= 0 /\ -1 * (s IDPostprocessing_k) + 160 <= 0)%Z
    | 5%positive => (1 * (s IDPostprocessing_k) + -160 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 6%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 7%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 8%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) <= 0 /\ -1 * (s IDPostprocessing_k) <= 0)%Z
    | 9%positive => (-1 * (s IDPostprocessing_k) <= 0 /\ 1 * (s IDPostprocessing_k) <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 10%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) <= 0 /\ -1 * (s IDPostprocessing_k) <= 0)%Z
    | 11%positive => (-1 * (s IDPostprocessing_k) <= 0 /\ 1 * (s IDPostprocessing_k) <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 12%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 13%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 14%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 15%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 16%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 17%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 18%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 19%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 20%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 21%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 22%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 23%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 24%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 25%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 26%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | 27%positive => (1 * (s IDPostprocessing_k) + -159 <= 0 /\ -1 * (s IDPostprocessing_z) <= 0)%Z
    | 28%positive => (-1 * (s IDPostprocessing_z) <= 0 /\ 1 * (s IDPostprocessing_k) + -159 <= 0)%Z
    | _ => False
  end.

Definition Postprocessing_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((159 # 1))%Q
    | 2%positive => ((159 # 1) + (s IDPostprocessing_z))%Q
    | 3%positive => ((159 # 1) + (s IDPostprocessing_z))%Q
    | 4%positive => (-(1 # 1) + (s IDPostprocessing_k)
                     + (s IDPostprocessing_z))%Q
    | 5%positive => (-(1 # 1) + (s IDPostprocessing_k)
                     + (s IDPostprocessing_z))%Q
    | 6%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 7%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 8%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 9%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 10%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 11%positive => ((s IDPostprocessing_z))%Q
    | 12%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 13%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 14%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 15%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 16%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 17%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 18%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 19%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 20%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 21%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 22%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 23%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 24%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 25%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 26%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 27%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | 28%positive => ((s IDPostprocessing_k) + (s IDPostprocessing_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition Postprocessing_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDPostprocessing_k))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDPostprocessing_k)) (0))) (F_max0_ge_0 ((s IDPostprocessing_k)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | _ => []
  end.


Theorem Postprocessing_ai_correct:
  forall s p' s', steps (g_start Postprocessing) s (g_edges Postprocessing) p' s' -> Postprocessing_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Postprocessing_pot_correct:
  forall s p' s',
    steps (g_start Postprocessing) s (g_edges Postprocessing) p' s' ->
    (Postprocessing_pot (g_start Postprocessing) s >= Postprocessing_pot p' s')%Q.
Proof.
  check_lp Postprocessing_ai_correct Postprocessing_hints.
Qed.

