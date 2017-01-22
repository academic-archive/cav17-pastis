Require Import pasta.Pasta.

Notation IDgs_imager_state_initialize_z := 1%positive.
Notation IDgs_imager_state_initialize__tmp := 2%positive.
Notation IDgs_imager_state_initialize_i := 3%positive.
Notation IDgs_imager_state_initialize_mem := 4%positive.
Notation IDgs_imager_state_initialize_pis := 5%positive.
Definition gs_imager_state_initialize : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDgs_imager_state_initialize_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgs_imager_state_initialize_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgs_imager_state_initialize_i) s) <
             (eval (ENum (2)) s))%Z)),19%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDgs_imager_state_initialize_i) s) >=
             (eval (ENum (2)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,15%positive)::(9%positive,ANone,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDgs_imager_state_initialize__tmp
             (Some (ENum (0)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,18%positive)::
             (15%positive,(AAssign IDgs_imager_state_initialize__tmp
             (Some (ENum (-25)))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDgs_imager_state_initialize_i
             (Some (EAdd (EVar IDgs_imager_state_initialize_i) (ENum (1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDgs_imager_state_initialize_z
             (Some (EAdd (ENum (1)) (EVar IDgs_imager_state_initialize_z)))),
             25%positive)::(25%positive,AWeaken,5%positive)::nil
|}.

Definition gs_imager_state_initialize_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgs_imager_state_initialize_i) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) <= 0 /\ 1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 6%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 7%positive => (-1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 8%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 9%positive => (-1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 10%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 11%positive => (-1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 12%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 13%positive => (-1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ 1 * (s IDgs_imager_state_initialize__tmp) <= 0 /\ -1 * (s IDgs_imager_state_initialize__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDgs_imager_state_initialize__tmp) <= 0 /\ 1 * (s IDgs_imager_state_initialize__tmp) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 15%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 16%positive => (-1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ 1 * (s IDgs_imager_state_initialize__tmp) + 25 <= 0 /\ -1 * (s IDgs_imager_state_initialize__tmp) + -25 <= 0)%Z
    | 17%positive => (-1 * (s IDgs_imager_state_initialize__tmp) + -25 <= 0 /\ 1 * (s IDgs_imager_state_initialize__tmp) + 25 <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0)%Z
    | 18%positive => (1 * (s IDgs_imager_state_initialize__tmp) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize__tmp) + -25 <= 0)%Z
    | 19%positive => (-1 * (s IDgs_imager_state_initialize_i) <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDgs_imager_state_initialize_i) + -1 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) <= 0)%Z
    | 21%positive => (-1 * (s IDgs_imager_state_initialize_i) <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 1 <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 23%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 1 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) <= 0)%Z
    | 24%positive => (-1 * (s IDgs_imager_state_initialize_z) <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 1 <= 0 /\ 1 * (s IDgs_imager_state_initialize_i) + -2 <= 0)%Z
    | 25%positive => (1 * (s IDgs_imager_state_initialize_i) + -2 <= 0 /\ -1 * (s IDgs_imager_state_initialize_i) + 1 <= 0 /\ -1 * (s IDgs_imager_state_initialize_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gs_imager_state_initialize_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1))%Q
    | 2%positive => ((2 # 1) + (s IDgs_imager_state_initialize_z))%Q
    | 3%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 4%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 5%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 6%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 7%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 8%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 9%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                     + (s IDgs_imager_state_initialize_z))%Q
    | 10%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 11%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 12%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 13%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 14%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 15%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 16%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 17%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 18%positive => ((s IDgs_imager_state_initialize_z))%Q
    | 19%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 20%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 21%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 22%positive => ((3 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 23%positive => ((3 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 24%positive => ((3 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | 25%positive => ((2 # 1) - (s IDgs_imager_state_initialize_i)
                      + (s IDgs_imager_state_initialize_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_imager_state_initialize_hints (p : node) (s : state) := 
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
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (2
                                                             - (s IDgs_imager_state_initialize_i)) (1
                                                                    - (s IDgs_imager_state_initialize_i)));
                      (*-1 0*) F_max0_ge_0 (1
                                            - (s IDgs_imager_state_initialize_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDgs_imager_state_initialize_i)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDgs_imager_state_initialize_i)))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (2
                                                             - (s IDgs_imager_state_initialize_i)) (1
                                                                    - (s IDgs_imager_state_initialize_i)));
                      (*-1 0*) F_max0_ge_0 (1
                                            - (s IDgs_imager_state_initialize_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDgs_imager_state_initialize_i)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDgs_imager_state_initialize_i)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | _ => []
  end.


Theorem gs_imager_state_initialize_ai_correct:
  forall s p' s', steps (g_start gs_imager_state_initialize) s (g_edges gs_imager_state_initialize) p' s' -> gs_imager_state_initialize_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_imager_state_initialize_pot_correct:
  forall s p' s',
    steps (g_start gs_imager_state_initialize) s (g_edges gs_imager_state_initialize) p' s' ->
    (gs_imager_state_initialize_pot (g_start gs_imager_state_initialize) s >= gs_imager_state_initialize_pot p' s')%Q.
Proof.
  check_lp gs_imager_state_initialize_ai_correct gs_imager_state_initialize_hints.
Qed.

