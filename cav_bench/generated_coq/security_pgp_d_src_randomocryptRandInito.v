Require Import pasta.Pasta.

Notation IDcryptRandInit_z := 1%positive.
Notation IDcryptRandInit_i := 2%positive.
Notation IDcryptRandInit_randSeedOpen := 3%positive.
Notation IDcryptRandInit_cfb := 4%positive.
Definition cryptRandInit : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDcryptRandInit_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDcryptRandInit_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcryptRandInit_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDcryptRandInit_i)
             s) < (eval (ENum (24)) s))%Z)),14%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDcryptRandInit_i)
             s) >= (eval (ENum (24)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::(9%positive,ANone,11%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDcryptRandInit_randSeedOpen
             (Some (ENum (1)))),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcryptRandInit_i
             (Some (EAdd (EVar IDcryptRandInit_i) (ENum (1))))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcryptRandInit_z (Some (EAdd (ENum (1))
             (EVar IDcryptRandInit_z)))),20%positive)::
             (20%positive,AWeaken,7%positive)::nil
|}.

Definition cryptRandInit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcryptRandInit_i) <= 0 /\ 1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) <= 0 /\ -1 * (s IDcryptRandInit_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcryptRandInit_i) <= 0 /\ 1 * (s IDcryptRandInit_i) <= 0 /\ 1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 8%positive => (1 * (s IDcryptRandInit_i) + -24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) + 24 <= 0)%Z
    | 9%positive => (-1 * (s IDcryptRandInit_i) + 24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 10%positive => (1 * (s IDcryptRandInit_i) + -24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) + 24 <= 0)%Z
    | 11%positive => (-1 * (s IDcryptRandInit_i) + 24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 12%positive => (1 * (s IDcryptRandInit_i) + -24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) + 24 <= 0 /\ 1 * (s IDcryptRandInit_randSeedOpen) + -1 <= 0 /\ -1 * (s IDcryptRandInit_randSeedOpen) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDcryptRandInit_randSeedOpen) + 1 <= 0 /\ 1 * (s IDcryptRandInit_randSeedOpen) + -1 <= 0 /\ -1 * (s IDcryptRandInit_i) + 24 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 14%positive => (-1 * (s IDcryptRandInit_i) <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) + -23 <= 0)%Z
    | 15%positive => (1 * (s IDcryptRandInit_i) + -23 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcryptRandInit_i) <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0 /\ 1 * (s IDcryptRandInit_i) + -23 <= 0)%Z
    | 17%positive => (-1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) + 1 <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 18%positive => (1 * (s IDcryptRandInit_i) + -24 <= 0 /\ -1 * (s IDcryptRandInit_i) + 1 <= 0 /\ -1 * (s IDcryptRandInit_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcryptRandInit_z) <= 0 /\ -1 * (s IDcryptRandInit_i) + 1 <= 0 /\ 1 * (s IDcryptRandInit_i) + -24 <= 0)%Z
    | 20%positive => (1 * (s IDcryptRandInit_i) + -24 <= 0 /\ -1 * (s IDcryptRandInit_i) + 1 <= 0 /\ -1 * (s IDcryptRandInit_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition cryptRandInit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((24 # 1))%Q
    | 2%positive => ((24 # 1) + (s IDcryptRandInit_z))%Q
    | 3%positive => ((24 # 1) + (s IDcryptRandInit_z))%Q
    | 4%positive => ((24 # 1) + (s IDcryptRandInit_z))%Q
    | 5%positive => ((24 # 1) - (s IDcryptRandInit_i) + (s IDcryptRandInit_z))%Q
    | 6%positive => ((24 # 1) - (s IDcryptRandInit_i) + (s IDcryptRandInit_z))%Q
    | 7%positive => ((24 # 1) - (s IDcryptRandInit_i) + (s IDcryptRandInit_z))%Q
    | 8%positive => ((24 # 1) - (s IDcryptRandInit_i) + (s IDcryptRandInit_z))%Q
    | 9%positive => ((24 # 1) - (s IDcryptRandInit_i) + (s IDcryptRandInit_z))%Q
    | 10%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 11%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 12%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 13%positive => ((s IDcryptRandInit_z))%Q
    | 14%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 15%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 16%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 17%positive => ((25 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 18%positive => ((25 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 19%positive => ((25 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | 20%positive => ((24 # 1) - (s IDcryptRandInit_i)
                      + (s IDcryptRandInit_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cryptRandInit_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (24
                                                             - (s IDcryptRandInit_i)) (23
                                                                    - (s IDcryptRandInit_i)));
                      (*-1 0*) F_max0_ge_0 (23 - (s IDcryptRandInit_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (24
                                                                    - (s IDcryptRandInit_i)) (0))) (F_max0_ge_0 (24
                                                                    - (s IDcryptRandInit_i)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem cryptRandInit_ai_correct:
  forall s p' s', steps (g_start cryptRandInit) s (g_edges cryptRandInit) p' s' -> cryptRandInit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cryptRandInit_pot_correct:
  forall s p' s',
    steps (g_start cryptRandInit) s (g_edges cryptRandInit) p' s' ->
    (cryptRandInit_pot (g_start cryptRandInit) s >= cryptRandInit_pot p' s')%Q.
Proof.
  check_lp cryptRandInit_ai_correct cryptRandInit_hints.
Qed.

