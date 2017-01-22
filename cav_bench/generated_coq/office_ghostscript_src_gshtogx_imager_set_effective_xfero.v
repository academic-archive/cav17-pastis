Require Import pasta.Pasta.

Notation IDgx_imager_set_effective_xfer_z := 1%positive.
Notation IDgx_imager_set_effective_xfer_i := 2%positive.
Notation IDgx_imager_set_effective_xfer_pis := 3%positive.
Definition gx_imager_set_effective_xfer : graph := {|
  g_start := 1%positive;
  g_end := 29%positive;
  g_edges := (1%positive,(AAssign IDgx_imager_set_effective_xfer_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::
             (3%positive,ANone,27%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,22%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDgx_imager_set_effective_xfer_i
             (Some (ENum (0)))),7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDgx_imager_set_effective_xfer_i) s) <
             (eval (ENum (4)) s))%Z)),13%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDgx_imager_set_effective_xfer_i) s) >=
             (eval (ENum (4)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,29%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (14%positive,ANone,16%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDgx_imager_set_effective_xfer_i
             (Some (EAdd (EVar IDgx_imager_set_effective_xfer_i)
             (ENum (1))))),18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDgx_imager_set_effective_xfer_z
             (Some (EAdd (ENum (1))
             (EVar IDgx_imager_set_effective_xfer_z)))),21%positive)::
             (21%positive,AWeaken,9%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (23%positive,ANone,25%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,29%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::nil
|}.

Definition gx_imager_set_effective_xfer_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 4%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 6%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) <= 0)%Z
    | 8%positive => (-1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0)%Z
    | 10%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 4 <= 0)%Z
    | 11%positive => (-1 * (s IDgx_imager_set_effective_xfer_i) + 4 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -3 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -3 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) <= 0)%Z
    | 17%positive => (-1 * (s IDgx_imager_set_effective_xfer_i) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -3 <= 0)%Z
    | 18%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 1 <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0)%Z
    | 19%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 1 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 20%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 1 <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0)%Z
    | 21%positive => (1 * (s IDgx_imager_set_effective_xfer_i) + -4 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_i) + 1 <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 24%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 25%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 26%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 27%positive => (1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ -1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 28%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0 /\ 1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | 29%positive => (-1 * (s IDgx_imager_set_effective_xfer_z) <= 0)%Z
    | _ => False
  end.

Definition gx_imager_set_effective_xfer_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + max0((s IDgx_imager_set_effective_xfer_z)))%Q
    | 3%positive => ((4 # 1) + max0((s IDgx_imager_set_effective_xfer_z)))%Q
    | 4%positive => ((4 # 1) + max0((s IDgx_imager_set_effective_xfer_z)))%Q
    | 5%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 6%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 7%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                     + (s IDgx_imager_set_effective_xfer_z))%Q
    | 8%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                     + (s IDgx_imager_set_effective_xfer_z))%Q
    | 9%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                     + (s IDgx_imager_set_effective_xfer_z))%Q
    | 10%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 11%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 12%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 13%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 14%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 15%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 16%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 17%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 18%positive => ((5 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 19%positive => ((5 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 20%positive => ((5 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 21%positive => ((4 # 1) - (s IDgx_imager_set_effective_xfer_i)
                      + (s IDgx_imager_set_effective_xfer_z))%Q
    | 22%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 23%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 24%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 25%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 26%positive => ((4 # 1) + (s IDgx_imager_set_effective_xfer_z))%Q
    | 27%positive => ((4 # 1) + max0((s IDgx_imager_set_effective_xfer_z)))%Q
    | 28%positive => ((4 # 1) + max0((s IDgx_imager_set_effective_xfer_z)))%Q
    | 29%positive => ((s IDgx_imager_set_effective_xfer_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_imager_set_effective_xfer_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_imager_set_effective_xfer_z))) (F_check_ge ((s IDgx_imager_set_effective_xfer_z)) (0))]
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDgx_imager_set_effective_xfer_i)) (3
                                                                    - (s IDgx_imager_set_effective_xfer_i)));
                      (*-1 0*) F_max0_ge_0 (3
                                            - (s IDgx_imager_set_effective_xfer_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDgx_imager_set_effective_xfer_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDgx_imager_set_effective_xfer_i)))]
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
    | 26%positive => [(*-4 0*) F_one]
    | 27%positive => []
    | 28%positive => [(*-4 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_imager_set_effective_xfer_z))) (F_check_ge ((s IDgx_imager_set_effective_xfer_z)) (0))]
    | 29%positive => []
    | _ => []
  end.


Theorem gx_imager_set_effective_xfer_ai_correct:
  forall s p' s', steps (g_start gx_imager_set_effective_xfer) s (g_edges gx_imager_set_effective_xfer) p' s' -> gx_imager_set_effective_xfer_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_imager_set_effective_xfer_pot_correct:
  forall s p' s',
    steps (g_start gx_imager_set_effective_xfer) s (g_edges gx_imager_set_effective_xfer) p' s' ->
    (gx_imager_set_effective_xfer_pot (g_start gx_imager_set_effective_xfer) s >= gx_imager_set_effective_xfer_pot p' s')%Q.
Proof.
  check_lp gx_imager_set_effective_xfer_ai_correct gx_imager_set_effective_xfer_hints.
Qed.

