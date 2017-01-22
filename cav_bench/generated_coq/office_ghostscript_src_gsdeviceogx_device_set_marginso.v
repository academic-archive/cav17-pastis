Require Import pasta.Pasta.

Notation IDgx_device_set_margins_z := 1%positive.
Notation IDgx_device_set_margins__tmp := 2%positive.
Notation IDgx_device_set_margins_i := 3%positive.
Notation IDgx_device_set_margins_dev := 4%positive.
Notation IDgx_device_set_margins_margins := 5%positive.
Notation IDgx_device_set_margins_move_origin := 6%positive.
Definition gx_device_set_margins : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDgx_device_set_margins_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgx_device_set_margins__tmp
             (Some (EVar IDgx_device_set_margins_move_origin))),3%positive)::
             (3%positive,(AAssign IDgx_device_set_margins_i
             (Some (ENum (0)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_device_set_margins_i) s) <
             (eval (ENum (4)) s))%Z)),14%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_device_set_margins_i) s) >=
             (eval (ENum (4)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDgx_device_set_margins__tmp) s) <>
             (eval (ENum (0)) s))%Z)),10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDgx_device_set_margins__tmp) s) =
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,13%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDgx_device_set_margins_i
             (Some (EAdd (EVar IDgx_device_set_margins_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDgx_device_set_margins_z
             (Some (EAdd (ENum (1)) (EVar IDgx_device_set_margins_z)))),
             20%positive)::(20%positive,AWeaken,6%positive)::nil
|}.

Definition gx_device_set_margins_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_z) <= 0)%Z
    | 4%positive => (1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) <= 0 /\ -1 * (s IDgx_device_set_margins_i) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_device_set_margins_i) <= 0 /\ 1 * (s IDgx_device_set_margins_i) <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_z) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 7%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 4 <= 0)%Z
    | 8%positive => (-1 * (s IDgx_device_set_margins_i) + 4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 9%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 4 <= 0 /\ 1 * (s IDgx_device_set_margins__tmp) <= 0 /\ -1 * (s IDgx_device_set_margins__tmp) <= 0)%Z
    | 10%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 4 <= 0)%Z
    | 11%positive => (-1 * (s IDgx_device_set_margins_i) + 4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDgx_device_set_margins_i) + 4 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 14%positive => (-1 * (s IDgx_device_set_margins_i) <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDgx_device_set_margins_i) + -3 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) <= 0)%Z
    | 16%positive => (-1 * (s IDgx_device_set_margins_i) <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -3 <= 0)%Z
    | 17%positive => (-1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 1 <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 1 <= 0 /\ -1 * (s IDgx_device_set_margins_z) <= 0)%Z
    | 19%positive => (-1 * (s IDgx_device_set_margins_z) <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 1 <= 0 /\ 1 * (s IDgx_device_set_margins_i) + -4 <= 0)%Z
    | 20%positive => (1 * (s IDgx_device_set_margins_i) + -4 <= 0 /\ -1 * (s IDgx_device_set_margins_i) + 1 <= 0 /\ -1 * (s IDgx_device_set_margins_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gx_device_set_margins_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDgx_device_set_margins_z))%Q
    | 3%positive => ((4 # 1) + (s IDgx_device_set_margins_z))%Q
    | 4%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z))%Q
    | 5%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z))%Q
    | 6%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z))%Q
    | 7%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z))%Q
    | 8%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z)
                     - max0(3 - (s IDgx_device_set_margins_i)))%Q
    | 9%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                     + (s IDgx_device_set_margins_z)
                     - max0(3 - (s IDgx_device_set_margins_i)))%Q
    | 10%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z)
                      - max0(3 - (s IDgx_device_set_margins_i)))%Q
    | 11%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z)
                      - max0(3 - (s IDgx_device_set_margins_i)))%Q
    | 12%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z)
                      - max0(3 - (s IDgx_device_set_margins_i)))%Q
    | 13%positive => ((s IDgx_device_set_margins_z))%Q
    | 14%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 15%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 16%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 17%positive => ((5 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 18%positive => ((5 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 19%positive => ((5 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | 20%positive => ((4 # 1) - (s IDgx_device_set_margins_i)
                      + (s IDgx_device_set_margins_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_device_set_margins_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*0 1*) F_max0_ge_0 (3 - (s IDgx_device_set_margins_i))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDgx_device_set_margins_i)) (3
                                                                    - (s IDgx_device_set_margins_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDgx_device_set_margins_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDgx_device_set_margins_i)))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDgx_device_set_margins_i)) (3
                                                                    - (s IDgx_device_set_margins_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDgx_device_set_margins_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDgx_device_set_margins_i)))]
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


Theorem gx_device_set_margins_ai_correct:
  forall s p' s', steps (g_start gx_device_set_margins) s (g_edges gx_device_set_margins) p' s' -> gx_device_set_margins_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_device_set_margins_pot_correct:
  forall s p' s',
    steps (g_start gx_device_set_margins) s (g_edges gx_device_set_margins) p' s' ->
    (gx_device_set_margins_pot (g_start gx_device_set_margins) s >= gx_device_set_margins_pot p' s')%Q.
Proof.
  check_lp gx_device_set_margins_ai_correct gx_device_set_margins_hints.
Qed.

