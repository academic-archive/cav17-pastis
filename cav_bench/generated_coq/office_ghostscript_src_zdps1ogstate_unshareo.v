Require Import pasta.Pasta.

Notation IDgstate_unshare_z := 1%positive.
Notation IDgstate_unshare__tmp := 2%positive.
Notation IDgstate_unshare_i := 3%positive.
Notation IDgstate_unshare_op := 4%positive.
Definition gstate_unshare : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDgstate_unshare_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,7%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDgstate_unshare__tmp (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,25%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,22%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDgstate_unshare_i (Some (ENum (25)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDgstate_unshare_i
             (Some (EAdd (EVar IDgstate_unshare_i) (ENum (-1))))),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgstate_unshare_i) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),19%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgstate_unshare_i) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDgstate_unshare__tmp (Some (ENum (0)))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,AWeaken,25%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDgstate_unshare_z (Some (EAdd (ENum (1))
             (EVar IDgstate_unshare_z)))),11%positive)::
             (22%positive,(AAssign IDgstate_unshare__tmp
             (Some (ENum (-25)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition gstate_unshare_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0)%Z
    | 4%positive => (1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare__tmp) <= 0 /\ -1 * (s IDgstate_unshare__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDgstate_unshare__tmp) <= 0 /\ 1 * (s IDgstate_unshare__tmp) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 7%positive => (1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0)%Z
    | 9%positive => (1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 10%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -25 <= 0 /\ -1 * (s IDgstate_unshare_i) + 25 <= 0)%Z
    | 11%positive => (1 * (s IDgstate_unshare_i) + -25 <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 12%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -25 <= 0)%Z
    | 13%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -24 <= 0)%Z
    | 14%positive => (1 * (s IDgstate_unshare_i) + -24 <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 15%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -1 <= 0 /\ -1 * (s IDgstate_unshare_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDgstate_unshare_i) + 1 <= 0 /\ 1 * (s IDgstate_unshare_i) + -1 <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 17%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -1 <= 0 /\ -1 * (s IDgstate_unshare_i) + 1 <= 0 /\ 1 * (s IDgstate_unshare__tmp) <= 0 /\ -1 * (s IDgstate_unshare__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDgstate_unshare__tmp) <= 0 /\ 1 * (s IDgstate_unshare__tmp) <= 0 /\ -1 * (s IDgstate_unshare_i) + 1 <= 0 /\ 1 * (s IDgstate_unshare_i) + -1 <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 19%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -24 <= 0)%Z
    | 20%positive => (1 * (s IDgstate_unshare_i) + -24 <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 21%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_i) + -24 <= 0)%Z
    | 22%positive => (1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0 /\ 1 * (s IDgstate_unshare__tmp) + 25 <= 0 /\ -1 * (s IDgstate_unshare__tmp) + -25 <= 0)%Z
    | 24%positive => (-1 * (s IDgstate_unshare__tmp) + -25 <= 0 /\ 1 * (s IDgstate_unshare__tmp) + 25 <= 0 /\ 1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0)%Z
    | 25%positive => (1 * (s IDgstate_unshare__tmp) <= 0 /\ -1 * (s IDgstate_unshare_z) <= 0 /\ -1 * (s IDgstate_unshare__tmp) + -25 <= 0)%Z
    | _ => False
  end.

Definition gstate_unshare_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((23 # 1))%Q
    | 2%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 3%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 4%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 5%positive => ((s IDgstate_unshare_z)
                     + (23 # 25) * max0(25 + (s IDgstate_unshare__tmp)))%Q
    | 6%positive => ((s IDgstate_unshare_z)
                     + (23 # 25) * max0(25 + (s IDgstate_unshare__tmp)))%Q
    | 7%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 8%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 9%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 10%positive => (-(2 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 11%positive => (-(2 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 12%positive => (-(2 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 13%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 14%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 15%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 16%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 17%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 18%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 19%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 20%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 21%positive => (-(1 # 1) + (s IDgstate_unshare_i)
                      + (s IDgstate_unshare_z))%Q
    | 22%positive => ((23 # 1) + (s IDgstate_unshare_z))%Q
    | 23%positive => ((s IDgstate_unshare_z)
                      + (23 # 25) * max0(-(s IDgstate_unshare__tmp)))%Q
    | 24%positive => ((s IDgstate_unshare_z)
                      + (23 # 25) * max0(-(s IDgstate_unshare__tmp)))%Q
    | 25%positive => ((s IDgstate_unshare_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gstate_unshare_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-0.92 0*) F_binom_monotonic 1 (F_max0_ge_0 (25
                                                                   + 
                                                                   (s IDgstate_unshare__tmp))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgstate_unshare_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgstate_unshare_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgstate_unshare_i)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-0.92 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDgstate_unshare__tmp))) (F_check_ge (0) (0))]
    | 25%positive => []
    | _ => []
  end.


Theorem gstate_unshare_ai_correct:
  forall s p' s', steps (g_start gstate_unshare) s (g_edges gstate_unshare) p' s' -> gstate_unshare_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gstate_unshare_pot_correct:
  forall s p' s',
    steps (g_start gstate_unshare) s (g_edges gstate_unshare) p' s' ->
    (gstate_unshare_pot (g_start gstate_unshare) s >= gstate_unshare_pot p' s')%Q.
Proof.
  check_lp gstate_unshare_ai_correct gstate_unshare_hints.
Qed.

