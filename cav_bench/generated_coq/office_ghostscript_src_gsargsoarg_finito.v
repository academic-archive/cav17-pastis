Require Import pasta.Pasta.

Notation IDarg_finit_z := 1%positive.
Notation IDarg_finit_pal_dref_off36 := 2%positive.
Notation IDarg_finit_pal := 3%positive.
Definition arg_finit : graph := {|
  g_start := 1%positive;
  g_end := 6%positive;
  g_edges := (1%positive,(AAssign IDarg_finit_z (Some (ENum (0)))),
             2%positive)::(2%positive,ANone,3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDarg_finit_pal_dref_off36) s) <>
             (eval (ENum (0)) s))%Z)),7%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDarg_finit_pal_dref_off36) s) =
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDarg_finit_pal_dref_off36
             (Some (EAdd (EVar IDarg_finit_pal_dref_off36) (ENum (-1))))),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDarg_finit_z (Some (EAdd (ENum (1))
             (EVar IDarg_finit_z)))),15%positive)::
             (15%positive,AWeaken,4%positive)::nil
|}.

Definition arg_finit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDarg_finit_z) <= 0 /\ -1 * (s IDarg_finit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDarg_finit_z) <= 0 /\ 1 * (s IDarg_finit_z) <= 0)%Z
    | 4%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 5%positive => (-1 * (s IDarg_finit_z) <= 0 /\ 1 * (s IDarg_finit_pal_dref_off36) <= 0 /\ -1 * (s IDarg_finit_pal_dref_off36) <= 0)%Z
    | 6%positive => (-1 * (s IDarg_finit_pal_dref_off36) <= 0 /\ 1 * (s IDarg_finit_pal_dref_off36) <= 0 /\ -1 * (s IDarg_finit_z) <= 0)%Z
    | 7%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 8%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 9%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 10%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 11%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 12%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 13%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 14%positive => (-1 * (s IDarg_finit_z) <= 0)%Z
    | 15%positive => (-1 * (s IDarg_finit_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition arg_finit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDarg_finit_pal_dref_off36))%Q
    | 2%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 3%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 4%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 5%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 6%positive => ((s IDarg_finit_z))%Q
    | 7%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 8%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | 9%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                     + (s IDarg_finit_z))%Q
    | 10%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                      + (s IDarg_finit_z))%Q
    | 11%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                      + (s IDarg_finit_z))%Q
    | 12%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                      + (s IDarg_finit_z))%Q
    | 13%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                      + (s IDarg_finit_z))%Q
    | 14%positive => ((1 # 1) + (s IDarg_finit_pal_dref_off36)
                      + (s IDarg_finit_z))%Q
    | 15%positive => ((s IDarg_finit_pal_dref_off36) + (s IDarg_finit_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition arg_finit_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarg_finit_pal_dref_off36))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDarg_finit_pal_dref_off36)) (0))) (F_max0_ge_0 ((s IDarg_finit_pal_dref_off36)))]
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem arg_finit_ai_correct:
  forall s p' s', steps (g_start arg_finit) s (g_edges arg_finit) p' s' -> arg_finit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem arg_finit_pot_correct:
  forall s p' s',
    steps (g_start arg_finit) s (g_edges arg_finit) p' s' ->
    (arg_finit_pot (g_start arg_finit) s >= arg_finit_pot p' s')%Q.
Proof.
  check_lp arg_finit_ai_correct arg_finit_hints.
Qed.

