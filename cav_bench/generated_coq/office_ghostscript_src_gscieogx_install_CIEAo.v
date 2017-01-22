Require Import pasta.Pasta.

Notation IDgx_install_CIEA_z := 1%positive.
Notation IDgx_install_CIEA_i := 2%positive.
Notation IDgx_install_CIEA_pcs := 3%positive.
Notation IDgx_install_CIEA_pgs := 4%positive.
Definition gx_install_CIEA : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDgx_install_CIEA_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgx_install_CIEA_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDgx_install_CIEA_i)
             s) < (eval (ENum (512)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDgx_install_CIEA_i)
             s) >= (eval (ENum (512)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDgx_install_CIEA_i
             (Some (EAdd (EVar IDgx_install_CIEA_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDgx_install_CIEA_z (Some (EAdd (ENum (1))
             (EVar IDgx_install_CIEA_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition gx_install_CIEA_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_install_CIEA_z) <= 0 /\ 1 * (s IDgx_install_CIEA_z) <= 0 /\ 1 * (s IDgx_install_CIEA_i) <= 0 /\ -1 * (s IDgx_install_CIEA_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgx_install_CIEA_i) <= 0 /\ 1 * (s IDgx_install_CIEA_i) <= 0 /\ 1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_i) <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -512 <= 0)%Z
    | 6%positive => (1 * (s IDgx_install_CIEA_i) + -512 <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_i) + 512 <= 0)%Z
    | 7%positive => (-1 * (s IDgx_install_CIEA_i) + 512 <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -512 <= 0)%Z
    | 8%positive => (-1 * (s IDgx_install_CIEA_i) <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -511 <= 0)%Z
    | 9%positive => (1 * (s IDgx_install_CIEA_i) + -511 <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_install_CIEA_i) <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -511 <= 0)%Z
    | 11%positive => (-1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_i) + 1 <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -512 <= 0)%Z
    | 12%positive => (1 * (s IDgx_install_CIEA_i) + -512 <= 0 /\ -1 * (s IDgx_install_CIEA_i) + 1 <= 0 /\ -1 * (s IDgx_install_CIEA_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgx_install_CIEA_z) <= 0 /\ -1 * (s IDgx_install_CIEA_i) + 1 <= 0 /\ 1 * (s IDgx_install_CIEA_i) + -512 <= 0)%Z
    | 14%positive => (1 * (s IDgx_install_CIEA_i) + -512 <= 0 /\ -1 * (s IDgx_install_CIEA_i) + 1 <= 0 /\ -1 * (s IDgx_install_CIEA_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gx_install_CIEA_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1))%Q
    | 2%positive => ((512 # 1) + (s IDgx_install_CIEA_z))%Q
    | 3%positive => ((s IDgx_install_CIEA_z)
                     + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 4%positive => ((s IDgx_install_CIEA_z)
                     + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 5%positive => ((s IDgx_install_CIEA_z)
                     + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 6%positive => ((s IDgx_install_CIEA_z)
                     + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 7%positive => ((s IDgx_install_CIEA_z))%Q
    | 8%positive => ((s IDgx_install_CIEA_z)
                     + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 9%positive => ((1 # 1) + (s IDgx_install_CIEA_z)
                     + max0(511 - (s IDgx_install_CIEA_i)))%Q
    | 10%positive => ((1 # 1) + (s IDgx_install_CIEA_z)
                      + max0(511 - (s IDgx_install_CIEA_i)))%Q
    | 11%positive => ((1 # 1) + (s IDgx_install_CIEA_z)
                      + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 12%positive => ((1 # 1) + (s IDgx_install_CIEA_z)
                      + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 13%positive => ((1 # 1) + (s IDgx_install_CIEA_z)
                      + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | 14%positive => ((s IDgx_install_CIEA_z)
                      + max0(512 - (s IDgx_install_CIEA_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_install_CIEA_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                            - (s IDgx_install_CIEA_i)) (511
                                                                    - (s IDgx_install_CIEA_i)));
                     (*-1 0*) F_max0_ge_0 (511 - (s IDgx_install_CIEA_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (512
                                                    - (s IDgx_install_CIEA_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem gx_install_CIEA_ai_correct:
  forall s p' s', steps (g_start gx_install_CIEA) s (g_edges gx_install_CIEA) p' s' -> gx_install_CIEA_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_install_CIEA_pot_correct:
  forall s p' s',
    steps (g_start gx_install_CIEA) s (g_edges gx_install_CIEA) p' s' ->
    (gx_install_CIEA_pot (g_start gx_install_CIEA) s >= gx_install_CIEA_pot p' s')%Q.
Proof.
  check_lp gx_install_CIEA_ai_correct gx_install_CIEA_hints.
Qed.

