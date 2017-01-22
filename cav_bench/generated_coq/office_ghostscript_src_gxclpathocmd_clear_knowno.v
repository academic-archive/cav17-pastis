Require Import pasta.Pasta.

Notation IDcmd_clear_known_z := 1%positive.
Notation IDcmd_clear_known__tmp := 2%positive.
Notation IDcmd_clear_known_cldev_dref_off784 := 3%positive.
Notation IDcmd_clear_known_i := 4%positive.
Notation IDcmd_clear_known_unknown := 5%positive.
Notation IDcmd_clear_known_cldev := 6%positive.
Notation IDcmd_clear_known_known := 7%positive.
Definition cmd_clear_known : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDcmd_clear_known_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcmd_clear_known__tmp
             (Some (EVar IDcmd_clear_known_known))),3%positive)::
             (3%positive,(AAssign IDcmd_clear_known_unknown None),4%positive)::
             (4%positive,(AAssign IDcmd_clear_known_i
             (Some (EVar IDcmd_clear_known_cldev_dref_off784))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDcmd_clear_known_i
             (Some (EAdd (EVar IDcmd_clear_known_i) (ENum (-1))))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcmd_clear_known_i) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcmd_clear_known_i) (ENum (-1)))
             s) < (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDcmd_clear_known_z (Some (EAdd (ENum (1))
             (EVar IDcmd_clear_known_z)))),6%positive)::nil
|}.

Definition cmd_clear_known_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_clear_known_z) <= 0 /\ -1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ 1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 4%positive => (1 * (s IDcmd_clear_known_z) <= 0 /\ -1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ 1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ 1 * (s IDcmd_clear_known_i) <= 0)%Z
    | 10%positive => (1 * (s IDcmd_clear_known_i) <= 0 /\ -1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ -1 * (s IDcmd_clear_known_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_clear_known_i) + 1 <= 0 /\ -1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ -1 * (s IDcmd_clear_known_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDcmd_clear_known_i) + 1 <= 0 /\ -1 * (s IDcmd_clear_known_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_clear_known_z) <= 0 /\ -1 * (s IDcmd_clear_known_i) + 1 <= 0)%Z
    | _ => False
  end.

Definition cmd_clear_known_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcmd_clear_known_cldev_dref_off784)))%Q
    | 2%positive => ((s IDcmd_clear_known_z)
                     + max0(-1 + (s IDcmd_clear_known_cldev_dref_off784)))%Q
    | 3%positive => ((s IDcmd_clear_known_z)
                     + max0(-1 + (s IDcmd_clear_known_cldev_dref_off784)))%Q
    | 4%positive => ((s IDcmd_clear_known_z)
                     + max0(-1 + (s IDcmd_clear_known_cldev_dref_off784)))%Q
    | 5%positive => ((s IDcmd_clear_known_z)
                     + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | 6%positive => ((s IDcmd_clear_known_z)
                     + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | 7%positive => ((s IDcmd_clear_known_z) + max0((s IDcmd_clear_known_i)))%Q
    | 8%positive => ((s IDcmd_clear_known_z) + max0((s IDcmd_clear_known_i)))%Q
    | 9%positive => ((s IDcmd_clear_known_z) + max0((s IDcmd_clear_known_i)))%Q
    | 10%positive => ((s IDcmd_clear_known_z))%Q
    | 11%positive => ((s IDcmd_clear_known_z) + max0((s IDcmd_clear_known_i)))%Q
    | 12%positive => ((1 # 1) + (s IDcmd_clear_known_z)
                      + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | 13%positive => ((1 # 1) + (s IDcmd_clear_known_z)
                      + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | 14%positive => ((1 # 1) + (s IDcmd_clear_known_z)
                      + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | 15%positive => ((1 # 1) + (s IDcmd_clear_known_z)
                      + max0(-1 + (s IDcmd_clear_known_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_clear_known_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcmd_clear_known_i)) (-1
                                                                    + (s IDcmd_clear_known_i)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDcmd_clear_known_i))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcmd_clear_known_i)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem cmd_clear_known_ai_correct:
  forall s p' s', steps (g_start cmd_clear_known) s (g_edges cmd_clear_known) p' s' -> cmd_clear_known_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_clear_known_pot_correct:
  forall s p' s',
    steps (g_start cmd_clear_known) s (g_edges cmd_clear_known) p' s' ->
    (cmd_clear_known_pot (g_start cmd_clear_known) s >= cmd_clear_known_pot p' s')%Q.
Proof.
  check_lp cmd_clear_known_ai_correct cmd_clear_known_hints.
Qed.

