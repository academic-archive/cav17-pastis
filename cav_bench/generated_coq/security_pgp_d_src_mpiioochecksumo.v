Require Import pasta.Pasta.

Notation IDchecksum_z := 1%positive.
Notation IDchecksum__tmp := 2%positive.
Notation IDchecksum_cs := 3%positive.
Notation IDchecksum_buf := 4%positive.
Notation IDchecksum_count := 5%positive.
Definition checksum : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDchecksum_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDchecksum__tmp
             (Some (EVar IDchecksum_count))),3%positive)::
             (3%positive,(AAssign IDchecksum_cs (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDchecksum__tmp
             (Some (EAdd (EVar IDchecksum__tmp) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDchecksum__tmp)
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDchecksum__tmp) s) =
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDchecksum_cs None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDchecksum_z (Some (EAdd (ENum (1))
             (EVar IDchecksum_z)))),5%positive)::nil
|}.

Definition checksum_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDchecksum_z) <= 0 /\ -1 * (s IDchecksum_z) <= 0)%Z
    | 3%positive => (-1 * (s IDchecksum_z) <= 0 /\ 1 * (s IDchecksum_z) <= 0)%Z
    | 4%positive => (1 * (s IDchecksum_z) <= 0 /\ -1 * (s IDchecksum_z) <= 0 /\ 1 * (s IDchecksum_cs) <= 0 /\ -1 * (s IDchecksum_cs) <= 0)%Z
    | 5%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 6%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 7%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 8%positive => (-1 * (s IDchecksum_z) <= 0 /\ 1 * (s IDchecksum__tmp) <= 0 /\ -1 * (s IDchecksum__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDchecksum__tmp) <= 0 /\ 1 * (s IDchecksum__tmp) <= 0 /\ -1 * (s IDchecksum_z) <= 0)%Z
    | 10%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 11%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 12%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 13%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | 14%positive => (-1 * (s IDchecksum_z) <= 0)%Z
    | _ => False
  end.

Definition checksum_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDchecksum_count))%Q
    | 2%positive => ((s IDchecksum_count) + (s IDchecksum_z))%Q
    | 3%positive => ((s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 4%positive => ((s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 5%positive => ((s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 6%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 7%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 8%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 9%positive => ((s IDchecksum_z))%Q
    | 10%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 11%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 12%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 13%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | 14%positive => ((1 # 1) + (s IDchecksum__tmp) + (s IDchecksum_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition checksum_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDchecksum__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDchecksum__tmp)) (0))) (F_max0_ge_0 ((s IDchecksum__tmp)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem checksum_ai_correct:
  forall s p' s', steps (g_start checksum) s (g_edges checksum) p' s' -> checksum_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem checksum_pot_correct:
  forall s p' s',
    steps (g_start checksum) s (g_edges checksum) p' s' ->
    (checksum_pot (g_start checksum) s >= checksum_pot p' s')%Q.
Proof.
  check_lp checksum_ai_correct checksum_hints.
Qed.

