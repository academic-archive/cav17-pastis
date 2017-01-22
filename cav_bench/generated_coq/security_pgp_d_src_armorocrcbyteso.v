Require Import pasta.Pasta.

Notation IDcrcbytes_z := 1%positive.
Notation IDcrcbytes__tmp := 2%positive.
Notation IDcrcbytes__tmp1 := 3%positive.
Notation IDcrcbytes_accum := 4%positive.
Notation IDcrcbytes_buf := 5%positive.
Notation IDcrcbytes_len := 6%positive.
Definition crcbytes : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDcrcbytes_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcrcbytes__tmp
             (Some (EVar IDcrcbytes_len))),3%positive)::
             (3%positive,(AAssign IDcrcbytes__tmp1
             (Some (EVar IDcrcbytes_accum))),4%positive)::
             (4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDcrcbytes__tmp1 None),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDcrcbytes__tmp
             (Some (EAdd (EVar IDcrcbytes__tmp) (ENum (-1))))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcrcbytes__tmp) (ENum (-1))) s) <>
             (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDcrcbytes__tmp) (ENum (-1))) s) =
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDcrcbytes_z (Some (EAdd (ENum (1))
             (EVar IDcrcbytes_z)))),5%positive)::nil
|}.

Definition crcbytes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcrcbytes_z) <= 0 /\ -1 * (s IDcrcbytes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcrcbytes_z) <= 0 /\ 1 * (s IDcrcbytes_z) <= 0)%Z
    | 4%positive => (1 * (s IDcrcbytes_z) <= 0 /\ -1 * (s IDcrcbytes_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 8%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcrcbytes_z) <= 0 /\ 1 * (s IDcrcbytes__tmp) + -1 <= 0 /\ -1 * (s IDcrcbytes__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDcrcbytes__tmp) + 1 <= 0 /\ 1 * (s IDcrcbytes__tmp) + -1 <= 0 /\ -1 * (s IDcrcbytes_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcrcbytes_z) <= 0)%Z
    | _ => False
  end.

Definition crcbytes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDcrcbytes_len))%Q
    | 2%positive => ((s IDcrcbytes_len) + (s IDcrcbytes_z))%Q
    | 3%positive => ((s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 4%positive => ((s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 5%positive => ((s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 6%positive => ((s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 7%positive => ((s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 8%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 9%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 10%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 11%positive => ((s IDcrcbytes_z))%Q
    | 12%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 13%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | 14%positive => ((1 # 1) + (s IDcrcbytes__tmp) + (s IDcrcbytes_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition crcbytes_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-2 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDcrcbytes__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcrcbytes__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcrcbytes__tmp)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem crcbytes_ai_correct:
  forall s p' s', steps (g_start crcbytes) s (g_edges crcbytes) p' s' -> crcbytes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem crcbytes_pot_correct:
  forall s p' s',
    steps (g_start crcbytes) s (g_edges crcbytes) p' s' ->
    (crcbytes_pot (g_start crcbytes) s >= crcbytes_pot p' s')%Q.
Proof.
  check_lp crcbytes_ai_correct crcbytes_hints.
Qed.

