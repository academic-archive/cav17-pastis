Require Import pasta.Pasta.

Notation IDcrc32buf_z := 1%positive.
Notation IDcrc32buf__tmp := 2%positive.
Notation IDcrc32buf_oldcrc32 := 3%positive.
Notation IDcrc32buf_buf := 4%positive.
Notation IDcrc32buf_len := 5%positive.
Definition crc32buf : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDcrc32buf_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcrc32buf__tmp
             (Some (EVar IDcrc32buf_len))),3%positive)::
             (3%positive,(AAssign IDcrc32buf_oldcrc32 None),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcrc32buf__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcrc32buf__tmp) s) =
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDcrc32buf_oldcrc32 None),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDcrc32buf__tmp
             (Some (EAdd (EVar IDcrc32buf__tmp) (ENum (-1))))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDcrc32buf_z (Some (EAdd (ENum (1))
             (EVar IDcrc32buf_z)))),16%positive)::
             (16%positive,AWeaken,6%positive)::nil
|}.

Definition crc32buf_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcrc32buf_z) <= 0 /\ -1 * (s IDcrc32buf_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcrc32buf_z) <= 0 /\ 1 * (s IDcrc32buf_z) <= 0)%Z
    | 4%positive => (1 * (s IDcrc32buf_z) <= 0 /\ -1 * (s IDcrc32buf_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcrc32buf_z) <= 0 /\ 1 * (s IDcrc32buf_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcrc32buf_z) <= 0 /\ 1 * (s IDcrc32buf__tmp) <= 0 /\ -1 * (s IDcrc32buf__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDcrc32buf__tmp) <= 0 /\ 1 * (s IDcrc32buf__tmp) <= 0 /\ -1 * (s IDcrc32buf_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcrc32buf_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcrc32buf_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition crc32buf_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDcrc32buf_len))%Q
    | 2%positive => ((s IDcrc32buf_len) + (s IDcrc32buf_z))%Q
    | 3%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 4%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 5%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 6%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 7%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 8%positive => ((s IDcrc32buf_z))%Q
    | 9%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 10%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 11%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 12%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 13%positive => ((1 # 1) + (s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 14%positive => ((1 # 1) + (s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 15%positive => ((1 # 1) + (s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | 16%positive => ((s IDcrc32buf__tmp) + (s IDcrc32buf_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition crc32buf_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcrc32buf__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcrc32buf__tmp)) (0))) (F_max0_ge_0 ((s IDcrc32buf__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem crc32buf_ai_correct:
  forall s p' s', steps (g_start crc32buf) s (g_edges crc32buf) p' s' -> crc32buf_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem crc32buf_pot_correct:
  forall s p' s',
    steps (g_start crc32buf) s (g_edges crc32buf) p' s' ->
    (crc32buf_pot (g_start crc32buf) s >= crc32buf_pot p' s')%Q.
Proof.
  check_lp crc32buf_ai_correct crc32buf_hints.
Qed.

