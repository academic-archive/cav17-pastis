Require Import pasta.Pasta.

Notation IDWriteBytesSwapped_z := 1%positive.
Notation IDWriteBytesSwapped__tmp := 2%positive.
Notation IDWriteBytesSwapped_fp := 3%positive.
Notation IDWriteBytesSwapped_n := 4%positive.
Notation IDWriteBytesSwapped_p := 5%positive.
Definition WriteBytesSwapped : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDWriteBytesSwapped_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDWriteBytesSwapped__tmp
             (Some (EVar IDWriteBytesSwapped_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDWriteBytesSwapped__tmp
             (Some (EAdd (EVar IDWriteBytesSwapped__tmp) (ENum (-1))))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDWriteBytesSwapped__tmp) s) >
             (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDWriteBytesSwapped__tmp) s) <=
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDWriteBytesSwapped_z
             (Some (EAdd (ENum (1)) (EVar IDWriteBytesSwapped_z)))),
             4%positive)::nil
|}.

Definition WriteBytesSwapped_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDWriteBytesSwapped_z) <= 0 /\ -1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 3%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0 /\ 1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 4%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 5%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 6%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 7%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0 /\ 1 * (s IDWriteBytesSwapped__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDWriteBytesSwapped__tmp) <= 0 /\ -1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 9%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0 /\ -1 * (s IDWriteBytesSwapped__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDWriteBytesSwapped__tmp) + 1 <= 0 /\ -1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | 11%positive => (-1 * (s IDWriteBytesSwapped_z) <= 0 /\ -1 * (s IDWriteBytesSwapped__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDWriteBytesSwapped__tmp) + 1 <= 0 /\ -1 * (s IDWriteBytesSwapped_z) <= 0)%Z
    | _ => False
  end.

Definition WriteBytesSwapped_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDWriteBytesSwapped_n)))%Q
    | 2%positive => ((s IDWriteBytesSwapped_z)
                     + max0(-1 + (s IDWriteBytesSwapped_n)))%Q
    | 3%positive => ((s IDWriteBytesSwapped_z)
                     + max0(-1 + (s IDWriteBytesSwapped__tmp)))%Q
    | 4%positive => ((s IDWriteBytesSwapped_z)
                     + max0(-1 + (s IDWriteBytesSwapped__tmp)))%Q
    | 5%positive => ((s IDWriteBytesSwapped_z)
                     + max0((s IDWriteBytesSwapped__tmp)))%Q
    | 6%positive => ((s IDWriteBytesSwapped_z)
                     + max0((s IDWriteBytesSwapped__tmp)))%Q
    | 7%positive => ((s IDWriteBytesSwapped_z)
                     + max0((s IDWriteBytesSwapped__tmp)))%Q
    | 8%positive => ((s IDWriteBytesSwapped_z))%Q
    | 9%positive => ((s IDWriteBytesSwapped_z)
                     + max0((s IDWriteBytesSwapped__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDWriteBytesSwapped_z)
                      + max0(-1 + (s IDWriteBytesSwapped__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDWriteBytesSwapped_z)
                      + max0(-1 + (s IDWriteBytesSwapped__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDWriteBytesSwapped_z)
                      + max0(-1 + (s IDWriteBytesSwapped__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition WriteBytesSwapped_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDWriteBytesSwapped__tmp)) (-1
                                                                    + (s IDWriteBytesSwapped__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDWriteBytesSwapped__tmp))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDWriteBytesSwapped__tmp)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem WriteBytesSwapped_ai_correct:
  forall s p' s', steps (g_start WriteBytesSwapped) s (g_edges WriteBytesSwapped) p' s' -> WriteBytesSwapped_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem WriteBytesSwapped_pot_correct:
  forall s p' s',
    steps (g_start WriteBytesSwapped) s (g_edges WriteBytesSwapped) p' s' ->
    (WriteBytesSwapped_pot (g_start WriteBytesSwapped) s >= WriteBytesSwapped_pot p' s')%Q.
Proof.
  check_lp WriteBytesSwapped_ai_correct WriteBytesSwapped_hints.
Qed.

