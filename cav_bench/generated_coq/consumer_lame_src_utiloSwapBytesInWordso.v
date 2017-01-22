Require Import pasta.Pasta.

Notation IDSwapBytesInWords_z := 1%positive.
Notation IDSwapBytesInWords__tmp := 2%positive.
Notation IDSwapBytesInWords_i := 3%positive.
Notation IDSwapBytesInWords_loc := 4%positive.
Notation IDSwapBytesInWords_words := 5%positive.
Definition SwapBytesInWords : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDSwapBytesInWords_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDSwapBytesInWords__tmp
             (Some (EVar IDSwapBytesInWords_words))),3%positive)::
             (3%positive,(AAssign IDSwapBytesInWords_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDSwapBytesInWords_i)
             s) < (eval (EVar IDSwapBytesInWords__tmp) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDSwapBytesInWords_i)
             s) >= (eval (EVar IDSwapBytesInWords__tmp) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDSwapBytesInWords_i
             (Some (EAdd (EVar IDSwapBytesInWords_i) (ENum (1))))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDSwapBytesInWords_z
             (Some (EAdd (ENum (1)) (EVar IDSwapBytesInWords_z)))),
             15%positive)::(15%positive,AWeaken,6%positive)::nil
|}.

Definition SwapBytesInWords_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0)%Z
    | 3%positive => (-1 * (s IDSwapBytesInWords_z) <= 0 /\ 1 * (s IDSwapBytesInWords_z) <= 0)%Z
    | 4%positive => (1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ 1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 5%positive => (-1 * (s IDSwapBytesInWords_i) <= 0 /\ 1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ 1 * (s IDSwapBytesInWords_z) <= 0)%Z
    | 6%positive => (-1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 7%positive => (-1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ 1 * (s IDSwapBytesInWords__tmp)+ -1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 8%positive => (1 * (s IDSwapBytesInWords__tmp)+ -1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 9%positive => (-1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) + 1 <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 11%positive => (-1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_i) + 1 <= 0 /\ -1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 13%positive => (-1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_i) + 1 <= 0 /\ -1 * (s IDSwapBytesInWords_z) <= 0)%Z
    | 14%positive => (-1 * (s IDSwapBytesInWords_z) <= 0 /\ -1 * (s IDSwapBytesInWords_i) + 1 <= 0 /\ -1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) <= 0)%Z
    | 15%positive => (-1 * (s IDSwapBytesInWords__tmp)+ 1 * (s IDSwapBytesInWords_i) <= 0 /\ -1 * (s IDSwapBytesInWords_i) + 1 <= 0 /\ -1 * (s IDSwapBytesInWords_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition SwapBytesInWords_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDSwapBytesInWords_words)))%Q
    | 2%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords_words)))%Q
    | 3%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)))%Q
    | 4%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)
                            - (s IDSwapBytesInWords_i)))%Q
    | 5%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)
                            - (s IDSwapBytesInWords_i)))%Q
    | 6%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)
                            - (s IDSwapBytesInWords_i)))%Q
    | 7%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)
                            - (s IDSwapBytesInWords_i)))%Q
    | 8%positive => ((s IDSwapBytesInWords_z))%Q
    | 9%positive => ((s IDSwapBytesInWords_z)
                     + max0((s IDSwapBytesInWords__tmp)
                            - (s IDSwapBytesInWords_i)))%Q
    | 10%positive => ((1 # 1) + (s IDSwapBytesInWords_z)
                      + max0(-1 + (s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | 11%positive => ((1 # 1) + (s IDSwapBytesInWords_z)
                      + max0(-1 + (s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | 12%positive => ((1 # 1) + (s IDSwapBytesInWords_z)
                      + max0((s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | 13%positive => ((1 # 1) + (s IDSwapBytesInWords_z)
                      + max0((s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | 14%positive => ((1 # 1) + (s IDSwapBytesInWords_z)
                      + max0((s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | 15%positive => ((s IDSwapBytesInWords_z)
                      + max0((s IDSwapBytesInWords__tmp)
                             - (s IDSwapBytesInWords_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition SwapBytesInWords_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDSwapBytesInWords__tmp)
                                                            - (s IDSwapBytesInWords_i)) (-1
                                                                    + (s IDSwapBytesInWords__tmp)
                                                                    - (s IDSwapBytesInWords_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDSwapBytesInWords__tmp)
                                                                - (s IDSwapBytesInWords_i))) (F_check_ge (0) (0))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDSwapBytesInWords__tmp)
                                                    - (s IDSwapBytesInWords_i)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem SwapBytesInWords_ai_correct:
  forall s p' s', steps (g_start SwapBytesInWords) s (g_edges SwapBytesInWords) p' s' -> SwapBytesInWords_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem SwapBytesInWords_pot_correct:
  forall s p' s',
    steps (g_start SwapBytesInWords) s (g_edges SwapBytesInWords) p' s' ->
    (SwapBytesInWords_pot (g_start SwapBytesInWords) s >= SwapBytesInWords_pot p' s')%Q.
Proof.
  check_lp SwapBytesInWords_ai_correct SwapBytesInWords_hints.
Qed.

