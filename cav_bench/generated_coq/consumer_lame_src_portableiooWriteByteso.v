Require Import pasta.Pasta.

Notation IDWriteBytes_z := 1%positive.
Notation IDWriteBytes__tmp := 2%positive.
Notation IDWriteBytes_fp := 3%positive.
Notation IDWriteBytes_n := 4%positive.
Notation IDWriteBytes_p := 5%positive.
Definition WriteBytes : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDWriteBytes_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDWriteBytes__tmp
             (Some (EVar IDWriteBytes_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDWriteBytes__tmp
             (Some (EAdd (EVar IDWriteBytes__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDWriteBytes__tmp)
             s) > (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDWriteBytes__tmp)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDWriteBytes_z (Some (EAdd (ENum (1))
             (EVar IDWriteBytes_z)))),4%positive)::nil
|}.

Definition WriteBytes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDWriteBytes_z) <= 0 /\ -1 * (s IDWriteBytes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDWriteBytes_z) <= 0 /\ 1 * (s IDWriteBytes_z) <= 0)%Z
    | 4%positive => (-1 * (s IDWriteBytes_z) <= 0)%Z
    | 5%positive => (-1 * (s IDWriteBytes_z) <= 0)%Z
    | 6%positive => (-1 * (s IDWriteBytes_z) <= 0)%Z
    | 7%positive => (-1 * (s IDWriteBytes_z) <= 0 /\ 1 * (s IDWriteBytes__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDWriteBytes__tmp) <= 0 /\ -1 * (s IDWriteBytes_z) <= 0)%Z
    | 9%positive => (-1 * (s IDWriteBytes_z) <= 0 /\ -1 * (s IDWriteBytes__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDWriteBytes__tmp) + 1 <= 0 /\ -1 * (s IDWriteBytes_z) <= 0)%Z
    | 11%positive => (-1 * (s IDWriteBytes_z) <= 0 /\ -1 * (s IDWriteBytes__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDWriteBytes__tmp) + 1 <= 0 /\ -1 * (s IDWriteBytes_z) <= 0)%Z
    | _ => False
  end.

Definition WriteBytes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDWriteBytes_n)))%Q
    | 2%positive => ((s IDWriteBytes_z) + max0(-1 + (s IDWriteBytes_n)))%Q
    | 3%positive => ((s IDWriteBytes_z) + max0(-1 + (s IDWriteBytes__tmp)))%Q
    | 4%positive => ((s IDWriteBytes_z) + max0(-1 + (s IDWriteBytes__tmp)))%Q
    | 5%positive => ((s IDWriteBytes_z) + max0((s IDWriteBytes__tmp)))%Q
    | 6%positive => ((s IDWriteBytes_z) + max0((s IDWriteBytes__tmp)))%Q
    | 7%positive => ((s IDWriteBytes_z) + max0((s IDWriteBytes__tmp)))%Q
    | 8%positive => ((s IDWriteBytes_z))%Q
    | 9%positive => ((s IDWriteBytes_z) + max0((s IDWriteBytes__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDWriteBytes_z)
                      + max0(-1 + (s IDWriteBytes__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDWriteBytes_z)
                      + max0(-1 + (s IDWriteBytes__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDWriteBytes_z)
                      + max0(-1 + (s IDWriteBytes__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition WriteBytes_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDWriteBytes__tmp)) (-1
                                                                    + (s IDWriteBytes__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDWriteBytes__tmp))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDWriteBytes__tmp)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem WriteBytes_ai_correct:
  forall s p' s', steps (g_start WriteBytes) s (g_edges WriteBytes) p' s' -> WriteBytes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem WriteBytes_pot_correct:
  forall s p' s',
    steps (g_start WriteBytes) s (g_edges WriteBytes) p' s' ->
    (WriteBytes_pot (g_start WriteBytes) s >= WriteBytes_pot p' s')%Q.
Proof.
  check_lp WriteBytes_ai_correct WriteBytes_hints.
Qed.

