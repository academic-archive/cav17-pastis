Require Import pasta.Pasta.

Notation IDforcelc_z := 1%positive.
Notation IDforcelc__tmp := 2%positive.
Notation IDforcelc_dst := 3%positive.
Notation IDforcelc_len := 4%positive.
Definition forcelc : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDforcelc_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDforcelc__tmp
             (Some (EVar IDforcelc_len))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDforcelc__tmp
             (Some (EAdd (EVar IDforcelc__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDforcelc__tmp)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDforcelc__tmp)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDforcelc_z (Some (EAdd (ENum (1))
             (EVar IDforcelc_z)))),4%positive)::nil
|}.

Definition forcelc_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDforcelc_z) <= 0 /\ -1 * (s IDforcelc_z) <= 0)%Z
    | 3%positive => (-1 * (s IDforcelc_z) <= 0 /\ 1 * (s IDforcelc_z) <= 0)%Z
    | 4%positive => (-1 * (s IDforcelc_z) <= 0)%Z
    | 5%positive => (-1 * (s IDforcelc_z) <= 0)%Z
    | 6%positive => (-1 * (s IDforcelc_z) <= 0)%Z
    | 7%positive => (-1 * (s IDforcelc_z) <= 0 /\ 1 * (s IDforcelc__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDforcelc__tmp) <= 0 /\ -1 * (s IDforcelc_z) <= 0)%Z
    | 9%positive => (-1 * (s IDforcelc_z) <= 0 /\ -1 * (s IDforcelc__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDforcelc__tmp) + 1 <= 0 /\ -1 * (s IDforcelc_z) <= 0)%Z
    | 11%positive => (-1 * (s IDforcelc_z) <= 0 /\ -1 * (s IDforcelc__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDforcelc__tmp) + 1 <= 0 /\ -1 * (s IDforcelc_z) <= 0)%Z
    | 13%positive => (-1 * (s IDforcelc_z) <= 0 /\ -1 * (s IDforcelc__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition forcelc_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDforcelc_len)))%Q
    | 2%positive => ((s IDforcelc_z) + max0(-1 + (s IDforcelc_len)))%Q
    | 3%positive => ((s IDforcelc_z) + max0(-1 + (s IDforcelc__tmp)))%Q
    | 4%positive => ((s IDforcelc_z) + max0(-1 + (s IDforcelc__tmp)))%Q
    | 5%positive => ((s IDforcelc_z) + max0((s IDforcelc__tmp)))%Q
    | 6%positive => ((s IDforcelc_z) + max0((s IDforcelc__tmp)))%Q
    | 7%positive => ((s IDforcelc_z) + max0((s IDforcelc__tmp)))%Q
    | 8%positive => ((s IDforcelc_z))%Q
    | 9%positive => ((s IDforcelc_z) + max0((s IDforcelc__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDforcelc_z)
                      + max0(-1 + (s IDforcelc__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDforcelc_z)
                      + max0(-1 + (s IDforcelc__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDforcelc_z)
                      + max0(-1 + (s IDforcelc__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDforcelc_z)
                      + max0(-1 + (s IDforcelc__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition forcelc_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDforcelc__tmp)) (-1
                                                                    + (s IDforcelc__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDforcelc__tmp))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDforcelc__tmp)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | _ => []
  end.


Theorem forcelc_ai_correct:
  forall s p' s', steps (g_start forcelc) s (g_edges forcelc) p' s' -> forcelc_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem forcelc_pot_correct:
  forall s p' s',
    steps (g_start forcelc) s (g_edges forcelc) p' s' ->
    (forcelc_pot (g_start forcelc) s >= forcelc_pot p' s')%Q.
Proof.
  check_lp forcelc_ai_correct forcelc_hints.
Qed.

