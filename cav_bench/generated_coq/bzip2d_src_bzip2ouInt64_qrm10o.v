Require Import pasta.Pasta.

Notation IDuInt64_qrm10_z := 1%positive.
Notation IDuInt64_qrm10_i := 2%positive.
Notation IDuInt64_qrm10_rem := 3%positive.
Notation IDuInt64_qrm10_tmp := 4%positive.
Notation IDuInt64_qrm10_n := 5%positive.
Definition uInt64_qrm10 : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDuInt64_qrm10_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDuInt64_qrm10_rem (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDuInt64_qrm10_i (Some (ENum (7)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDuInt64_qrm10_i)
             s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDuInt64_qrm10_i)
             s) < (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDuInt64_qrm10_tmp None),11%positive)::
             (11%positive,(AAssign IDuInt64_qrm10_rem None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDuInt64_qrm10_i
             (Some (EAdd (EVar IDuInt64_qrm10_i) (ENum (-1))))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDuInt64_qrm10_z (Some (EAdd (ENum (1))
             (EVar IDuInt64_qrm10_z)))),17%positive)::
             (17%positive,AWeaken,6%positive)::nil
|}.

Definition uInt64_qrm10_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0)%Z
    | 3%positive => (-1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_rem) <= 0 /\ -1 * (s IDuInt64_qrm10_rem) <= 0)%Z
    | 4%positive => (-1 * (s IDuInt64_qrm10_rem) <= 0 /\ 1 * (s IDuInt64_qrm10_rem) <= 0 /\ 1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_i) + 7 <= 0)%Z
    | 5%positive => (-1 * (s IDuInt64_qrm10_i) + 7 <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_rem) <= 0 /\ -1 * (s IDuInt64_qrm10_rem) <= 0)%Z
    | 6%positive => (-1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_i) + -1 <= 0)%Z
    | 7%positive => (-1 * (s IDuInt64_qrm10_i) + -1 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + 1 <= 0)%Z
    | 8%positive => (1 * (s IDuInt64_qrm10_i) + 1 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_i) + -1 <= 0)%Z
    | 9%positive => (1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_i) <= 0)%Z
    | 10%positive => (-1 * (s IDuInt64_qrm10_i) <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -7 <= 0)%Z
    | 11%positive => (1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_i) <= 0)%Z
    | 12%positive => (-1 * (s IDuInt64_qrm10_i) <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -7 <= 0)%Z
    | 13%positive => (1 * (s IDuInt64_qrm10_i) + -7 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0 /\ -1 * (s IDuInt64_qrm10_i) <= 0)%Z
    | 14%positive => (-1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -6 <= 0 /\ -1 * (s IDuInt64_qrm10_i) + -1 <= 0)%Z
    | 15%positive => (-1 * (s IDuInt64_qrm10_i) + -1 <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -6 <= 0 /\ -1 * (s IDuInt64_qrm10_z) <= 0)%Z
    | 16%positive => (-1 * (s IDuInt64_qrm10_z) <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -6 <= 0 /\ -1 * (s IDuInt64_qrm10_i) + -1 <= 0)%Z
    | 17%positive => (-1 * (s IDuInt64_qrm10_i) + -1 <= 0 /\ 1 * (s IDuInt64_qrm10_i) + -6 <= 0 /\ -1 * (s IDuInt64_qrm10_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition uInt64_qrm10_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDuInt64_qrm10_z))%Q
    | 3%positive => ((8 # 1) + (s IDuInt64_qrm10_z))%Q
    | 4%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 5%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 6%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 7%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 8%positive => ((s IDuInt64_qrm10_z))%Q
    | 9%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 10%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0((s IDuInt64_qrm10_i)))%Q
    | 11%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0((s IDuInt64_qrm10_i)))%Q
    | 12%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0((s IDuInt64_qrm10_i)))%Q
    | 13%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0((s IDuInt64_qrm10_i)))%Q
    | 14%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 15%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 16%positive => ((1 # 1) + (s IDuInt64_qrm10_z)
                      + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | 17%positive => ((s IDuInt64_qrm10_z) + max0(1 + (s IDuInt64_qrm10_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition uInt64_qrm10_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDuInt64_qrm10_i)) ((s IDuInt64_qrm10_i)));
                     (*-1 0*) F_max0_ge_0 ((s IDuInt64_qrm10_i))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement (1 + (s IDuInt64_qrm10_i)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem uInt64_qrm10_ai_correct:
  forall s p' s', steps (g_start uInt64_qrm10) s (g_edges uInt64_qrm10) p' s' -> uInt64_qrm10_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem uInt64_qrm10_pot_correct:
  forall s p' s',
    steps (g_start uInt64_qrm10) s (g_edges uInt64_qrm10) p' s' ->
    (uInt64_qrm10_pot (g_start uInt64_qrm10) s >= uInt64_qrm10_pot p' s')%Q.
Proof.
  check_lp uInt64_qrm10_ai_correct uInt64_qrm10_hints.
Qed.

