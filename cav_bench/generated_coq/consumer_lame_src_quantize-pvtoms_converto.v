Require Import pasta.Pasta.

Notation IDms_convert_z := 1%positive.
Notation IDms_convert_i := 2%positive.
Notation IDms_convert_xr := 3%positive.
Notation IDms_convert_xr_org := 4%positive.
Definition ms_convert : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDms_convert_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDms_convert_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDms_convert_i) s) <
             (eval (ENum (576)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDms_convert_i) s) >=
             (eval (ENum (576)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDms_convert_i
             (Some (EAdd (EVar IDms_convert_i) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDms_convert_z (Some (EAdd (ENum (1))
             (EVar IDms_convert_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition ms_convert_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_z) <= 0)%Z
    | 3%positive => (-1 * (s IDms_convert_z) <= 0 /\ 1 * (s IDms_convert_z) <= 0 /\ 1 * (s IDms_convert_i) <= 0 /\ -1 * (s IDms_convert_i) <= 0)%Z
    | 4%positive => (-1 * (s IDms_convert_i) <= 0 /\ 1 * (s IDms_convert_i) <= 0 /\ 1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_z) <= 0)%Z
    | 5%positive => (-1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_i) <= 0 /\ 1 * (s IDms_convert_i) + -576 <= 0)%Z
    | 6%positive => (1 * (s IDms_convert_i) + -576 <= 0 /\ -1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_i) + 576 <= 0)%Z
    | 7%positive => (-1 * (s IDms_convert_i) + 576 <= 0 /\ -1 * (s IDms_convert_z) <= 0 /\ 1 * (s IDms_convert_i) + -576 <= 0)%Z
    | 8%positive => (-1 * (s IDms_convert_i) <= 0 /\ -1 * (s IDms_convert_z) <= 0 /\ 1 * (s IDms_convert_i) + -575 <= 0)%Z
    | 9%positive => (1 * (s IDms_convert_i) + -575 <= 0 /\ -1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_i) <= 0)%Z
    | 10%positive => (-1 * (s IDms_convert_i) <= 0 /\ -1 * (s IDms_convert_z) <= 0 /\ 1 * (s IDms_convert_i) + -575 <= 0)%Z
    | 11%positive => (-1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_i) + 1 <= 0 /\ 1 * (s IDms_convert_i) + -576 <= 0)%Z
    | 12%positive => (1 * (s IDms_convert_i) + -576 <= 0 /\ -1 * (s IDms_convert_i) + 1 <= 0 /\ -1 * (s IDms_convert_z) <= 0)%Z
    | 13%positive => (-1 * (s IDms_convert_z) <= 0 /\ -1 * (s IDms_convert_i) + 1 <= 0 /\ 1 * (s IDms_convert_i) + -576 <= 0)%Z
    | 14%positive => (1 * (s IDms_convert_i) + -576 <= 0 /\ -1 * (s IDms_convert_i) + 1 <= 0 /\ -1 * (s IDms_convert_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ms_convert_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((576 # 1))%Q
    | 2%positive => ((576 # 1) + (s IDms_convert_z))%Q
    | 3%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | 4%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | 5%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | 6%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | 7%positive => ((s IDms_convert_z))%Q
    | 8%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | 9%positive => ((1 # 1) + (s IDms_convert_z)
                     + max0(575 - (s IDms_convert_i)))%Q
    | 10%positive => ((1 # 1) + (s IDms_convert_z)
                      + max0(575 - (s IDms_convert_i)))%Q
    | 11%positive => ((1 # 1) + (s IDms_convert_z)
                      + max0(576 - (s IDms_convert_i)))%Q
    | 12%positive => ((1 # 1) + (s IDms_convert_z)
                      + max0(576 - (s IDms_convert_i)))%Q
    | 13%positive => ((1 # 1) + (s IDms_convert_z)
                      + max0(576 - (s IDms_convert_i)))%Q
    | 14%positive => ((s IDms_convert_z) + max0(576 - (s IDms_convert_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ms_convert_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (576
                                                            - (s IDms_convert_i)) (575
                                                                    - (s IDms_convert_i)));
                     (*-1 0*) F_max0_ge_0 (575 - (s IDms_convert_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (576 - (s IDms_convert_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem ms_convert_ai_correct:
  forall s p' s', steps (g_start ms_convert) s (g_edges ms_convert) p' s' -> ms_convert_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ms_convert_pot_correct:
  forall s p' s',
    steps (g_start ms_convert) s (g_edges ms_convert) p' s' ->
    (ms_convert_pot (g_start ms_convert) s >= ms_convert_pot p' s')%Q.
Proof.
  check_lp ms_convert_ai_correct ms_convert_hints.
Qed.

