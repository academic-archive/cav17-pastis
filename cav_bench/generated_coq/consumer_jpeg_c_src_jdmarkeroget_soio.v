Require Import pasta.Pasta.

Notation IDget_soi_z := 1%positive.
Notation IDget_soi_i := 2%positive.
Notation IDget_soi_cinfo := 3%positive.
Definition get_soi : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDget_soi_z (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::(3%positive,ANone,4%positive)::
             (3%positive,ANone,5%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDget_soi_i (Some (ENum (0)))),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDget_soi_i) s) <
             (eval (ENum (16)) s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDget_soi_i) s) >=
             (eval (ENum (16)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDget_soi_i (Some (EAdd (EVar IDget_soi_i)
             (ENum (1))))),14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDget_soi_z (Some (EAdd (ENum (1))
             (EVar IDget_soi_z)))),17%positive)::
             (17%positive,AWeaken,8%positive)::nil
|}.

Definition get_soi_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_z) <= 0)%Z
    | 3%positive => (-1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_z) <= 0)%Z
    | 4%positive => (1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_z) <= 0)%Z
    | 5%positive => (-1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_z) <= 0)%Z
    | 6%positive => (1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_i) <= 0 /\ -1 * (s IDget_soi_i) <= 0)%Z
    | 7%positive => (-1 * (s IDget_soi_i) <= 0 /\ 1 * (s IDget_soi_i) <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_z) <= 0)%Z
    | 8%positive => (-1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_i) <= 0 /\ 1 * (s IDget_soi_i) + -16 <= 0)%Z
    | 9%positive => (1 * (s IDget_soi_i) + -16 <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_i) + 16 <= 0)%Z
    | 10%positive => (-1 * (s IDget_soi_i) + 16 <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_i) + -16 <= 0)%Z
    | 11%positive => (-1 * (s IDget_soi_i) <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_i) + -15 <= 0)%Z
    | 12%positive => (1 * (s IDget_soi_i) + -15 <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_i) <= 0)%Z
    | 13%positive => (-1 * (s IDget_soi_i) <= 0 /\ -1 * (s IDget_soi_z) <= 0 /\ 1 * (s IDget_soi_i) + -15 <= 0)%Z
    | 14%positive => (-1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_i) + 1 <= 0 /\ 1 * (s IDget_soi_i) + -16 <= 0)%Z
    | 15%positive => (1 * (s IDget_soi_i) + -16 <= 0 /\ -1 * (s IDget_soi_i) + 1 <= 0 /\ -1 * (s IDget_soi_z) <= 0)%Z
    | 16%positive => (-1 * (s IDget_soi_z) <= 0 /\ -1 * (s IDget_soi_i) + 1 <= 0 /\ 1 * (s IDget_soi_i) + -16 <= 0)%Z
    | 17%positive => (1 * (s IDget_soi_i) + -16 <= 0 /\ -1 * (s IDget_soi_i) + 1 <= 0 /\ -1 * (s IDget_soi_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition get_soi_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDget_soi_z))%Q
    | 3%positive => ((16 # 1) + (s IDget_soi_z))%Q
    | 4%positive => ((16 # 1) + (s IDget_soi_z))%Q
    | 5%positive => ((16 # 1) + (s IDget_soi_z))%Q
    | 6%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 7%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 8%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 9%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 10%positive => ((s IDget_soi_z))%Q
    | 11%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 12%positive => ((1 # 1) + (s IDget_soi_z) + max0(15 - (s IDget_soi_i)))%Q
    | 13%positive => ((1 # 1) + (s IDget_soi_z) + max0(15 - (s IDget_soi_i)))%Q
    | 14%positive => ((1 # 1) + (s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 15%positive => ((1 # 1) + (s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 16%positive => ((1 # 1) + (s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | 17%positive => ((s IDget_soi_z) + max0(16 - (s IDget_soi_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition get_soi_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                            - (s IDget_soi_i)) (15
                                                                    - (s IDget_soi_i)));
                     (*-1 0*) F_max0_ge_0 (15 - (s IDget_soi_i))]
    | 10%positive => []
    | 11%positive => [(*0 1*) F_max0_pre_decrement (16 - (s IDget_soi_i)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem get_soi_ai_correct:
  forall s p' s', steps (g_start get_soi) s (g_edges get_soi) p' s' -> get_soi_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem get_soi_pot_correct:
  forall s p' s',
    steps (g_start get_soi) s (g_edges get_soi) p' s' ->
    (get_soi_pot (g_start get_soi) s >= get_soi_pot p' s')%Q.
Proof.
  check_lp get_soi_ai_correct get_soi_hints.
Qed.

