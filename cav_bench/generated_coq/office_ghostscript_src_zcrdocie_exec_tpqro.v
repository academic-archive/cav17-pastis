Require Import pasta.Pasta.

Notation IDcie_exec_tpqr_z := 1%positive.
Notation IDcie_exec_tpqr__tmp := 2%positive.
Notation IDcie_exec_tpqr_i := 3%positive.
Notation IDcie_exec_tpqr_space := 4%positive.
Notation IDcie_exec_tpqr_op := 5%positive.
Definition cie_exec_tpqr : graph := {|
  g_start := 1%positive;
  g_end := 31%positive;
  g_edges := (1%positive,(AAssign IDcie_exec_tpqr_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcie_exec_tpqr_space None),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,ANone,28%positive)::(4%positive,ANone,5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,ANone,25%positive)::(7%positive,ANone,8%positive)::
             (8%positive,ANone,9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDcie_exec_tpqr_i (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDcie_exec_tpqr_i)
             s) < (eval (ENum (4)) s))%Z)),18%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDcie_exec_tpqr_i)
             s) >= (eval (ENum (4)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDcie_exec_tpqr__tmp None),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,31%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDcie_exec_tpqr_i
             (Some (EAdd (EVar IDcie_exec_tpqr_i) (ENum (1))))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDcie_exec_tpqr_z (Some (EAdd (ENum (1))
             (EVar IDcie_exec_tpqr_z)))),24%positive)::
             (24%positive,AWeaken,13%positive)::
             (25%positive,(AAssign IDcie_exec_tpqr__tmp (Some (ENum (-16)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,31%positive)::
             (28%positive,(AAssign IDcie_exec_tpqr__tmp (Some (ENum (-17)))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::nil
|}.

Definition cie_exec_tpqr_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 4%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 6%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 8%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 10%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_exec_tpqr_i) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -4 <= 0)%Z
    | 14%positive => (1 * (s IDcie_exec_tpqr_i) + -4 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 4 <= 0)%Z
    | 15%positive => (-1 * (s IDcie_exec_tpqr_i) + 4 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDcie_exec_tpqr_i) + -4 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 4 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_exec_tpqr_i) + 4 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -4 <= 0)%Z
    | 18%positive => (-1 * (s IDcie_exec_tpqr_i) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDcie_exec_tpqr_i) + -3 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) <= 0)%Z
    | 20%positive => (-1 * (s IDcie_exec_tpqr_i) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -3 <= 0)%Z
    | 21%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 1 <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -4 <= 0)%Z
    | 22%positive => (1 * (s IDcie_exec_tpqr_i) + -4 <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 1 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 1 <= 0 /\ 1 * (s IDcie_exec_tpqr_i) + -4 <= 0)%Z
    | 24%positive => (1 * (s IDcie_exec_tpqr_i) + -4 <= 0 /\ -1 * (s IDcie_exec_tpqr_i) + 1 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 26%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr__tmp) + 16 <= 0 /\ -1 * (s IDcie_exec_tpqr__tmp) + -16 <= 0)%Z
    | 27%positive => (-1 * (s IDcie_exec_tpqr__tmp) + -16 <= 0 /\ 1 * (s IDcie_exec_tpqr__tmp) + 16 <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 28%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 29%positive => (1 * (s IDcie_exec_tpqr_z) <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr__tmp) + 17 <= 0 /\ -1 * (s IDcie_exec_tpqr__tmp) + -17 <= 0)%Z
    | 30%positive => (-1 * (s IDcie_exec_tpqr__tmp) + -17 <= 0 /\ 1 * (s IDcie_exec_tpqr__tmp) + 17 <= 0 /\ -1 * (s IDcie_exec_tpqr_z) <= 0 /\ 1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | 31%positive => (-1 * (s IDcie_exec_tpqr_z) <= 0)%Z
    | _ => False
  end.

Definition cie_exec_tpqr_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 3%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 4%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 5%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 6%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 7%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 8%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 9%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 10%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 11%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 12%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 13%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 14%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 15%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 16%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 17%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 18%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 19%positive => ((1 # 1) + (s IDcie_exec_tpqr_z)
                      + max0(3 - (s IDcie_exec_tpqr_i)))%Q
    | 20%positive => ((1 # 1) + (s IDcie_exec_tpqr_z)
                      + max0(3 - (s IDcie_exec_tpqr_i)))%Q
    | 21%positive => ((1 # 1) + (s IDcie_exec_tpqr_z)
                      + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 22%positive => ((1 # 1) + (s IDcie_exec_tpqr_z)
                      + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 23%positive => ((1 # 1) + (s IDcie_exec_tpqr_z)
                      + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 24%positive => ((s IDcie_exec_tpqr_z) + max0(4 - (s IDcie_exec_tpqr_i)))%Q
    | 25%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 26%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 27%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 28%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 29%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 30%positive => ((4 # 1) + (s IDcie_exec_tpqr_z))%Q
    | 31%positive => ((s IDcie_exec_tpqr_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_exec_tpqr_hints (p : node) (s : state) := 
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
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDcie_exec_tpqr_i)) (3
                                                                    - (s IDcie_exec_tpqr_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDcie_exec_tpqr_i))]
    | 18%positive => [(*0 1*) F_max0_pre_decrement (4 - (s IDcie_exec_tpqr_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-4 0*) F_one]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-4 0*) F_one]
    | 31%positive => []
    | _ => []
  end.


Theorem cie_exec_tpqr_ai_correct:
  forall s p' s', steps (g_start cie_exec_tpqr) s (g_edges cie_exec_tpqr) p' s' -> cie_exec_tpqr_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_exec_tpqr_pot_correct:
  forall s p' s',
    steps (g_start cie_exec_tpqr) s (g_edges cie_exec_tpqr) p' s' ->
    (cie_exec_tpqr_pot (g_start cie_exec_tpqr) s >= cie_exec_tpqr_pot p' s')%Q.
Proof.
  check_lp cie_exec_tpqr_ai_correct cie_exec_tpqr_hints.
Qed.

