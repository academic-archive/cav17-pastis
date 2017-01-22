Require Import pasta.Pasta.

Notation IDbit_shifter_z := 1%positive.
Notation IDbit_shifter__tmp := 2%positive.
Notation IDbit_shifter_i := 3%positive.
Notation IDbit_shifter_n := 4%positive.
Notation IDbit_shifter_x := 5%positive.
Definition bit_shifter : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDbit_shifter_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbit_shifter__tmp
             (Some (EVar IDbit_shifter_x))),3%positive)::
             (3%positive,(AAssign IDbit_shifter_n (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDbit_shifter_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDbit_shifter__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDbit_shifter__tmp)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,14%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDbit_shifter_i)
             s) < (eval (ENum (64)) s))%Z)),15%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDbit_shifter_i)
             s) >= (eval (ENum (64)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDbit_shifter_n None),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDbit_shifter_i
             (Some (EAdd (EVar IDbit_shifter_i) (ENum (1))))),19%positive)::
             (19%positive,(AAssign IDbit_shifter__tmp None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDbit_shifter_z (Some (EAdd (ENum (1))
             (EVar IDbit_shifter_z)))),23%positive)::
             (23%positive,AWeaken,7%positive)::nil
|}.

Definition bit_shifter_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_z) <= 0)%Z
    | 4%positive => (1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_n) <= 0 /\ -1 * (s IDbit_shifter_n) <= 0)%Z
    | 5%positive => (-1 * (s IDbit_shifter_n) <= 0 /\ 1 * (s IDbit_shifter_n) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0)%Z
    | 6%positive => (-1 * (s IDbit_shifter_i) <= 0 /\ 1 * (s IDbit_shifter_i) <= 0 /\ 1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_n) <= 0 /\ -1 * (s IDbit_shifter_n) <= 0)%Z
    | 7%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0)%Z
    | 8%positive => (1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter__tmp) <= 0 /\ -1 * (s IDbit_shifter__tmp) <= 0)%Z
    | 9%positive => (1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0)%Z
    | 10%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0)%Z
    | 11%positive => (1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0)%Z
    | 12%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0)%Z
    | 13%positive => (1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) + 64 <= 0)%Z
    | 14%positive => (-1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0)%Z
    | 15%positive => (-1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) + -63 <= 0)%Z
    | 16%positive => (1 * (s IDbit_shifter_i) + -63 <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0)%Z
    | 17%positive => (-1 * (s IDbit_shifter_i) <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) + -63 <= 0)%Z
    | 18%positive => (1 * (s IDbit_shifter_i) + -63 <= 0 /\ -1 * (s IDbit_shifter_z) <= 0 /\ -1 * (s IDbit_shifter_i) <= 0)%Z
    | 19%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDbit_shifter_i) + 1 <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_z) <= 0)%Z
    | 21%positive => (-1 * (s IDbit_shifter_z) <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDbit_shifter_i) + 1 <= 0 /\ 1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_z) <= 0)%Z
    | 23%positive => (1 * (s IDbit_shifter_i) + -64 <= 0 /\ -1 * (s IDbit_shifter_i) + 1 <= 0 /\ -1 * (s IDbit_shifter_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition bit_shifter_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((64 # 1))%Q
    | 2%positive => ((64 # 1) + (s IDbit_shifter_z))%Q
    | 3%positive => ((64 # 1) + (s IDbit_shifter_z))%Q
    | 4%positive => ((64 # 1) + (s IDbit_shifter_z))%Q
    | 5%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 6%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 7%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 8%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 9%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 10%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 11%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 12%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 13%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 14%positive => ((s IDbit_shifter_z))%Q
    | 15%positive => ((s IDbit_shifter_z) + max0(64 - (s IDbit_shifter_i)))%Q
    | 16%positive => ((64 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 17%positive => ((64 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 18%positive => ((64 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 19%positive => ((65 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 20%positive => ((65 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 21%positive => ((65 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 22%positive => ((65 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | 23%positive => ((64 # 1) - (s IDbit_shifter_i) + (s IDbit_shifter_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition bit_shifter_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                            - (s IDbit_shifter_i)) (63
                                                                    - (s IDbit_shifter_i)));
                     (*-1 0*) F_max0_ge_0 (63 - (s IDbit_shifter_i))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDbit_shifter_i)) (63
                                                                    - (s IDbit_shifter_i)));
                      (*-1 0*) F_max0_ge_0 (63 - (s IDbit_shifter_i))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (64
                                                                   - 
                                                                   (s IDbit_shifter_i))) (F_check_ge (64
                                                                    - (s IDbit_shifter_i)) (0))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDbit_shifter_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDbit_shifter_i)))]
    | _ => []
  end.


Theorem bit_shifter_ai_correct:
  forall s p' s', steps (g_start bit_shifter) s (g_edges bit_shifter) p' s' -> bit_shifter_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bit_shifter_pot_correct:
  forall s p' s',
    steps (g_start bit_shifter) s (g_edges bit_shifter) p' s' ->
    (bit_shifter_pot (g_start bit_shifter) s >= bit_shifter_pot p' s')%Q.
Proof.
  check_lp bit_shifter_ai_correct bit_shifter_hints.
Qed.

