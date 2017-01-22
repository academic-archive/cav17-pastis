Require Import pasta.Pasta.

Notation IDmk_crctbl_z := 1%positive.
Notation IDmk_crctbl__tmp := 2%positive.
Notation IDmk_crctbl_i := 3%positive.
Notation IDmk_crctbl_t := 4%positive.
Notation IDmk_crctbl_poly := 5%positive.
Definition mk_crctbl : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDmk_crctbl_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmk_crctbl__tmp
             (Some (EVar IDmk_crctbl_poly))),3%positive)::
             (3%positive,(AAssign IDmk_crctbl_i (Some (ENum (1)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmk_crctbl_i) s) <
             (eval (ENum (128)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmk_crctbl_i) s) >=
             (eval (ENum (128)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDmk_crctbl_t None),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,15%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDmk_crctbl_t None),14%positive)::
             (14%positive,ANone,17%positive)::
             (15%positive,(AAssign IDmk_crctbl_t None),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDmk_crctbl_i
             (Some (EAdd (EVar IDmk_crctbl_i) (ENum (1))))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDmk_crctbl_z (Some (EAdd (ENum (1))
             (EVar IDmk_crctbl_z)))),22%positive)::
             (22%positive,AWeaken,6%positive)::nil
|}.

Definition mk_crctbl_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_z) <= 0)%Z
    | 4%positive => (1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -1 <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 5%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ 1 * (s IDmk_crctbl_i) + -1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0)%Z
    | 7%positive => (1 * (s IDmk_crctbl_i) + -128 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 128 <= 0)%Z
    | 8%positive => (-1 * (s IDmk_crctbl_i) + 128 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0)%Z
    | 9%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -127 <= 0)%Z
    | 10%positive => (1 * (s IDmk_crctbl_i) + -127 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -127 <= 0)%Z
    | 12%positive => (1 * (s IDmk_crctbl_i) + -127 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -127 <= 0)%Z
    | 14%positive => (1 * (s IDmk_crctbl_i) + -127 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -127 <= 0)%Z
    | 16%positive => (1 * (s IDmk_crctbl_i) + -127 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDmk_crctbl_i) + 1 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -127 <= 0)%Z
    | 18%positive => (1 * (s IDmk_crctbl_i) + -127 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0 /\ -1 * (s IDmk_crctbl_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0 /\ -1 * (s IDmk_crctbl_i) + 2 <= 0)%Z
    | 20%positive => (-1 * (s IDmk_crctbl_i) + 2 <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0 /\ -1 * (s IDmk_crctbl_z) <= 0)%Z
    | 21%positive => (-1 * (s IDmk_crctbl_z) <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0 /\ -1 * (s IDmk_crctbl_i) + 2 <= 0)%Z
    | 22%positive => (-1 * (s IDmk_crctbl_i) + 2 <= 0 /\ 1 * (s IDmk_crctbl_i) + -128 <= 0 /\ -1 * (s IDmk_crctbl_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mk_crctbl_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((127 # 1))%Q
    | 2%positive => ((127 # 1) + (s IDmk_crctbl_z))%Q
    | 3%positive => ((127 # 1) + (s IDmk_crctbl_z))%Q
    | 4%positive => ((128 # 1) - (s IDmk_crctbl_i) + (s IDmk_crctbl_z))%Q
    | 5%positive => ((128 # 1) - (s IDmk_crctbl_i) + (s IDmk_crctbl_z))%Q
    | 6%positive => ((s IDmk_crctbl_z) + max0(128 - (s IDmk_crctbl_i)))%Q
    | 7%positive => ((s IDmk_crctbl_z) + max0(128 - (s IDmk_crctbl_i)))%Q
    | 8%positive => ((s IDmk_crctbl_z))%Q
    | 9%positive => ((s IDmk_crctbl_z) + max0(128 - (s IDmk_crctbl_i)))%Q
    | 10%positive => (-(127 # 1) + (s IDmk_crctbl_i) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i))
                      + max0(128 - (s IDmk_crctbl_i)))%Q
    | 11%positive => (-(127 # 1) + (s IDmk_crctbl_i) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i))
                      + max0(128 - (s IDmk_crctbl_i)))%Q
    | 12%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 13%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 14%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 15%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 16%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 17%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 18%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(127 - (s IDmk_crctbl_i)))%Q
    | 19%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(128 - (s IDmk_crctbl_i)))%Q
    | 20%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(128 - (s IDmk_crctbl_i)))%Q
    | 21%positive => ((1 # 1) + (s IDmk_crctbl_z)
                      + max0(128 - (s IDmk_crctbl_i)))%Q
    | 22%positive => ((s IDmk_crctbl_z) + max0(128 - (s IDmk_crctbl_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mk_crctbl_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (128
                                                                    - (s IDmk_crctbl_i)) (0))) (F_max0_ge_0 (128
                                                                    - (s IDmk_crctbl_i)))]
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (128
                                                            - (s IDmk_crctbl_i)) (127
                                                                    - (s IDmk_crctbl_i)));
                     (*-1 0*) F_max0_ge_0 (127 - (s IDmk_crctbl_i))]
    | 8%positive => []
    | 9%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (127
                                                                    - (s IDmk_crctbl_i)) (0))) (F_max0_ge_0 (127
                                                                    - (s IDmk_crctbl_i)))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                                   - 
                                                                   (s IDmk_crctbl_i))) (F_check_ge (128
                                                                    - (s IDmk_crctbl_i)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | _ => []
  end.


Theorem mk_crctbl_ai_correct:
  forall s p' s', steps (g_start mk_crctbl) s (g_edges mk_crctbl) p' s' -> mk_crctbl_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mk_crctbl_pot_correct:
  forall s p' s',
    steps (g_start mk_crctbl) s (g_edges mk_crctbl) p' s' ->
    (mk_crctbl_pot (g_start mk_crctbl) s >= mk_crctbl_pot p' s')%Q.
Proof.
  check_lp mk_crctbl_ai_correct mk_crctbl_hints.
Qed.

