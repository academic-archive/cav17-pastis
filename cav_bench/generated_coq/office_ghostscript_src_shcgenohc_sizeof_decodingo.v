Require Import pasta.Pasta.

Notation IDhc_sizeof_decoding_z := 1%positive.
Notation IDhc_sizeof_decoding__tmp := 2%positive.
Notation IDhc_sizeof_decoding_carry := 3%positive.
Notation IDhc_sizeof_decoding_def_dref_off8 := 4%positive.
Notation IDhc_sizeof_decoding_i := 5%positive.
Notation IDhc_sizeof_decoding_mask := 6%positive.
Notation IDhc_sizeof_decoding_size := 7%positive.
Notation IDhc_sizeof_decoding_def := 8%positive.
Notation IDhc_sizeof_decoding_initial_bits := 9%positive.
Definition hc_sizeof_decoding : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDhc_sizeof_decoding_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizeof_decoding_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizeof_decoding_def_dref_off8) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDhc_sizeof_decoding__tmp
             (Some (EVar IDhc_sizeof_decoding_initial_bits))),6%positive)::
             (6%positive,(AAssign IDhc_sizeof_decoding_size None),7%positive)::
             (7%positive,(AAssign IDhc_sizeof_decoding_carry
             (Some (ENum (0)))),8%positive)::
             (8%positive,(AAssign IDhc_sizeof_decoding_mask
             (Some (ENum (-2)))),9%positive)::
             (9%positive,(AAssign IDhc_sizeof_decoding_i
             (Some (EAdd (EVar IDhc_sizeof_decoding__tmp) (ENum (1))))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizeof_decoding_i) s) <=
             (eval (EVar IDhc_sizeof_decoding_def_dref_off8) s))%Z)),
             15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDhc_sizeof_decoding_i) s) >
             (eval (EVar IDhc_sizeof_decoding_def_dref_off8) s))%Z)),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDhc_sizeof_decoding_carry None),
             17%positive)::
             (17%positive,(AAssign IDhc_sizeof_decoding_size None),
             18%positive)::
             (18%positive,(AAssign IDhc_sizeof_decoding_carry None),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDhc_sizeof_decoding_i
             (Some (EAdd (EVar IDhc_sizeof_decoding_i) (ENum (1))))),
             21%positive)::
             (21%positive,(AAssign IDhc_sizeof_decoding_carry None),
             22%positive)::
             (22%positive,(AAssign IDhc_sizeof_decoding_mask None),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDhc_sizeof_decoding_z
             (Some (EAdd (ENum (1)) (EVar IDhc_sizeof_decoding_z)))),
             26%positive)::(26%positive,AWeaken,12%positive)::nil
|}.

Definition hc_sizeof_decoding_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0)%Z
    | 3%positive => (-1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 4%positive => (-1 * (s IDhc_sizeof_decoding_i) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 5%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 6%positive => (-1 * (s IDhc_sizeof_decoding_i) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 7%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 8%positive => (-1 * (s IDhc_sizeof_decoding_i) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ 1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ -1 * (s IDhc_sizeof_decoding_carry) <= 0)%Z
    | 9%positive => (-1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ 1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_i) <= 0 /\ 1 * (s IDhc_sizeof_decoding_mask) + 2 <= 0 /\ -1 * (s IDhc_sizeof_decoding_mask) + -2 <= 0)%Z
    | 10%positive => (-1 * (s IDhc_sizeof_decoding_mask) + -2 <= 0 /\ 1 * (s IDhc_sizeof_decoding_mask) + 2 <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ 1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ -1 * (s IDhc_sizeof_decoding_carry) <= 0)%Z
    | 11%positive => (-1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ 1 * (s IDhc_sizeof_decoding_carry) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_mask) + 2 <= 0 /\ -1 * (s IDhc_sizeof_decoding_mask) + -2 <= 0)%Z
    | 12%positive => (-1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 13%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ 1 * (s IDhc_sizeof_decoding_def_dref_off8)+ -1 * (s IDhc_sizeof_decoding_i) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDhc_sizeof_decoding_def_dref_off8)+ -1 * (s IDhc_sizeof_decoding_i) + 1 <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 15%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 16%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 17%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 18%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 19%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0)%Z
    | 20%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 21%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 23%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0)%Z
    | 25%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0)%Z
    | 26%positive => (-1 * (s IDhc_sizeof_decoding_def_dref_off8)+ 1 * (s IDhc_sizeof_decoding_i) + -1 <= 0 /\ -1 * (s IDhc_sizeof_decoding_def_dref_off8) <= 0 /\ -1 * (s IDhc_sizeof_decoding_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition hc_sizeof_decoding_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDhc_sizeof_decoding_def_dref_off8)
                          - (s IDhc_sizeof_decoding_initial_bits)))%Q
    | 2%positive => ((s IDhc_sizeof_decoding_z)
                     + max0((s IDhc_sizeof_decoding_def_dref_off8)
                            - (s IDhc_sizeof_decoding_initial_bits)))%Q
    | 3%positive => ((s IDhc_sizeof_decoding_z)
                     + max0((s IDhc_sizeof_decoding_def_dref_off8)
                            - (s IDhc_sizeof_decoding_initial_bits)))%Q
    | 4%positive => ((s IDhc_sizeof_decoding_z)
                     + max0((s IDhc_sizeof_decoding_def_dref_off8)
                            - (s IDhc_sizeof_decoding_initial_bits)))%Q
    | 5%positive => ((s IDhc_sizeof_decoding_z)
                     + max0((s IDhc_sizeof_decoding_def_dref_off8)
                            - (s IDhc_sizeof_decoding_initial_bits)))%Q
    | 6%positive => ((s IDhc_sizeof_decoding_z)
                     + max0(-(s IDhc_sizeof_decoding__tmp)
                            + (s IDhc_sizeof_decoding_def_dref_off8)))%Q
    | 7%positive => ((s IDhc_sizeof_decoding_z)
                     + max0(-(s IDhc_sizeof_decoding__tmp)
                            + (s IDhc_sizeof_decoding_def_dref_off8)))%Q
    | 8%positive => ((s IDhc_sizeof_decoding_z)
                     + max0(-(s IDhc_sizeof_decoding__tmp)
                            + (s IDhc_sizeof_decoding_def_dref_off8)))%Q
    | 9%positive => ((s IDhc_sizeof_decoding_z)
                     + max0(-(s IDhc_sizeof_decoding__tmp)
                            + (s IDhc_sizeof_decoding_def_dref_off8)))%Q
    | 10%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 11%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 12%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 13%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 14%positive => ((s IDhc_sizeof_decoding_z))%Q
    | 15%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 16%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0((s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 17%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0((s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 18%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0((s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 19%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0((s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 20%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0((s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 21%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 22%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 23%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 24%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 25%positive => ((1 # 1) + (s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | 26%positive => ((s IDhc_sizeof_decoding_z)
                      + max0(1 + (s IDhc_sizeof_decoding_def_dref_off8)
                             - (s IDhc_sizeof_decoding_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition hc_sizeof_decoding_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDhc_sizeof_decoding_def_dref_off8)
                                                             - (s IDhc_sizeof_decoding_i)) ((s IDhc_sizeof_decoding_def_dref_off8)
                                                                    - (s IDhc_sizeof_decoding_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDhc_sizeof_decoding_def_dref_off8)
                                                                 - (s IDhc_sizeof_decoding_i))) (F_check_ge (0) (0))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDhc_sizeof_decoding_def_dref_off8)
                                                     - (s IDhc_sizeof_decoding_i)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem hc_sizeof_decoding_ai_correct:
  forall s p' s', steps (g_start hc_sizeof_decoding) s (g_edges hc_sizeof_decoding) p' s' -> hc_sizeof_decoding_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem hc_sizeof_decoding_pot_correct:
  forall s p' s',
    steps (g_start hc_sizeof_decoding) s (g_edges hc_sizeof_decoding) p' s' ->
    (hc_sizeof_decoding_pot (g_start hc_sizeof_decoding) s >= hc_sizeof_decoding_pot p' s')%Q.
Proof.
  check_lp hc_sizeof_decoding_ai_correct hc_sizeof_decoding_hints.
Qed.

