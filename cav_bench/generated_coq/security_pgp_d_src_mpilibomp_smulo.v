Require Import pasta.Pasta.

Notation IDmp_smul_z := 1%positive.
Notation IDmp_smul__tmp := 2%positive.
Notation IDmp_smul_carry := 3%positive.
Notation IDmp_smul_i := 4%positive.
Notation IDmp_smul_munit_prec := 5%positive.
Notation IDmp_smul_p := 6%positive.
Notation IDmp_smul_multiplicand := 7%positive.
Notation IDmp_smul_multiplier := 8%positive.
Notation IDmp_smul_prod := 9%positive.
Definition mp_smul : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDmp_smul_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmp_smul__tmp
             (Some (EVar IDmp_smul_multiplier))),3%positive)::
             (3%positive,(AAssign IDmp_smul_carry (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDmp_smul_i (Some (ENum (0)))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDmp_smul_i) s) <
             (eval (EVar IDmp_smul_munit_prec) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDmp_smul_i) s) >=
             (eval (EVar IDmp_smul_munit_prec) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDmp_smul_p None),12%positive)::
             (12%positive,(AAssign IDmp_smul_p None),13%positive)::
             (13%positive,(AAssign IDmp_smul_carry None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDmp_smul_i (Some (EAdd (EVar IDmp_smul_i)
             (ENum (1))))),16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDmp_smul_z (Some (EAdd (ENum (1))
             (EVar IDmp_smul_z)))),19%positive)::
             (19%positive,AWeaken,7%positive)::nil
|}.

Definition mp_smul_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_z) <= 0)%Z
    | 4%positive => (1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_carry) <= 0 /\ -1 * (s IDmp_smul_carry) <= 0)%Z
    | 5%positive => (-1 * (s IDmp_smul_carry) <= 0 /\ 1 * (s IDmp_smul_carry) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 6%positive => (-1 * (s IDmp_smul_i) <= 0 /\ 1 * (s IDmp_smul_i) <= 0 /\ 1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_carry) <= 0 /\ -1 * (s IDmp_smul_carry) <= 0)%Z
    | 7%positive => (-1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 8%positive => (-1 * (s IDmp_smul_i) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i)+ 1 * (s IDmp_smul_munit_prec) <= 0)%Z
    | 9%positive => (-1 * (s IDmp_smul_i)+ 1 * (s IDmp_smul_munit_prec) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 10%positive => (-1 * (s IDmp_smul_i) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 12%positive => (-1 * (s IDmp_smul_i) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 14%positive => (-1 * (s IDmp_smul_i) <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0)%Z
    | 15%positive => (1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) + 1 <= 0 /\ -1 * (s IDmp_smul_z) <= 0 /\ -1 * (s IDmp_smul_i) <= 0)%Z
    | 16%positive => (-1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) <= 0 /\ -1 * (s IDmp_smul_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDmp_smul_i) + 1 <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) <= 0 /\ -1 * (s IDmp_smul_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmp_smul_z) <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) <= 0 /\ -1 * (s IDmp_smul_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDmp_smul_i) + 1 <= 0 /\ 1 * (s IDmp_smul_i)+ -1 * (s IDmp_smul_munit_prec) <= 0 /\ -1 * (s IDmp_smul_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mp_smul_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDmp_smul_munit_prec)))%Q
    | 2%positive => ((s IDmp_smul_z) + max0((s IDmp_smul_munit_prec)))%Q
    | 3%positive => ((s IDmp_smul_z) + max0((s IDmp_smul_munit_prec)))%Q
    | 4%positive => ((s IDmp_smul_z) + max0((s IDmp_smul_munit_prec)))%Q
    | 5%positive => ((s IDmp_smul_z)
                     + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 6%positive => ((s IDmp_smul_z)
                     + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 7%positive => ((s IDmp_smul_z)
                     + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 8%positive => ((s IDmp_smul_z)
                     + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 9%positive => ((s IDmp_smul_z))%Q
    | 10%positive => ((s IDmp_smul_z)
                      + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 11%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-1 - (s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 12%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-1 - (s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 13%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-1 - (s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 14%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-1 - (s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 15%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-1 - (s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 16%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 17%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 18%positive => ((1 # 1) + (s IDmp_smul_z)
                      + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | 19%positive => ((s IDmp_smul_z)
                      + max0(-(s IDmp_smul_i) + (s IDmp_smul_munit_prec)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mp_smul_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDmp_smul_i)
                                                            + (s IDmp_smul_munit_prec)) (-1
                                                                    - (s IDmp_smul_i)
                                                                    + (s IDmp_smul_munit_prec)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDmp_smul_i)
                                                                + (s IDmp_smul_munit_prec))) (F_check_ge (0) (0))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDmp_smul_i)
                                                     + (s IDmp_smul_munit_prec)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem mp_smul_ai_correct:
  forall s p' s', steps (g_start mp_smul) s (g_edges mp_smul) p' s' -> mp_smul_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mp_smul_pot_correct:
  forall s p' s',
    steps (g_start mp_smul) s (g_edges mp_smul) p' s' ->
    (mp_smul_pot (g_start mp_smul) s >= mp_smul_pot p' s')%Q.
Proof.
  check_lp mp_smul_ai_correct mp_smul_hints.
Qed.

