Require Import pasta.Pasta.

Notation IDBF_PartLength_z := 1%positive.
Notation IDBF_PartLength_bits := 2%positive.
Notation IDBF_PartLength_i := 3%positive.
Notation IDBF_PartLength_part_dref_off0 := 4%positive.
Notation IDBF_PartLength_part := 5%positive.
Definition BF_PartLength : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDBF_PartLength_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDBF_PartLength_part_dref_off0) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDBF_PartLength_i)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDBF_PartLength_bits (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDBF_PartLength_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDBF_PartLength_i)
             s) < (eval (EVar IDBF_PartLength_part_dref_off0) s))%Z)),
             12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDBF_PartLength_i)
             s) >= (eval (EVar IDBF_PartLength_part_dref_off0) s))%Z)),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDBF_PartLength_bits None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDBF_PartLength_i
             (Some (EAdd (EVar IDBF_PartLength_i) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDBF_PartLength_z (Some (EAdd (ENum (1))
             (EVar IDBF_PartLength_z)))),19%positive)::
             (19%positive,AWeaken,9%positive)::nil
|}.

Definition BF_PartLength_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_part_dref_off0) <= 0)%Z
    | 4%positive => (-1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0)%Z
    | 5%positive => (-1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_part_dref_off0) <= 0)%Z
    | 6%positive => (-1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0 /\ 1 * (s IDBF_PartLength_bits) <= 0 /\ -1 * (s IDBF_PartLength_bits) <= 0)%Z
    | 7%positive => (-1 * (s IDBF_PartLength_bits) <= 0 /\ 1 * (s IDBF_PartLength_bits) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ 1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0)%Z
    | 8%positive => (-1 * (s IDBF_PartLength_i) <= 0 /\ 1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ 1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_bits) <= 0 /\ -1 * (s IDBF_PartLength_bits) <= 0)%Z
    | 9%positive => (-1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0)%Z
    | 10%positive => (1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i)+ 1 * (s IDBF_PartLength_part_dref_off0) <= 0)%Z
    | 11%positive => (-1 * (s IDBF_PartLength_i)+ 1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0)%Z
    | 12%positive => (-1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) + 1 <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0)%Z
    | 14%positive => (-1 * (s IDBF_PartLength_i) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) + 1 <= 0)%Z
    | 15%positive => (1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) + 1 <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0 /\ -1 * (s IDBF_PartLength_i) <= 0)%Z
    | 16%positive => (-1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDBF_PartLength_i) + 1 <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_z) <= 0)%Z
    | 18%positive => (-1 * (s IDBF_PartLength_z) <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDBF_PartLength_i) + 1 <= 0 /\ 1 * (s IDBF_PartLength_i)+ -1 * (s IDBF_PartLength_part_dref_off0) <= 0 /\ -1 * (s IDBF_PartLength_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition BF_PartLength_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 2%positive => ((s IDBF_PartLength_z)
                     + max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 3%positive => ((s IDBF_PartLength_z)
                     + max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 4%positive => ((s IDBF_PartLength_z)
                     + max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 5%positive => ((s IDBF_PartLength_z)
                     + max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 6%positive => ((s IDBF_PartLength_z)
                     + max0((s IDBF_PartLength_part_dref_off0)))%Q
    | 7%positive => ((s IDBF_PartLength_z)
                     + max0(-(s IDBF_PartLength_i)
                            + (s IDBF_PartLength_part_dref_off0)))%Q
    | 8%positive => ((s IDBF_PartLength_z)
                     + max0(-(s IDBF_PartLength_i)
                            + (s IDBF_PartLength_part_dref_off0)))%Q
    | 9%positive => ((s IDBF_PartLength_z)
                     + max0(-(s IDBF_PartLength_i)
                            + (s IDBF_PartLength_part_dref_off0)))%Q
    | 10%positive => ((s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 11%positive => ((s IDBF_PartLength_z))%Q
    | 12%positive => ((s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 13%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-1 - (s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 14%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-1 - (s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 15%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-1 - (s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 16%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 17%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 18%positive => ((1 # 1) + (s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | 19%positive => ((s IDBF_PartLength_z)
                      + max0(-(s IDBF_PartLength_i)
                             + (s IDBF_PartLength_part_dref_off0)))%Q
    | _ => (0 # 1)%Q
  end.

Definition BF_PartLength_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDBF_PartLength_i)
                                                             + (s IDBF_PartLength_part_dref_off0)) (-1
                                                                    - (s IDBF_PartLength_i)
                                                                    + (s IDBF_PartLength_part_dref_off0)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDBF_PartLength_i)
                                            + (s IDBF_PartLength_part_dref_off0))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDBF_PartLength_i)
                                                     + (s IDBF_PartLength_part_dref_off0)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem BF_PartLength_ai_correct:
  forall s p' s', steps (g_start BF_PartLength) s (g_edges BF_PartLength) p' s' -> BF_PartLength_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BF_PartLength_pot_correct:
  forall s p' s',
    steps (g_start BF_PartLength) s (g_edges BF_PartLength) p' s' ->
    (BF_PartLength_pot (g_start BF_PartLength) s >= BF_PartLength_pot p' s')%Q.
Proof.
  check_lp BF_PartLength_ai_correct BF_PartLength_hints.
Qed.

