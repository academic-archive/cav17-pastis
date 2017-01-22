Require Import pasta.Pasta.

Notation IDBF_LoadHolderFromBitstreamPart_z := 1%positive.
Notation IDBF_LoadHolderFromBitstreamPart_i := 2%positive.
Notation IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0 := 3%positive.
Notation IDBF_LoadHolderFromBitstreamPart_theHolder := 4%positive.
Notation IDBF_LoadHolderFromBitstreamPart_thePart := 5%positive.
Definition BF_LoadHolderFromBitstreamPart : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDBF_LoadHolderFromBitstreamPart_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDBF_LoadHolderFromBitstreamPart_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDBF_LoadHolderFromBitstreamPart_i
             (Some (ENum (0)))),6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDBF_LoadHolderFromBitstreamPart_i) s) <
             (eval (EVar IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)
             s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDBF_LoadHolderFromBitstreamPart_i) s) >=
             (eval (EVar IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)
             s))%Z)),9%positive)::(9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDBF_LoadHolderFromBitstreamPart_i
             (Some (EAdd (EVar IDBF_LoadHolderFromBitstreamPart_i)
             (ENum (1))))),14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDBF_LoadHolderFromBitstreamPart_z
             (Some (EAdd (ENum (1))
             (EVar IDBF_LoadHolderFromBitstreamPart_z)))),17%positive)::
             (17%positive,AWeaken,8%positive)::nil
|}.

Definition BF_LoadHolderFromBitstreamPart_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 4%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0)%Z
    | 5%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 6%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0)%Z
    | 7%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 8%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 9%positive => (1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ 1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 10%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ 1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 11%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) + 1 <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0)%Z
    | 13%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_i) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) + 1 <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 15%positive => (1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) + 1 <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0)%Z
    | 16%positive => (-1 * (s IDBF_LoadHolderFromBitstreamPart_z) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) + 1 <= 0 /\ 1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0)%Z
    | 17%positive => (1 * (s IDBF_LoadHolderFromBitstreamPart_i)+ -1 * (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_i) + 1 <= 0 /\ -1 * (s IDBF_LoadHolderFromBitstreamPart_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition BF_LoadHolderFromBitstreamPart_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 2%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0((s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 3%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0((s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 4%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0((s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 5%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0((s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 6%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                            + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 7%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                            + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 8%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                            + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 9%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                     + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                            + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 10%positive => ((s IDBF_LoadHolderFromBitstreamPart_z))%Q
    | 11%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 12%positive => ((1 # 1) + (s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-1 - (s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 13%positive => ((1 # 1) + (s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-1 - (s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 14%positive => ((1 # 1) + (s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 15%positive => ((1 # 1) + (s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 16%positive => ((1 # 1) + (s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | 17%positive => ((s IDBF_LoadHolderFromBitstreamPart_z)
                      + max0(-(s IDBF_LoadHolderFromBitstreamPart_i)
                             + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)))%Q
    | _ => (0 # 1)%Q
  end.

Definition BF_LoadHolderFromBitstreamPart_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDBF_LoadHolderFromBitstreamPart_i)
                                                            + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)) (-1
                                                                    - (s IDBF_LoadHolderFromBitstreamPart_i)
                                                                    + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           - (s IDBF_LoadHolderFromBitstreamPart_i)
                                           + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDBF_LoadHolderFromBitstreamPart_i)
                                                     + (s IDBF_LoadHolderFromBitstreamPart_thePart_dref_off0)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem BF_LoadHolderFromBitstreamPart_ai_correct:
  forall s p' s', steps (g_start BF_LoadHolderFromBitstreamPart) s (g_edges BF_LoadHolderFromBitstreamPart) p' s' -> BF_LoadHolderFromBitstreamPart_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BF_LoadHolderFromBitstreamPart_pot_correct:
  forall s p' s',
    steps (g_start BF_LoadHolderFromBitstreamPart) s (g_edges BF_LoadHolderFromBitstreamPart) p' s' ->
    (BF_LoadHolderFromBitstreamPart_pot (g_start BF_LoadHolderFromBitstreamPart) s >= BF_LoadHolderFromBitstreamPart_pot p' s')%Q.
Proof.
  check_lp BF_LoadHolderFromBitstreamPart_ai_correct BF_LoadHolderFromBitstreamPart_hints.
Qed.

