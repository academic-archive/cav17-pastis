Require Import pasta.Pasta.

Notation IDIII_overlap_z_z := 1%positive.
Notation IDIII_overlap_z__tmp := 2%positive.
Notation IDIII_overlap_z_i := 3%positive.
Notation IDIII_overlap_z_overlap := 4%positive.
Notation IDIII_overlap_z_sample := 5%positive.
Notation IDIII_overlap_z_sb := 6%positive.
Definition III_overlap_z : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDIII_overlap_z_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDIII_overlap_z_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDIII_overlap_z__tmp
             (Some (EVar IDIII_overlap_z_sb))),5%positive)::
             (5%positive,(AAssign IDIII_overlap_z_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDIII_overlap_z_i)
             s) < (eval (ENum (18)) s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDIII_overlap_z_i)
             s) >= (eval (ENum (18)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDIII_overlap_z_i
             (Some (EAdd (EVar IDIII_overlap_z_i) (ENum (1))))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDIII_overlap_z_z (Some (EAdd (ENum (1))
             (EVar IDIII_overlap_z_z)))),17%positive)::
             (17%positive,AWeaken,8%positive)::nil
|}.

Definition III_overlap_z_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) <= 0)%Z
    | 4%positive => (-1 * (s IDIII_overlap_z_i) <= 0 /\ 1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) <= 0)%Z
    | 6%positive => (1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_i) <= 0 /\ -1 * (s IDIII_overlap_z_i) <= 0)%Z
    | 7%positive => (-1 * (s IDIII_overlap_z_i) <= 0 /\ 1 * (s IDIII_overlap_z_i) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_z) <= 0)%Z
    | 8%positive => (-1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) <= 0 /\ 1 * (s IDIII_overlap_z_i) + -18 <= 0)%Z
    | 9%positive => (1 * (s IDIII_overlap_z_i) + -18 <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) + 18 <= 0)%Z
    | 10%positive => (-1 * (s IDIII_overlap_z_i) + 18 <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_i) + -18 <= 0)%Z
    | 11%positive => (-1 * (s IDIII_overlap_z_i) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_i) + -17 <= 0)%Z
    | 12%positive => (1 * (s IDIII_overlap_z_i) + -17 <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) <= 0)%Z
    | 13%positive => (-1 * (s IDIII_overlap_z_i) <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0 /\ 1 * (s IDIII_overlap_z_i) + -17 <= 0)%Z
    | 14%positive => (-1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) + 1 <= 0 /\ 1 * (s IDIII_overlap_z_i) + -18 <= 0)%Z
    | 15%positive => (1 * (s IDIII_overlap_z_i) + -18 <= 0 /\ -1 * (s IDIII_overlap_z_i) + 1 <= 0 /\ -1 * (s IDIII_overlap_z_z) <= 0)%Z
    | 16%positive => (-1 * (s IDIII_overlap_z_z) <= 0 /\ -1 * (s IDIII_overlap_z_i) + 1 <= 0 /\ 1 * (s IDIII_overlap_z_i) + -18 <= 0)%Z
    | 17%positive => (1 * (s IDIII_overlap_z_i) + -18 <= 0 /\ -1 * (s IDIII_overlap_z_i) + 1 <= 0 /\ -1 * (s IDIII_overlap_z_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition III_overlap_z_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((18 # 1))%Q
    | 2%positive => ((18 # 1) + (s IDIII_overlap_z_z))%Q
    | 3%positive => ((18 # 1) + (s IDIII_overlap_z_z))%Q
    | 4%positive => ((18 # 1) + (s IDIII_overlap_z_z))%Q
    | 5%positive => ((18 # 1) + (s IDIII_overlap_z_z))%Q
    | 6%positive => ((s IDIII_overlap_z_z) + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 7%positive => ((s IDIII_overlap_z_z) + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 8%positive => ((s IDIII_overlap_z_z) + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 9%positive => ((s IDIII_overlap_z_z) + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 10%positive => ((s IDIII_overlap_z_z))%Q
    | 11%positive => ((s IDIII_overlap_z_z)
                      + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 12%positive => ((1 # 1) + (s IDIII_overlap_z_z)
                      + max0(17 - (s IDIII_overlap_z_i)))%Q
    | 13%positive => ((1 # 1) + (s IDIII_overlap_z_z)
                      + max0(17 - (s IDIII_overlap_z_i)))%Q
    | 14%positive => ((1 # 1) + (s IDIII_overlap_z_z)
                      + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 15%positive => ((1 # 1) + (s IDIII_overlap_z_z)
                      + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 16%positive => ((1 # 1) + (s IDIII_overlap_z_z)
                      + max0(18 - (s IDIII_overlap_z_i)))%Q
    | 17%positive => ((s IDIII_overlap_z_z)
                      + max0(18 - (s IDIII_overlap_z_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_overlap_z_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (18
                                                            - (s IDIII_overlap_z_i)) (17
                                                                    - (s IDIII_overlap_z_i)));
                     (*-1 0*) F_max0_ge_0 (17 - (s IDIII_overlap_z_i))]
    | 10%positive => []
    | 11%positive => [(*0 1*) F_max0_pre_decrement (18
                                                    - (s IDIII_overlap_z_i)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem III_overlap_z_ai_correct:
  forall s p' s', steps (g_start III_overlap_z) s (g_edges III_overlap_z) p' s' -> III_overlap_z_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_overlap_z_pot_correct:
  forall s p' s',
    steps (g_start III_overlap_z) s (g_edges III_overlap_z) p' s' ->
    (III_overlap_z_pot (g_start III_overlap_z) s >= III_overlap_z_pot p' s')%Q.
Proof.
  check_lp III_overlap_z_ai_correct III_overlap_z_hints.
Qed.

