Require Import pasta.Pasta.

Notation IDlookup_offset_z := 1%positive.
Notation IDlookup_offset__tmp := 2%positive.
Notation IDlookup_offset__tmp1 := 3%positive.
Notation IDlookup_offset_i := 4%positive.
Notation IDlookup_offset_nmsg := 5%positive.
Notation IDlookup_offset_crc := 6%positive.
Definition lookup_offset : graph := {|
  g_start := 1%positive;
  g_end := 22%positive;
  g_edges := (1%positive,(AAssign IDlookup_offset_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDlookup_offset__tmp
             (Some (EVar IDlookup_offset_crc))),3%positive)::
             (3%positive,(AAssign IDlookup_offset_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDlookup_offset_i)
             s) < (eval (EVar IDlookup_offset_nmsg) s))%Z)),11%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDlookup_offset_i)
             s) >= (eval (EVar IDlookup_offset_nmsg) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDlookup_offset__tmp1 (Some (ENum (-1)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,22%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,19%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDlookup_offset_i
             (Some (EAdd (EVar IDlookup_offset_i) (ENum (1))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDlookup_offset_z (Some (EAdd (ENum (1))
             (EVar IDlookup_offset_z)))),18%positive)::
             (18%positive,AWeaken,6%positive)::
             (19%positive,(AAssign IDlookup_offset__tmp1 None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::nil
|}.

Definition lookup_offset_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0)%Z
    | 3%positive => (-1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_z) <= 0)%Z
    | 4%positive => (1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 5%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ 1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_z) <= 0)%Z
    | 6%positive => (-1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 7%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i)+ 1 * (s IDlookup_offset_nmsg) <= 0)%Z
    | 8%positive => (-1 * (s IDlookup_offset_i)+ 1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 9%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i)+ 1 * (s IDlookup_offset_nmsg) <= 0 /\ 1 * (s IDlookup_offset__tmp1) + 1 <= 0 /\ -1 * (s IDlookup_offset__tmp1) + -1 <= 0)%Z
    | 10%positive => (-1 * (s IDlookup_offset__tmp1) + -1 <= 0 /\ 1 * (s IDlookup_offset__tmp1) + 1 <= 0 /\ -1 * (s IDlookup_offset_i)+ 1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 11%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 13%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 15%positive => (-1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDlookup_offset_i) + 1 <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0)%Z
    | 17%positive => (-1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDlookup_offset_i) + 1 <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) <= 0 /\ -1 * (s IDlookup_offset_z) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | 21%positive => (-1 * (s IDlookup_offset_i) <= 0 /\ -1 * (s IDlookup_offset_z) <= 0 /\ 1 * (s IDlookup_offset_i)+ -1 * (s IDlookup_offset_nmsg) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDlookup_offset_z) <= 0 /\ -1 * (s IDlookup_offset_i) <= 0)%Z
    | _ => False
  end.

Definition lookup_offset_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDlookup_offset_nmsg)))%Q
    | 2%positive => ((s IDlookup_offset_z) + max0((s IDlookup_offset_nmsg)))%Q
    | 3%positive => ((s IDlookup_offset_z) + max0((s IDlookup_offset_nmsg)))%Q
    | 4%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 5%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 6%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 7%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 8%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 9%positive => ((s IDlookup_offset_z)
                     + max0(-(s IDlookup_offset_i) + (s IDlookup_offset_nmsg)))%Q
    | 10%positive => ((s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 11%positive => ((s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 12%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 13%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 14%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 15%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 16%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 17%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 18%positive => ((s IDlookup_offset_z)
                      + max0(-(s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 19%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 20%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 21%positive => ((1 # 1) + (s IDlookup_offset_z)
                      + max0(-1 - (s IDlookup_offset_i)
                             + (s IDlookup_offset_nmsg)))%Q
    | 22%positive => ((s IDlookup_offset_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition lookup_offset_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDlookup_offset_i)
                                                             + (s IDlookup_offset_nmsg)) (-1
                                                                    - (s IDlookup_offset_i)
                                                                    + (s IDlookup_offset_nmsg)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDlookup_offset_i)
                                            + (s IDlookup_offset_nmsg))]
    | 11%positive => [(*0 1*) F_max0_pre_decrement (-(s IDlookup_offset_i)
                                                    + (s IDlookup_offset_nmsg)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDlookup_offset_i)
                                            + (s IDlookup_offset_nmsg))]
    | 22%positive => []
    | _ => []
  end.


Theorem lookup_offset_ai_correct:
  forall s p' s', steps (g_start lookup_offset) s (g_edges lookup_offset) p' s' -> lookup_offset_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem lookup_offset_pot_correct:
  forall s p' s',
    steps (g_start lookup_offset) s (g_edges lookup_offset) p' s' ->
    (lookup_offset_pot (g_start lookup_offset) s >= lookup_offset_pot p' s')%Q.
Proof.
  check_lp lookup_offset_ai_correct lookup_offset_hints.
Qed.

