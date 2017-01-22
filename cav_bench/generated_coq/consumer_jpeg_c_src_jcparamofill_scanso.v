Require Import pasta.Pasta.

Notation IDfill_scans_z := 1%positive.
Notation IDfill_scans__tmp := 2%positive.
Notation IDfill_scans__tmp1 := 3%positive.
Notation IDfill_scans__tmp2 := 4%positive.
Notation IDfill_scans__tmp3 := 5%positive.
Notation IDfill_scans__tmp4 := 6%positive.
Notation IDfill_scans_ci := 7%positive.
Notation IDfill_scans_Ah := 8%positive.
Notation IDfill_scans_Al := 9%positive.
Notation IDfill_scans_Se := 10%positive.
Notation IDfill_scans_Ss := 11%positive.
Notation IDfill_scans_ncomps := 12%positive.
Notation IDfill_scans_scanptr := 13%positive.
Definition fill_scans : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDfill_scans_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfill_scans__tmp
             (Some (EVar IDfill_scans_ncomps))),3%positive)::
             (3%positive,(AAssign IDfill_scans__tmp4
             (Some (EVar IDfill_scans_Ss))),4%positive)::
             (4%positive,(AAssign IDfill_scans__tmp3
             (Some (EVar IDfill_scans_Se))),5%positive)::
             (5%positive,(AAssign IDfill_scans__tmp2
             (Some (EVar IDfill_scans_Ah))),6%positive)::
             (6%positive,(AAssign IDfill_scans__tmp1
             (Some (EVar IDfill_scans_Al))),7%positive)::
             (7%positive,(AAssign IDfill_scans_ci (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDfill_scans_ci)
             s) < (eval (EVar IDfill_scans__tmp) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDfill_scans_ci)
             s) >= (eval (EVar IDfill_scans__tmp) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDfill_scans_ci
             (Some (EAdd (EVar IDfill_scans_ci) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDfill_scans_z (Some (EAdd (ENum (1))
             (EVar IDfill_scans_z)))),19%positive)::
             (19%positive,AWeaken,10%positive)::nil
|}.

Definition fill_scans_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans_z) <= 0)%Z
    | 4%positive => (1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans_z) <= 0)%Z
    | 6%positive => (1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans_z) <= 0)%Z
    | 8%positive => (1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_ci) <= 0)%Z
    | 9%positive => (-1 * (s IDfill_scans_ci) <= 0 /\ 1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans_z) <= 0)%Z
    | 10%positive => (-1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_ci) <= 0)%Z
    | 11%positive => (-1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ 1 * (s IDfill_scans__tmp)+ -1 * (s IDfill_scans_ci) <= 0)%Z
    | 12%positive => (1 * (s IDfill_scans__tmp)+ -1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_ci) <= 0)%Z
    | 13%positive => (-1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) + 1 <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_ci) <= 0)%Z
    | 15%positive => (-1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_ci) + 1 <= 0 /\ -1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) <= 0)%Z
    | 17%positive => (-1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_ci) + 1 <= 0 /\ -1 * (s IDfill_scans_z) <= 0)%Z
    | 18%positive => (-1 * (s IDfill_scans_z) <= 0 /\ -1 * (s IDfill_scans_ci) + 1 <= 0 /\ -1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) <= 0)%Z
    | 19%positive => (-1 * (s IDfill_scans__tmp)+ 1 * (s IDfill_scans_ci) <= 0 /\ -1 * (s IDfill_scans_ci) + 1 <= 0 /\ -1 * (s IDfill_scans_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition fill_scans_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDfill_scans_ncomps)))%Q
    | 2%positive => ((s IDfill_scans_z) + max0((s IDfill_scans_ncomps)))%Q
    | 3%positive => ((s IDfill_scans_z) + max0((s IDfill_scans__tmp)))%Q
    | 4%positive => ((s IDfill_scans_z) + max0((s IDfill_scans__tmp)))%Q
    | 5%positive => ((s IDfill_scans_z) + max0((s IDfill_scans__tmp)))%Q
    | 6%positive => ((s IDfill_scans_z) + max0((s IDfill_scans__tmp)))%Q
    | 7%positive => ((s IDfill_scans_z) + max0((s IDfill_scans__tmp)))%Q
    | 8%positive => ((s IDfill_scans_z)
                     + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 9%positive => ((s IDfill_scans_z)
                     + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 10%positive => ((s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 11%positive => ((s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 12%positive => ((s IDfill_scans_z))%Q
    | 13%positive => ((s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 14%positive => ((1 # 1) + (s IDfill_scans_z)
                      + max0(-1 + (s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 15%positive => ((1 # 1) + (s IDfill_scans_z)
                      + max0(-1 + (s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 16%positive => ((1 # 1) + (s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 17%positive => ((1 # 1) + (s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 18%positive => ((1 # 1) + (s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | 19%positive => ((s IDfill_scans_z)
                      + max0((s IDfill_scans__tmp) - (s IDfill_scans_ci)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fill_scans_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfill_scans__tmp)
                                                             - (s IDfill_scans_ci)) (-1
                                                                    + (s IDfill_scans__tmp)
                                                                    - (s IDfill_scans_ci)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDfill_scans__tmp)
                                                                 - (s IDfill_scans_ci))) (F_check_ge (0) (0))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfill_scans__tmp)
                                                     - (s IDfill_scans_ci)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem fill_scans_ai_correct:
  forall s p' s', steps (g_start fill_scans) s (g_edges fill_scans) p' s' -> fill_scans_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fill_scans_pot_correct:
  forall s p' s',
    steps (g_start fill_scans) s (g_edges fill_scans) p' s' ->
    (fill_scans_pot (g_start fill_scans) s >= fill_scans_pot p' s')%Q.
Proof.
  check_lp fill_scans_ai_correct fill_scans_hints.
Qed.

