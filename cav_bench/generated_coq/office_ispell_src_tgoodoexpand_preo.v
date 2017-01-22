Require Import pasta.Pasta.

Notation IDexpand_pre_z := 1%positive.
Notation IDexpand_pre__tmp := 2%positive.
Notation IDexpand_pre_entcount := 3%positive.
Notation IDexpand_pre_explength := 4%positive.
Notation IDexpand_pre_numpflags := 5%positive.
Notation IDexpand_pre_croot := 6%positive.
Notation IDexpand_pre_extra := 7%positive.
Notation IDexpand_pre_mask := 8%positive.
Notation IDexpand_pre_option := 9%positive.
Notation IDexpand_pre_rootword := 10%positive.
Definition expand_pre : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDexpand_pre_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDexpand_pre__tmp
             (Some (EVar IDexpand_pre_option))),3%positive)::
             (3%positive,(AAssign IDexpand_pre_entcount
             (Some (EVar IDexpand_pre_numpflags))),4%positive)::
             (4%positive,(AAssign IDexpand_pre_explength (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_pre_entcount) s) >
             (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_pre_entcount) s) <=
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (11%positive,ANone,14%positive)::
             (12%positive,(AAssign IDexpand_pre_explength None),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDexpand_pre_entcount
             (Some (EAdd (EVar IDexpand_pre_entcount) (ENum (-1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDexpand_pre_z (Some (EAdd (ENum (1))
             (EVar IDexpand_pre_z)))),19%positive)::
             (19%positive,AWeaken,7%positive)::nil
|}.

Definition expand_pre_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 3%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ 1 * (s IDexpand_pre_z) <= 0)%Z
    | 4%positive => (1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 5%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ 1 * (s IDexpand_pre_z) <= 0 /\ 1 * (s IDexpand_pre_explength) <= 0 /\ -1 * (s IDexpand_pre_explength) <= 0)%Z
    | 6%positive => (-1 * (s IDexpand_pre_explength) <= 0 /\ 1 * (s IDexpand_pre_explength) <= 0 /\ 1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 7%positive => (-1 * (s IDexpand_pre_z) <= 0)%Z
    | 8%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ 1 * (s IDexpand_pre_entcount) <= 0)%Z
    | 9%positive => (1 * (s IDexpand_pre_entcount) <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 10%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_entcount) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDexpand_pre_entcount) + 1 <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 12%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_entcount) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDexpand_pre_entcount) + 1 <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 14%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_entcount) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDexpand_pre_entcount) + 1 <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 16%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_entcount) <= 0)%Z
    | 17%positive => (-1 * (s IDexpand_pre_entcount) <= 0 /\ -1 * (s IDexpand_pre_z) <= 0)%Z
    | 18%positive => (-1 * (s IDexpand_pre_z) <= 0 /\ -1 * (s IDexpand_pre_entcount) <= 0)%Z
    | 19%positive => (-1 * (s IDexpand_pre_entcount) <= 0 /\ -1 * (s IDexpand_pre_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition expand_pre_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDexpand_pre_numpflags)))%Q
    | 2%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_numpflags)))%Q
    | 3%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_numpflags)))%Q
    | 4%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 5%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 6%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 7%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 8%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 9%positive => ((s IDexpand_pre_z))%Q
    | 10%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | 11%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0(-1 + (s IDexpand_pre_entcount)))%Q
    | 12%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0(-1 + (s IDexpand_pre_entcount)))%Q
    | 13%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0(-1 + (s IDexpand_pre_entcount)))%Q
    | 14%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0(-1 + (s IDexpand_pre_entcount)))%Q
    | 15%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0(-1 + (s IDexpand_pre_entcount)))%Q
    | 16%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0((s IDexpand_pre_entcount)))%Q
    | 17%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0((s IDexpand_pre_entcount)))%Q
    | 18%positive => ((1 # 1) + (s IDexpand_pre_z)
                      + max0((s IDexpand_pre_entcount)))%Q
    | 19%positive => ((s IDexpand_pre_z) + max0((s IDexpand_pre_entcount)))%Q
    | _ => (0 # 1)%Q
  end.

Definition expand_pre_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDexpand_pre_entcount)) (-1
                                                                    + (s IDexpand_pre_entcount)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDexpand_pre_entcount))) (F_check_ge (0) (0))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement ((s IDexpand_pre_entcount)) (1)]
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


Theorem expand_pre_ai_correct:
  forall s p' s', steps (g_start expand_pre) s (g_edges expand_pre) p' s' -> expand_pre_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem expand_pre_pot_correct:
  forall s p' s',
    steps (g_start expand_pre) s (g_edges expand_pre) p' s' ->
    (expand_pre_pot (g_start expand_pre) s >= expand_pre_pot p' s')%Q.
Proof.
  check_lp expand_pre_ai_correct expand_pre_hints.
Qed.

