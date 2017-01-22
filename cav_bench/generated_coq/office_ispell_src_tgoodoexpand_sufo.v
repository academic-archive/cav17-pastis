Require Import pasta.Pasta.

Notation IDexpand_suf_z := 1%positive.
Notation IDexpand_suf__tmp := 2%positive.
Notation IDexpand_suf__tmp1 := 3%positive.
Notation IDexpand_suf_entcount := 4%positive.
Notation IDexpand_suf_explength := 5%positive.
Notation IDexpand_suf_numsflags := 6%positive.
Notation IDexpand_suf_croot := 7%positive.
Notation IDexpand_suf_extra := 8%positive.
Notation IDexpand_suf_mask := 9%positive.
Notation IDexpand_suf_optflags := 10%positive.
Notation IDexpand_suf_option := 11%positive.
Notation IDexpand_suf_rootword := 12%positive.
Definition expand_suf : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDexpand_suf_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDexpand_suf__tmp1
             (Some (EVar IDexpand_suf_optflags))),3%positive)::
             (3%positive,(AAssign IDexpand_suf__tmp
             (Some (EVar IDexpand_suf_option))),4%positive)::
             (4%positive,(AAssign IDexpand_suf_entcount
             (Some (EVar IDexpand_suf_numsflags))),5%positive)::
             (5%positive,(AAssign IDexpand_suf_explength (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_suf_entcount) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_suf_entcount) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,20%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,17%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,19%positive)::
             (17%positive,(AAssign IDexpand_suf_explength None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDexpand_suf_entcount
             (Some (EAdd (EVar IDexpand_suf_entcount) (ENum (-1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDexpand_suf_z (Some (EAdd (ENum (1))
             (EVar IDexpand_suf_z)))),25%positive)::
             (25%positive,AWeaken,8%positive)::nil
|}.

Definition expand_suf_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 3%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ 1 * (s IDexpand_suf_z) <= 0)%Z
    | 4%positive => (1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 5%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ 1 * (s IDexpand_suf_z) <= 0)%Z
    | 6%positive => (1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0 /\ 1 * (s IDexpand_suf_explength) <= 0 /\ -1 * (s IDexpand_suf_explength) <= 0)%Z
    | 7%positive => (-1 * (s IDexpand_suf_explength) <= 0 /\ 1 * (s IDexpand_suf_explength) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0 /\ 1 * (s IDexpand_suf_z) <= 0)%Z
    | 8%positive => (-1 * (s IDexpand_suf_z) <= 0)%Z
    | 9%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ 1 * (s IDexpand_suf_entcount) <= 0)%Z
    | 10%positive => (1 * (s IDexpand_suf_entcount) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 11%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDexpand_suf_entcount) + 1 <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 13%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDexpand_suf_entcount) + 1 <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 15%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDexpand_suf_entcount) + 1 <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 17%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDexpand_suf_entcount) + 1 <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 19%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDexpand_suf_entcount) + 1 <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 21%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) <= 0)%Z
    | 23%positive => (-1 * (s IDexpand_suf_entcount) <= 0 /\ -1 * (s IDexpand_suf_z) <= 0)%Z
    | 24%positive => (-1 * (s IDexpand_suf_z) <= 0 /\ -1 * (s IDexpand_suf_entcount) <= 0)%Z
    | 25%positive => (-1 * (s IDexpand_suf_entcount) <= 0 /\ -1 * (s IDexpand_suf_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition expand_suf_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDexpand_suf_numsflags)))%Q
    | 2%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_numsflags)))%Q
    | 3%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_numsflags)))%Q
    | 4%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_numsflags)))%Q
    | 5%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 6%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 7%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 8%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 9%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 10%positive => ((s IDexpand_suf_z))%Q
    | 11%positive => ((s IDexpand_suf_z) + max0((s IDexpand_suf_entcount)))%Q
    | 12%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 13%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 14%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 15%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 16%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 17%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 18%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 19%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 20%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 21%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | 22%positive => ((1 # 1) + (s IDexpand_suf_entcount)
                      + (s IDexpand_suf_z))%Q
    | 23%positive => ((1 # 1) + (s IDexpand_suf_entcount)
                      + (s IDexpand_suf_z))%Q
    | 24%positive => ((1 # 1) + (s IDexpand_suf_entcount)
                      + (s IDexpand_suf_z))%Q
    | 25%positive => ((s IDexpand_suf_entcount) + (s IDexpand_suf_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition expand_suf_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDexpand_suf_entcount)) (-1
                                                                    + (s IDexpand_suf_entcount)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDexpand_suf_entcount))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_suf_entcount))) (F_check_ge ((s IDexpand_suf_entcount)) (0))]
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
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_suf_entcount)) (0))) (F_max0_ge_0 ((s IDexpand_suf_entcount)))]
    | _ => []
  end.


Theorem expand_suf_ai_correct:
  forall s p' s', steps (g_start expand_suf) s (g_edges expand_suf) p' s' -> expand_suf_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem expand_suf_pot_correct:
  forall s p' s',
    steps (g_start expand_suf) s (g_edges expand_suf) p' s' ->
    (expand_suf_pot (g_start expand_suf) s >= expand_suf_pot p' s')%Q.
Proof.
  check_lp expand_suf_ai_correct expand_suf_hints.
Qed.

