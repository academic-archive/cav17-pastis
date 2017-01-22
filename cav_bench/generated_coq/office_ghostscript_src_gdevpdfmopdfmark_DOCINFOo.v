Require Import pasta.Pasta.

Notation IDpdfmark_DOCINFO_z := 1%positive.
Notation IDpdfmark_DOCINFO__tmp := 2%positive.
Notation IDpdfmark_DOCINFO__tmp1 := 3%positive.
Notation IDpdfmark_DOCINFO_i := 4%positive.
Notation IDpdfmark_DOCINFO_info_id := 5%positive.
Notation IDpdfmark_DOCINFO_count := 6%positive.
Notation IDpdfmark_DOCINFO_pairs := 7%positive.
Notation IDpdfmark_DOCINFO_pctm := 8%positive.
Notation IDpdfmark_DOCINFO_pdev := 9%positive.
Definition pdfmark_DOCINFO : graph := {|
  g_start := 1%positive;
  g_end := 33%positive;
  g_edges := (1%positive,(AAssign IDpdfmark_DOCINFO_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_DOCINFO_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_DOCINFO__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpdfmark_DOCINFO__tmp1
             (Some (EVar IDpdfmark_DOCINFO_count))),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (7%positive,ANone,10%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,30%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDpdfmark_DOCINFO_info_id None),
             11%positive)::
             (11%positive,(AAssign IDpdfmark_DOCINFO_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_DOCINFO_i)
             s) < (eval (EVar IDpdfmark_DOCINFO__tmp1) s))%Z)),19%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_DOCINFO_i)
             s) >= (eval (EVar IDpdfmark_DOCINFO__tmp1) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDpdfmark_DOCINFO__tmp (Some (ENum (0)))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,AWeaken,33%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,24%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDpdfmark_DOCINFO_i
             (Some (EAdd (EVar IDpdfmark_DOCINFO_i) (ENum (2))))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDpdfmark_DOCINFO_z (Some (EAdd (ENum (1))
             (EVar IDpdfmark_DOCINFO_z)))),29%positive)::
             (29%positive,AWeaken,14%positive)::
             (30%positive,(AAssign IDpdfmark_DOCINFO__tmp
             (Some (ENum (-13)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::nil
|}.

Definition pdfmark_DOCINFO_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1) <= 0)%Z
    | 5%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 6%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 8%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 11%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 12%positive => (1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 13%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 14%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 15%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp1)+ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 16%positive => (1 * (s IDpdfmark_DOCINFO__tmp1)+ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 17%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp1)+ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDpdfmark_DOCINFO__tmp) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp1)+ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 19%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 21%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 23%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0)%Z
    | 25%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) + 2 <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + -1 <= 0)%Z
    | 27%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + -1 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) + 2 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 28%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) + 2 <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + -1 <= 0)%Z
    | 29%positive => (-1 * (s IDpdfmark_DOCINFO__tmp1)+ 1 * (s IDpdfmark_DOCINFO_i) + -1 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) + 2 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 31%positive => (-1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp) + 13 <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp) + -13 <= 0)%Z
    | 32%positive => (-1 * (s IDpdfmark_DOCINFO__tmp) + -13 <= 0 /\ 1 * (s IDpdfmark_DOCINFO__tmp) + 13 <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ 1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0)%Z
    | 33%positive => (1 * (s IDpdfmark_DOCINFO__tmp) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_z) <= 0 /\ -1 * (s IDpdfmark_DOCINFO_i) <= 0 /\ -1 * (s IDpdfmark_DOCINFO__tmp) + -13 <= 0)%Z
    | _ => False
  end.

Definition pdfmark_DOCINFO_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 2) * max0(1 + (s IDpdfmark_DOCINFO_count)))%Q
    | 2%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO_count)))%Q
    | 3%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO_count)))%Q
    | 4%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO_count)))%Q
    | 5%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO_count)))%Q
    | 6%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 7%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 8%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 9%positive => ((s IDpdfmark_DOCINFO_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 10%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 11%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 12%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 13%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 14%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 15%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 16%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 17%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 18%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 19%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 20%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 21%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 22%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 23%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 24%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 25%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 26%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 27%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 28%positive => ((1 # 1) + (s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 29%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)
                                       - (s IDpdfmark_DOCINFO_i)))%Q
    | 30%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 31%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 32%positive => ((s IDpdfmark_DOCINFO_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_DOCINFO__tmp1)))%Q
    | 33%positive => ((s IDpdfmark_DOCINFO_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pdfmark_DOCINFO_hints (p : node) (s : state) := 
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
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_DOCINFO__tmp1)
                                                               - (s IDpdfmark_DOCINFO_i)) (-1
                                                                    + (s IDpdfmark_DOCINFO__tmp1)
                                                                    - (s IDpdfmark_DOCINFO_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_DOCINFO__tmp1)
                                              - (s IDpdfmark_DOCINFO_i))]
    | 19%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_DOCINFO__tmp1)
                                                       - (s IDpdfmark_DOCINFO_i)) (2)]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.5 0*) F_max0_ge_0 (1 + (s IDpdfmark_DOCINFO__tmp1))]
    | 33%positive => []
    | _ => []
  end.


Theorem pdfmark_DOCINFO_ai_correct:
  forall s p' s', steps (g_start pdfmark_DOCINFO) s (g_edges pdfmark_DOCINFO) p' s' -> pdfmark_DOCINFO_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pdfmark_DOCINFO_pot_correct:
  forall s p' s',
    steps (g_start pdfmark_DOCINFO) s (g_edges pdfmark_DOCINFO) p' s' ->
    (pdfmark_DOCINFO_pot (g_start pdfmark_DOCINFO) s >= pdfmark_DOCINFO_pot p' s')%Q.
Proof.
  check_lp pdfmark_DOCINFO_ai_correct pdfmark_DOCINFO_hints.
Qed.

