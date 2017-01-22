Require Import pasta.Pasta.

Notation IDpdfmark_find_key_z := 1%positive.
Notation IDpdfmark_find_key__tmp := 2%positive.
Notation IDpdfmark_find_key__tmp1 := 3%positive.
Notation IDpdfmark_find_key_i := 4%positive.
Notation IDpdfmark_find_key_count := 5%positive.
Notation IDpdfmark_find_key_key := 6%positive.
Notation IDpdfmark_find_key_pairs := 7%positive.
Notation IDpdfmark_find_key_pstr := 8%positive.
Definition pdfmark_find_key : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDpdfmark_find_key_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_find_key_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_find_key__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpdfmark_find_key__tmp
             (Some (EVar IDpdfmark_find_key_count))),6%positive)::
             (6%positive,(AAssign IDpdfmark_find_key_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_find_key_i)
             s) < (eval (EVar IDpdfmark_find_key__tmp) s))%Z)),14%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpdfmark_find_key_i)
             s) >= (eval (EVar IDpdfmark_find_key__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDpdfmark_find_key__tmp1
             (Some (ENum (0)))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,25%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,22%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDpdfmark_find_key_i
             (Some (EAdd (EVar IDpdfmark_find_key_i) (ENum (2))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDpdfmark_find_key_z
             (Some (EAdd (ENum (1)) (EVar IDpdfmark_find_key_z)))),
             21%positive)::(21%positive,AWeaken,9%positive)::
             (22%positive,(AAssign IDpdfmark_find_key__tmp1
             (Some (ENum (1)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition pdfmark_find_key_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDpdfmark_find_key__tmp) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 6%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 8%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp)+ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 11%positive => (1 * (s IDpdfmark_find_key__tmp)+ -1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 12%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp)+ -1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp1) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp1) <= 0)%Z
    | 13%positive => (-1 * (s IDpdfmark_find_key__tmp1) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp1) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp)+ -1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 14%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 16%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0)%Z
    | 18%positive => (-1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key_i) + 2 <= 0)%Z
    | 19%positive => (-1 * (s IDpdfmark_find_key_i) + 2 <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0)%Z
    | 20%positive => (-1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key_i) + 2 <= 0)%Z
    | 21%positive => (-1 * (s IDpdfmark_find_key_i) + 2 <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp1) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key__tmp1) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDpdfmark_find_key__tmp1) + 1 <= 0 /\ 1 * (s IDpdfmark_find_key__tmp1) + -1 <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key__tmp)+ 1 * (s IDpdfmark_find_key_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDpdfmark_find_key__tmp1) <= 0 /\ -1 * (s IDpdfmark_find_key_z) <= 0 /\ -1 * (s IDpdfmark_find_key_i) <= 0 /\ 1 * (s IDpdfmark_find_key__tmp1) + -1 <= 0)%Z
    | _ => False
  end.

Definition pdfmark_find_key_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 2) * max0(1 + (s IDpdfmark_find_key_count)))%Q
    | 2%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key_count)))%Q
    | 3%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key_count)))%Q
    | 4%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key_count)))%Q
    | 5%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key_count)))%Q
    | 6%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)))%Q
    | 7%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                      - (s IDpdfmark_find_key_i)))%Q
    | 8%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                      - (s IDpdfmark_find_key_i)))%Q
    | 9%positive => ((s IDpdfmark_find_key_z)
                     + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                      - (s IDpdfmark_find_key_i)))%Q
    | 10%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 11%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 12%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 13%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 14%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 15%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 16%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 17%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 18%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 19%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 20%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 21%positive => ((s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 22%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 23%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 24%positive => ((1 # 1) + (s IDpdfmark_find_key_z)
                      + (1 # 2) * max0(-1 + (s IDpdfmark_find_key__tmp)
                                       - (s IDpdfmark_find_key_i)))%Q
    | 25%positive => ((s IDpdfmark_find_key_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pdfmark_find_key_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_find_key__tmp)
                                                               - (s IDpdfmark_find_key_i)) (-1
                                                                    + (s IDpdfmark_find_key__tmp)
                                                                    - (s IDpdfmark_find_key_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_find_key__tmp)
                                              - (s IDpdfmark_find_key_i))]
    | 14%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_find_key__tmp)
                                                       - (s IDpdfmark_find_key_i)) (2)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_one;
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_find_key__tmp)
                                              - (s IDpdfmark_find_key_i))]
    | 25%positive => []
    | _ => []
  end.


Theorem pdfmark_find_key_ai_correct:
  forall s p' s', steps (g_start pdfmark_find_key) s (g_edges pdfmark_find_key) p' s' -> pdfmark_find_key_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pdfmark_find_key_pot_correct:
  forall s p' s',
    steps (g_start pdfmark_find_key) s (g_edges pdfmark_find_key) p' s' ->
    (pdfmark_find_key_pot (g_start pdfmark_find_key) s >= pdfmark_find_key_pot p' s')%Q.
Proof.
  check_lp pdfmark_find_key_ai_correct pdfmark_find_key_hints.
Qed.

