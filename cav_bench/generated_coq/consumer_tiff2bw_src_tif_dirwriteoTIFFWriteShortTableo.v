Require Import pasta.Pasta.

Notation IDTIFFWriteShortTable_z := 1%positive.
Notation IDTIFFWriteShortTable__tmp := 2%positive.
Notation IDTIFFWriteShortTable__tmp1 := 3%positive.
Notation IDTIFFWriteShortTable__tmp2 := 4%positive.
Notation IDTIFFWriteShortTable_i := 5%positive.
Notation IDTIFFWriteShortTable_off := 6%positive.
Notation IDTIFFWriteShortTable_dir := 7%positive.
Notation IDTIFFWriteShortTable_n := 8%positive.
Notation IDTIFFWriteShortTable_table := 9%positive.
Notation IDTIFFWriteShortTable_tag := 10%positive.
Notation IDTIFFWriteShortTable_tif := 11%positive.
Definition TIFFWriteShortTable : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDTIFFWriteShortTable_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteShortTable_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteShortTable__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDTIFFWriteShortTable__tmp2
             (Some (EVar IDTIFFWriteShortTable_tag))),6%positive)::
             (6%positive,(AAssign IDTIFFWriteShortTable__tmp
             (Some (EVar IDTIFFWriteShortTable_n))),7%positive)::
             (7%positive,(AAssign IDTIFFWriteShortTable_off None),8%positive)::
             (8%positive,(AAssign IDTIFFWriteShortTable_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteShortTable_i) s) <
             (eval (EVar IDTIFFWriteShortTable__tmp) s))%Z)),16%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteShortTable_i) s) >=
             (eval (EVar IDTIFFWriteShortTable__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDTIFFWriteShortTable__tmp1
             (Some (ENum (1)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,21%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,22%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDTIFFWriteShortTable__tmp1
             (Some (ENum (0)))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDTIFFWriteShortTable_i
             (Some (EAdd (EVar IDTIFFWriteShortTable_i) (ENum (1))))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDTIFFWriteShortTable_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteShortTable_z)))),
             27%positive)::(27%positive,AWeaken,11%positive)::nil
|}.

Definition TIFFWriteShortTable_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFWriteShortTable__tmp) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 6%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp)+ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 13%positive => (1 * (s IDTIFFWriteShortTable__tmp)+ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp)+ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp1) + -1 <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp1) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFWriteShortTable__tmp1) + 1 <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp1) + -1 <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp)+ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 16%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 18%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp1) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp1) <= 0)%Z
    | 20%positive => (-1 * (s IDTIFFWriteShortTable__tmp1) <= 0 /\ 1 * (s IDTIFFWriteShortTable__tmp1) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDTIFFWriteShortTable__tmp1) + -1 <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp1) <= 0)%Z
    | 22%positive => (-1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFWriteShortTable_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFWriteShortTable_z) <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDTIFFWriteShortTable_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteShortTable__tmp)+ 1 * (s IDTIFFWriteShortTable_i) <= 0 /\ -1 * (s IDTIFFWriteShortTable_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFWriteShortTable_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDTIFFWriteShortTable_n)))%Q
    | 2%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable_n)))%Q
    | 3%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable_n)))%Q
    | 4%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable_n)))%Q
    | 5%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable_n)))%Q
    | 6%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable_n)))%Q
    | 7%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable__tmp)))%Q
    | 8%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable__tmp)))%Q
    | 9%positive => ((s IDTIFFWriteShortTable_z)
                     + max0((s IDTIFFWriteShortTable__tmp)
                            - (s IDTIFFWriteShortTable_i)))%Q
    | 10%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 11%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 12%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 13%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 14%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 15%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 16%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 17%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 18%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 19%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 20%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 21%positive => ((s IDTIFFWriteShortTable_z))%Q
    | 22%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 23%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0(-1 + (s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 24%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 25%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 26%positive => ((1 # 1) + (s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | 27%positive => ((s IDTIFFWriteShortTable_z)
                      + max0((s IDTIFFWriteShortTable__tmp)
                             - (s IDTIFFWriteShortTable_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFWriteShortTable_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteShortTable__tmp)
                                                             - (s IDTIFFWriteShortTable_i)) (-1
                                                                    + (s IDTIFFWriteShortTable__tmp)
                                                                    - (s IDTIFFWriteShortTable_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFWriteShortTable__tmp)
                                            - (s IDTIFFWriteShortTable_i))]
    | 16%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFWriteShortTable__tmp)
                                                     - (s IDTIFFWriteShortTable_i)) (1)]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFWriteShortTable__tmp)
                                            - (s IDTIFFWriteShortTable_i))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | _ => []
  end.


Theorem TIFFWriteShortTable_ai_correct:
  forall s p' s', steps (g_start TIFFWriteShortTable) s (g_edges TIFFWriteShortTable) p' s' -> TIFFWriteShortTable_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFWriteShortTable_pot_correct:
  forall s p' s',
    steps (g_start TIFFWriteShortTable) s (g_edges TIFFWriteShortTable) p' s' ->
    (TIFFWriteShortTable_pot (g_start TIFFWriteShortTable) s >= TIFFWriteShortTable_pot p' s')%Q.
Proof.
  check_lp TIFFWriteShortTable_ai_correct TIFFWriteShortTable_hints.
Qed.

