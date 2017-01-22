Require Import pasta.Pasta.

Notation IDput_demapped_gray_z := 1%positive.
Notation IDput_demapped_gray__tmp := 2%positive.
Notation IDput_demapped_gray_cinfo_dref_off128 := 3%positive.
Notation IDput_demapped_gray_col := 4%positive.
Notation IDput_demapped_gray_cinfo := 5%positive.
Notation IDput_demapped_gray_dinfo := 6%positive.
Notation IDput_demapped_gray_rows_supplied := 7%positive.
Definition put_demapped_gray : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDput_demapped_gray_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDput_demapped_gray_col) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDput_demapped_gray__tmp
             (Some (EVar IDput_demapped_gray_rows_supplied))),5%positive)::
             (5%positive,(AAssign IDput_demapped_gray_col
             (Some (EVar IDput_demapped_gray_cinfo_dref_off128))),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDput_demapped_gray_col) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDput_demapped_gray_col) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDput_demapped_gray_col
             (Some (EAdd (EVar IDput_demapped_gray_col) (ENum (-1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDput_demapped_gray_z
             (Some (EAdd (ENum (1)) (EVar IDput_demapped_gray_z)))),
             17%positive)::(17%positive,AWeaken,8%positive)::nil
|}.

Definition put_demapped_gray_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 3%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ 1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) <= 0)%Z
    | 4%positive => (-1 * (s IDput_demapped_gray_col) <= 0 /\ 1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 5%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ 1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) <= 0)%Z
    | 6%positive => (1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 7%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ 1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 8%positive => (-1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 9%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ 1 * (s IDput_demapped_gray_col) <= 0)%Z
    | 10%positive => (1 * (s IDput_demapped_gray_col) <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 11%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDput_demapped_gray_col) + 1 <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 13%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) <= 0)%Z
    | 15%positive => (-1 * (s IDput_demapped_gray_col) <= 0 /\ -1 * (s IDput_demapped_gray_z) <= 0)%Z
    | 16%positive => (-1 * (s IDput_demapped_gray_z) <= 0 /\ -1 * (s IDput_demapped_gray_col) <= 0)%Z
    | 17%positive => (-1 * (s IDput_demapped_gray_col) <= 0 /\ -1 * (s IDput_demapped_gray_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition put_demapped_gray_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDput_demapped_gray_cinfo_dref_off128)))%Q
    | 2%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_cinfo_dref_off128)))%Q
    | 3%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_cinfo_dref_off128)))%Q
    | 4%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_cinfo_dref_off128)))%Q
    | 5%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_cinfo_dref_off128)))%Q
    | 6%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_col)))%Q
    | 7%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_col)))%Q
    | 8%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_col)))%Q
    | 9%positive => ((s IDput_demapped_gray_z)
                     + max0((s IDput_demapped_gray_col)))%Q
    | 10%positive => ((s IDput_demapped_gray_z))%Q
    | 11%positive => ((s IDput_demapped_gray_z)
                      + max0((s IDput_demapped_gray_col)))%Q
    | 12%positive => ((1 # 1) + (s IDput_demapped_gray_z)
                      + max0(-1 + (s IDput_demapped_gray_col)))%Q
    | 13%positive => ((1 # 1) + (s IDput_demapped_gray_z)
                      + max0(-1 + (s IDput_demapped_gray_col)))%Q
    | 14%positive => ((1 # 1) + (s IDput_demapped_gray_z)
                      + max0((s IDput_demapped_gray_col)))%Q
    | 15%positive => ((1 # 1) + (s IDput_demapped_gray_z)
                      + max0((s IDput_demapped_gray_col)))%Q
    | 16%positive => ((1 # 1) + (s IDput_demapped_gray_z)
                      + max0((s IDput_demapped_gray_col)))%Q
    | 17%positive => ((s IDput_demapped_gray_z)
                      + max0((s IDput_demapped_gray_col)))%Q
    | _ => (0 # 1)%Q
  end.

Definition put_demapped_gray_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDput_demapped_gray_col)) (-1
                                                                    + (s IDput_demapped_gray_col)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDput_demapped_gray_col))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDput_demapped_gray_col)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem put_demapped_gray_ai_correct:
  forall s p' s', steps (g_start put_demapped_gray) s (g_edges put_demapped_gray) p' s' -> put_demapped_gray_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem put_demapped_gray_pot_correct:
  forall s p' s',
    steps (g_start put_demapped_gray) s (g_edges put_demapped_gray) p' s' ->
    (put_demapped_gray_pot (g_start put_demapped_gray) s >= put_demapped_gray_pot p' s')%Q.
Proof.
  check_lp put_demapped_gray_ai_correct put_demapped_gray_hints.
Qed.

