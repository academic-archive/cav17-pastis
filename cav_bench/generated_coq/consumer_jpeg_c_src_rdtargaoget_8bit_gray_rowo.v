Require Import pasta.Pasta.

Notation IDget_8bit_gray_row_z := 1%positive.
Notation IDget_8bit_gray_row_cinfo_dref_off40 := 2%positive.
Notation IDget_8bit_gray_row_col := 3%positive.
Notation IDget_8bit_gray_row_cinfo := 4%positive.
Notation IDget_8bit_gray_row_sinfo := 5%positive.
Definition get_8bit_gray_row : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDget_8bit_gray_row_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDget_8bit_gray_row_col) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDget_8bit_gray_row_col
             (Some (EVar IDget_8bit_gray_row_cinfo_dref_off40))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDget_8bit_gray_row_col) s) >
             (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDget_8bit_gray_row_col) s) <=
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDget_8bit_gray_row_col
             (Some (EAdd (EVar IDget_8bit_gray_row_col) (ENum (-1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDget_8bit_gray_row_z
             (Some (EAdd (ENum (1)) (EVar IDget_8bit_gray_row_z)))),
             16%positive)::(16%positive,AWeaken,7%positive)::nil
|}.

Definition get_8bit_gray_row_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 3%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ 1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_col) <= 0)%Z
    | 4%positive => (-1 * (s IDget_8bit_gray_row_col) <= 0 /\ 1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 5%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ 1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 6%positive => (1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 7%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 8%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ 1 * (s IDget_8bit_gray_row_col) <= 0)%Z
    | 9%positive => (1 * (s IDget_8bit_gray_row_col) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 10%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_col) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDget_8bit_gray_row_col) + 1 <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 12%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_col) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_col) <= 0)%Z
    | 14%positive => (-1 * (s IDget_8bit_gray_row_col) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) <= 0)%Z
    | 15%positive => (-1 * (s IDget_8bit_gray_row_z) <= 0 /\ -1 * (s IDget_8bit_gray_row_col) <= 0)%Z
    | 16%positive => (-1 * (s IDget_8bit_gray_row_col) <= 0 /\ -1 * (s IDget_8bit_gray_row_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition get_8bit_gray_row_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDget_8bit_gray_row_cinfo_dref_off40)))%Q
    | 2%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_cinfo_dref_off40)))%Q
    | 3%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_cinfo_dref_off40)))%Q
    | 4%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_cinfo_dref_off40)))%Q
    | 5%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_col)))%Q
    | 6%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_col)))%Q
    | 7%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_col)))%Q
    | 8%positive => ((s IDget_8bit_gray_row_z)
                     + max0((s IDget_8bit_gray_row_col)))%Q
    | 9%positive => ((s IDget_8bit_gray_row_z))%Q
    | 10%positive => ((s IDget_8bit_gray_row_z)
                      + max0((s IDget_8bit_gray_row_col)))%Q
    | 11%positive => ((1 # 1) + (s IDget_8bit_gray_row_z)
                      + max0(-1 + (s IDget_8bit_gray_row_col)))%Q
    | 12%positive => ((1 # 1) + (s IDget_8bit_gray_row_z)
                      + max0(-1 + (s IDget_8bit_gray_row_col)))%Q
    | 13%positive => ((1 # 1) + (s IDget_8bit_gray_row_z)
                      + max0((s IDget_8bit_gray_row_col)))%Q
    | 14%positive => ((1 # 1) + (s IDget_8bit_gray_row_z)
                      + max0((s IDget_8bit_gray_row_col)))%Q
    | 15%positive => ((1 # 1) + (s IDget_8bit_gray_row_z)
                      + max0((s IDget_8bit_gray_row_col)))%Q
    | 16%positive => ((s IDget_8bit_gray_row_z)
                      + max0((s IDget_8bit_gray_row_col)))%Q
    | _ => (0 # 1)%Q
  end.

Definition get_8bit_gray_row_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDget_8bit_gray_row_col)) (-1
                                                                    + (s IDget_8bit_gray_row_col)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDget_8bit_gray_row_col))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement ((s IDget_8bit_gray_row_col)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem get_8bit_gray_row_ai_correct:
  forall s p' s', steps (g_start get_8bit_gray_row) s (g_edges get_8bit_gray_row) p' s' -> get_8bit_gray_row_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem get_8bit_gray_row_pot_correct:
  forall s p' s',
    steps (g_start get_8bit_gray_row) s (g_edges get_8bit_gray_row) p' s' ->
    (get_8bit_gray_row_pot (g_start get_8bit_gray_row) s >= get_8bit_gray_row_pot p' s')%Q.
Proof.
  check_lp get_8bit_gray_row_ai_correct get_8bit_gray_row_hints.
Qed.

