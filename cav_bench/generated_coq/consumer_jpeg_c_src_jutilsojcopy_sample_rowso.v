Require Import pasta.Pasta.

Notation IDjcopy_sample_rows_z := 1%positive.
Notation IDjcopy_sample_rows__tmp := 2%positive.
Notation IDjcopy_sample_rows__tmp1 := 3%positive.
Notation IDjcopy_sample_rows__tmp2 := 4%positive.
Notation IDjcopy_sample_rows__tmp3 := 5%positive.
Notation IDjcopy_sample_rows_count := 6%positive.
Notation IDjcopy_sample_rows_row := 7%positive.
Notation IDjcopy_sample_rows_dest_row := 8%positive.
Notation IDjcopy_sample_rows_input_array := 9%positive.
Notation IDjcopy_sample_rows_num_cols := 10%positive.
Notation IDjcopy_sample_rows_num_rows := 11%positive.
Notation IDjcopy_sample_rows_output_array := 12%positive.
Notation IDjcopy_sample_rows_source_row := 13%positive.
Definition jcopy_sample_rows : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDjcopy_sample_rows_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjcopy_sample_rows__tmp3
             (Some (EVar IDjcopy_sample_rows_source_row))),3%positive)::
             (3%positive,(AAssign IDjcopy_sample_rows__tmp2
             (Some (EVar IDjcopy_sample_rows_dest_row))),4%positive)::
             (4%positive,(AAssign IDjcopy_sample_rows__tmp
             (Some (EVar IDjcopy_sample_rows_num_rows))),5%positive)::
             (5%positive,(AAssign IDjcopy_sample_rows__tmp1
             (Some (EVar IDjcopy_sample_rows_num_cols))),6%positive)::
             (6%positive,(AAssign IDjcopy_sample_rows_count
             (Some (EMul (EVar IDjcopy_sample_rows__tmp1) (ENum (1))))),
             7%positive)::
             (7%positive,(AAssign IDjcopy_sample_rows_row
             (Some (EVar IDjcopy_sample_rows__tmp))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjcopy_sample_rows_row) s) >
             (eval (ENum (0)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjcopy_sample_rows_row) s) <=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDjcopy_sample_rows_row
             (Some (EAdd (EVar IDjcopy_sample_rows_row) (ENum (-1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDjcopy_sample_rows_z
             (Some (EAdd (ENum (1)) (EVar IDjcopy_sample_rows_z)))),
             19%positive)::(19%positive,AWeaken,10%positive)::nil
|}.

Definition jcopy_sample_rows_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ 1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 4%positive => (1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ 1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 6%positive => (1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 7%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ 1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 8%positive => (1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 9%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ 1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ 1 * (s IDjcopy_sample_rows_row) <= 0)%Z
    | 12%positive => (1 * (s IDjcopy_sample_rows_row) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 13%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_row) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDjcopy_sample_rows_row) + 1 <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 15%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_row) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_row) <= 0)%Z
    | 17%positive => (-1 * (s IDjcopy_sample_rows_row) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) <= 0)%Z
    | 18%positive => (-1 * (s IDjcopy_sample_rows_z) <= 0 /\ -1 * (s IDjcopy_sample_rows_row) <= 0)%Z
    | 19%positive => (-1 * (s IDjcopy_sample_rows_row) <= 0 /\ -1 * (s IDjcopy_sample_rows_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jcopy_sample_rows_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDjcopy_sample_rows_num_rows)))%Q
    | 2%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows_num_rows)))%Q
    | 3%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows_num_rows)))%Q
    | 4%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows_num_rows)))%Q
    | 5%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows__tmp)))%Q
    | 6%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows__tmp)))%Q
    | 7%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows__tmp)))%Q
    | 8%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows_row)))%Q
    | 9%positive => ((s IDjcopy_sample_rows_z)
                     + max0((s IDjcopy_sample_rows_row)))%Q
    | 10%positive => ((s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 11%positive => ((s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 12%positive => ((s IDjcopy_sample_rows_z))%Q
    | 13%positive => ((s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 14%positive => ((1 # 1) + (s IDjcopy_sample_rows_z)
                      + max0(-1 + (s IDjcopy_sample_rows_row)))%Q
    | 15%positive => ((1 # 1) + (s IDjcopy_sample_rows_z)
                      + max0(-1 + (s IDjcopy_sample_rows_row)))%Q
    | 16%positive => ((1 # 1) + (s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 17%positive => ((1 # 1) + (s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 18%positive => ((1 # 1) + (s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | 19%positive => ((s IDjcopy_sample_rows_z)
                      + max0((s IDjcopy_sample_rows_row)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jcopy_sample_rows_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDjcopy_sample_rows_row)) (-1
                                                                    + (s IDjcopy_sample_rows_row)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDjcopy_sample_rows_row))) (F_check_ge (0) (0))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement ((s IDjcopy_sample_rows_row)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem jcopy_sample_rows_ai_correct:
  forall s p' s', steps (g_start jcopy_sample_rows) s (g_edges jcopy_sample_rows) p' s' -> jcopy_sample_rows_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jcopy_sample_rows_pot_correct:
  forall s p' s',
    steps (g_start jcopy_sample_rows) s (g_edges jcopy_sample_rows) p' s' ->
    (jcopy_sample_rows_pot (g_start jcopy_sample_rows) s >= jcopy_sample_rows_pot p' s')%Q.
Proof.
  check_lp jcopy_sample_rows_ai_correct jcopy_sample_rows_hints.
Qed.

