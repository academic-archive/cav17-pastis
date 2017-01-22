Require Import pasta.Pasta.

Notation IDexpand_bottom_edge_z := 1%positive.
Notation IDexpand_bottom_edge__tmp := 2%positive.
Notation IDexpand_bottom_edge__tmp1 := 3%positive.
Notation IDexpand_bottom_edge__tmp2 := 4%positive.
Notation IDexpand_bottom_edge_row := 5%positive.
Notation IDexpand_bottom_edge_image_data := 6%positive.
Notation IDexpand_bottom_edge_input_rows := 7%positive.
Notation IDexpand_bottom_edge_num_cols := 8%positive.
Notation IDexpand_bottom_edge_output_rows := 9%positive.
Definition expand_bottom_edge : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDexpand_bottom_edge_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDexpand_bottom_edge__tmp2
             (Some (EVar IDexpand_bottom_edge_num_cols))),3%positive)::
             (3%positive,(AAssign IDexpand_bottom_edge__tmp1
             (Some (EVar IDexpand_bottom_edge_input_rows))),4%positive)::
             (4%positive,(AAssign IDexpand_bottom_edge__tmp
             (Some (EVar IDexpand_bottom_edge_output_rows))),5%positive)::
             (5%positive,(AAssign IDexpand_bottom_edge_row
             (Some (EVar IDexpand_bottom_edge__tmp1))),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_bottom_edge_row) s) <
             (eval (EVar IDexpand_bottom_edge__tmp) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_bottom_edge_row) s) >=
             (eval (EVar IDexpand_bottom_edge__tmp) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDexpand_bottom_edge_row
             (Some (EAdd (EVar IDexpand_bottom_edge_row) (ENum (1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDexpand_bottom_edge_z
             (Some (EAdd (ENum (1)) (EVar IDexpand_bottom_edge_z)))),
             17%positive)::(17%positive,AWeaken,8%positive)::nil
|}.

Definition expand_bottom_edge_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 3%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ 1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 4%positive => (1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 5%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ 1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 6%positive => (1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 7%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ 1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 8%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 9%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ 1 * (s IDexpand_bottom_edge__tmp)+ -1 * (s IDexpand_bottom_edge_row) <= 0)%Z
    | 10%positive => (1 * (s IDexpand_bottom_edge__tmp)+ -1 * (s IDexpand_bottom_edge_row) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 11%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 13%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) <= 0)%Z
    | 15%positive => (-1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) <= 0)%Z
    | 16%positive => (-1 * (s IDexpand_bottom_edge_z) <= 0 /\ -1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) <= 0)%Z
    | 17%positive => (-1 * (s IDexpand_bottom_edge__tmp)+ 1 * (s IDexpand_bottom_edge_row) <= 0 /\ -1 * (s IDexpand_bottom_edge_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition expand_bottom_edge_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-(s IDexpand_bottom_edge_input_rows)
                          + (s IDexpand_bottom_edge_output_rows)))%Q
    | 2%positive => ((s IDexpand_bottom_edge_z)
                     + max0(-(s IDexpand_bottom_edge_input_rows)
                            + (s IDexpand_bottom_edge_output_rows)))%Q
    | 3%positive => ((s IDexpand_bottom_edge_z)
                     + max0(-(s IDexpand_bottom_edge_input_rows)
                            + (s IDexpand_bottom_edge_output_rows)))%Q
    | 4%positive => ((s IDexpand_bottom_edge_z)
                     + max0(-(s IDexpand_bottom_edge__tmp1)
                            + (s IDexpand_bottom_edge_output_rows)))%Q
    | 5%positive => ((s IDexpand_bottom_edge_z)
                     + max0((s IDexpand_bottom_edge__tmp)
                            - (s IDexpand_bottom_edge__tmp1)))%Q
    | 6%positive => ((s IDexpand_bottom_edge_z)
                     + max0((s IDexpand_bottom_edge__tmp)
                            - (s IDexpand_bottom_edge_row)))%Q
    | 7%positive => ((s IDexpand_bottom_edge_z)
                     + max0((s IDexpand_bottom_edge__tmp)
                            - (s IDexpand_bottom_edge_row)))%Q
    | 8%positive => ((s IDexpand_bottom_edge_z)
                     + max0((s IDexpand_bottom_edge__tmp)
                            - (s IDexpand_bottom_edge_row)))%Q
    | 9%positive => ((s IDexpand_bottom_edge_z)
                     + max0((s IDexpand_bottom_edge__tmp)
                            - (s IDexpand_bottom_edge_row)))%Q
    | 10%positive => ((s IDexpand_bottom_edge_z))%Q
    | 11%positive => ((s IDexpand_bottom_edge_z)
                      + max0((s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 12%positive => ((1 # 1) + (s IDexpand_bottom_edge_z)
                      + max0(-1 + (s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 13%positive => ((1 # 1) + (s IDexpand_bottom_edge_z)
                      + max0(-1 + (s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 14%positive => ((1 # 1) + (s IDexpand_bottom_edge_z)
                      + max0((s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 15%positive => ((1 # 1) + (s IDexpand_bottom_edge_z)
                      + max0((s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 16%positive => ((1 # 1) + (s IDexpand_bottom_edge_z)
                      + max0((s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | 17%positive => ((s IDexpand_bottom_edge_z)
                      + max0((s IDexpand_bottom_edge__tmp)
                             - (s IDexpand_bottom_edge_row)))%Q
    | _ => (0 # 1)%Q
  end.

Definition expand_bottom_edge_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDexpand_bottom_edge__tmp)
                                                            - (s IDexpand_bottom_edge_row)) (-1
                                                                    + (s IDexpand_bottom_edge__tmp)
                                                                    - (s IDexpand_bottom_edge_row)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDexpand_bottom_edge__tmp)
                                                                - (s IDexpand_bottom_edge_row))) (F_check_ge (0) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDexpand_bottom_edge__tmp)
                                                     - (s IDexpand_bottom_edge_row)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem expand_bottom_edge_ai_correct:
  forall s p' s', steps (g_start expand_bottom_edge) s (g_edges expand_bottom_edge) p' s' -> expand_bottom_edge_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem expand_bottom_edge_pot_correct:
  forall s p' s',
    steps (g_start expand_bottom_edge) s (g_edges expand_bottom_edge) p' s' ->
    (expand_bottom_edge_pot (g_start expand_bottom_edge) s >= expand_bottom_edge_pot p' s')%Q.
Proof.
  check_lp expand_bottom_edge_ai_correct expand_bottom_edge_hints.
Qed.

