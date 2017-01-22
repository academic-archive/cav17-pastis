Require Import pasta.Pasta.

Notation IDexpand_right_edge_z := 1%positive.
Notation IDexpand_right_edge__tmp := 2%positive.
Notation IDexpand_right_edge__tmp1 := 3%positive.
Notation IDexpand_right_edge__tmp2 := 4%positive.
Notation IDexpand_right_edge_count := 5%positive.
Notation IDexpand_right_edge_numcols := 6%positive.
Notation IDexpand_right_edge_pixval := 7%positive.
Notation IDexpand_right_edge_row := 8%positive.
Notation IDexpand_right_edge_image_data := 9%positive.
Notation IDexpand_right_edge_input_cols := 10%positive.
Notation IDexpand_right_edge_num_rows := 11%positive.
Notation IDexpand_right_edge_output_cols := 12%positive.
Definition expand_right_edge : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDexpand_right_edge_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDexpand_right_edge__tmp
             (Some (EVar IDexpand_right_edge_num_rows))),3%positive)::
             (3%positive,(AAssign IDexpand_right_edge__tmp1
             (Some (EVar IDexpand_right_edge_input_cols))),4%positive)::
             (4%positive,(AAssign IDexpand_right_edge__tmp2
             (Some (EVar IDexpand_right_edge_output_cols))),5%positive)::
             (5%positive,(AAssign IDexpand_right_edge_numcols
             (Some (ESub (EVar IDexpand_right_edge__tmp2)
             (EVar IDexpand_right_edge__tmp1)))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_numcols) s) >
             (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_numcols) s) <=
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,17%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDexpand_right_edge_row
             (Some (ENum (0)))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_row) s) <
             (eval (EVar IDexpand_right_edge__tmp) s))%Z)),18%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_row) s) >=
             (eval (EVar IDexpand_right_edge__tmp) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDexpand_right_edge_pixval None),
             20%positive)::
             (20%positive,(AAssign IDexpand_right_edge_count
             (Some (EVar IDexpand_right_edge_numcols))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_count) s) >
             (eval (ENum (0)) s))%Z)),31%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDexpand_right_edge_count) s) <=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDexpand_right_edge_row
             (Some (EAdd (EVar IDexpand_right_edge_row) (ENum (1))))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDexpand_right_edge_z
             (Some (EAdd (ENum (1)) (EVar IDexpand_right_edge_z)))),
             30%positive)::(30%positive,AWeaken,13%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDexpand_right_edge_count
             (Some (EAdd (EVar IDexpand_right_edge_count) (ENum (-1))))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDexpand_right_edge_z
             (Some (EAdd (ENum (1)) (EVar IDexpand_right_edge_z)))),
             37%positive)::(37%positive,AWeaken,23%positive)::nil
|}.

Definition expand_right_edge_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 3%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 4%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 5%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 6%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 7%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 8%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_numcols) <= 0)%Z
    | 9%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 11%positive => (1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ 1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 12%positive => (-1 * (s IDexpand_right_edge_row) <= 0 /\ 1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 13%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge__tmp)+ -1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 15%positive => (1 * (s IDexpand_right_edge__tmp)+ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge__tmp)+ -1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 17%positive => (-1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 18%positive => (-1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_count) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDexpand_right_edge_count) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 24%positive => (-1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 25%positive => (1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 26%positive => (-1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 27%positive => (1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 28%positive => (-1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ 1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 29%positive => (1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 30%positive => (-1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ 1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_count) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDexpand_right_edge_count) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0)%Z
    | 33%positive => (-1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge_count) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 35%positive => (-1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) <= 0)%Z
    | 36%positive => (-1 * (s IDexpand_right_edge_z) <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_count) <= 0)%Z
    | 37%positive => (-1 * (s IDexpand_right_edge_count) <= 0 /\ -1 * (s IDexpand_right_edge_row) <= 0 /\ -1 * (s IDexpand_right_edge_numcols) + 1 <= 0 /\ -1 * (s IDexpand_right_edge__tmp)+ 1 * (s IDexpand_right_edge_row) + 1 <= 0 /\ -1 * (s IDexpand_right_edge_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition expand_right_edge_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 - (s IDexpand_right_edge_input_cols)
                          + (s IDexpand_right_edge_output_cols)) * max0((s IDexpand_right_edge_num_rows))
                     + (2 # 1) * max0((s IDexpand_right_edge_num_rows)))%Q
    | 2%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 - (s IDexpand_right_edge_input_cols)
                            + (s IDexpand_right_edge_output_cols)) * max0((s IDexpand_right_edge_num_rows))
                     + (2 # 1) * max0((s IDexpand_right_edge_num_rows)))%Q
    | 3%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 - (s IDexpand_right_edge_input_cols)
                            + (s IDexpand_right_edge_output_cols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 4%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 - (s IDexpand_right_edge__tmp1)
                            + (s IDexpand_right_edge_output_cols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 5%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 - (s IDexpand_right_edge__tmp1)
                            + (s IDexpand_right_edge__tmp2)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 6%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 7%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 8%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 9%positive => ((s IDexpand_right_edge_z)
                     + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                     + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 10%positive => ((s IDexpand_right_edge_z)
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 1) * max0((s IDexpand_right_edge__tmp)))%Q
    | 11%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + (s IDexpand_right_edge_z)
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row)))%Q
    | 12%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + (s IDexpand_right_edge_z)
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row)))%Q
    | 13%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 14%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 15%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 16%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 17%positive => ((s IDexpand_right_edge_z))%Q
    | 18%positive => (-(1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge__tmp))
                      + (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge__tmp)
                             - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 19%positive => ((6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 20%positive => ((6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + max0((s IDexpand_right_edge_z)))%Q
    | 21%positive => ((1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + max0((s IDexpand_right_edge_z)))%Q
    | 22%positive => ((1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + max0((s IDexpand_right_edge_z)))%Q
    | 23%positive => ((13 # 10)
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z)))%Q
    | 24%positive => ((13 # 10)
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z)))%Q
    | 25%positive => ((13 # 10) - (21 # 20) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (29 # 20) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z)))%Q
    | 26%positive => ((13 # 10) - (21 # 20) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (29 # 20) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z)))%Q
    | 27%positive => ((9 # 10) - (1 # 4) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0(-1
                                                               + (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge_row))
                      + (29 # 20) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (11 # 10) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + max0((s IDexpand_right_edge_z)))%Q
    | 28%positive => ((9 # 10) - (1 # 4) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0(-1
                                                               + (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge_row))
                      + (29 # 20) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (11 # 10) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + max0((s IDexpand_right_edge_z)))%Q
    | 29%positive => ((9 # 10) - (1 # 4) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0(-1
                                                               + (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge_row))
                      + (29 # 20) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (11 # 10) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + max0((s IDexpand_right_edge_z)))%Q
    | 30%positive => ((9 # 10) - (1 # 4) * (s IDexpand_right_edge__tmp)
                      - (4 # 5) * (s IDexpand_right_edge__tmp) * (s IDexpand_right_edge_row)
                      - (1 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (7 # 20) * (s IDexpand_right_edge__tmp)^2
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0(-1
                                                               + (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp))
                      - (1 # 10) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (11 # 10) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      + (2 # 5) * (s IDexpand_right_edge_row)^2
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (3 # 10) * max0(-1 + (s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge_row))
                      + max0(-1 + (s IDexpand_right_edge_z))
                      + (19 # 20) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 5) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      + (9 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (11 # 10) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2)%Q
    | 31%positive => ((13 # 10)
                      + (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-
                                                                    (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (4 # 5) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-(s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0((s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z)))%Q
    | 32%positive => ((13 # 10)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge_z))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (9 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge__tmp))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (17 # 10) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * max0((s IDexpand_right_edge_z))^2)%Q
    | 33%positive => ((13 # 10)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge_z))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge_count))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (9 # 10) * max0(-1 + (s IDexpand_right_edge_count)) * max0((s IDexpand_right_edge__tmp))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (17 # 10) * max0((s IDexpand_right_edge__tmp))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      + (1 # 2) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * max0((s IDexpand_right_edge_z))^2)%Q
    | 34%positive => ((3 # 2)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (4 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (17 # 10) * max0((s IDexpand_right_edge__tmp))
                      + (9 # 10) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (3 # 5) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      - max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * max0((s IDexpand_right_edge_z))^2)%Q
    | 35%positive => ((3 # 2)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (4 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (17 # 10) * max0((s IDexpand_right_edge__tmp))
                      + (9 # 10) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (3 # 5) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      - max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * max0((s IDexpand_right_edge_z))^2)%Q
    | 36%positive => ((3 # 2)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge_z))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_z))
                      + (4 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (17 # 10) * max0((s IDexpand_right_edge__tmp))
                      + (9 # 10) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (3 # 5) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      - max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_z))
                      - (1 # 2) * max0((s IDexpand_right_edge_z))^2)%Q
    | 37%positive => ((3 # 2)
                      - (1 # 2) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge__tmp) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 20) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp))
                      + (6 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (2 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_count))
                      + (3 # 5) * (s IDexpand_right_edge__tmp) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 5) * (s IDexpand_right_edge_count)
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 10) * (s IDexpand_right_edge_count) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      - (1 # 10) * (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_numcols))
                      - (s IDexpand_right_edge_count) * max0((s IDexpand_right_edge_row))
                      - (1 # 5) * (s IDexpand_right_edge_numcols)
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp))
                      + (1 # 2) * (s IDexpand_right_edge_numcols) * max0(-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_count))
                      - (s IDexpand_right_edge_numcols) * max0((s IDexpand_right_edge_row))
                      + (2 # 5) * (s IDexpand_right_edge_row)
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_row) * max0(-1
                                                                    + 
                                                                    (s IDexpand_right_edge_z))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_count))
                      + (1 # 2) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_numcols))
                      - (2 # 5) * (s IDexpand_right_edge_row) * max0((s IDexpand_right_edge_row))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp))
                      - (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge__tmp)
                                                                   - 
                                                                   (s IDexpand_right_edge_row))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0(-1
                                                                   + 
                                                                   (s IDexpand_right_edge_z))
                      + (1 # 2) * (s IDexpand_right_edge_z) * max0((s IDexpand_right_edge__tmp))
                      + (3 # 5) * max0(-1 + (s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_numcols))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0(-1
                                                                    + (s IDexpand_right_edge_z))
                      - max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      + (2 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_row))
                      + (13 # 10) * max0(-1 + (s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (1 # 5) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 2) * max0(-1 + (s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row))^2
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))
                      + max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols)) * max0((s IDexpand_right_edge_count))
                      - (1 # 10) * max0(-1 + (s IDexpand_right_edge_numcols))^2
                      - (1 # 2) * max0(-1 + (s IDexpand_right_edge_z))^2
                      - (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))
                      + (1 # 5) * max0(-(s IDexpand_right_edge__tmp)
                                       + (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (6 # 5) * max0((s IDexpand_right_edge__tmp))
                      + (9 # 10) * max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_count))
                      - max0((s IDexpand_right_edge__tmp)) * max0((s IDexpand_right_edge_numcols))
                      - (1 # 20) * max0((s IDexpand_right_edge__tmp))^2
                      - (13 # 10) * max0((s IDexpand_right_edge__tmp)
                                         - (s IDexpand_right_edge_row))
                      - (2 # 5) * max0((s IDexpand_right_edge__tmp)
                                       - (s IDexpand_right_edge_row)) * max0((s IDexpand_right_edge_row))
                      + (1 # 2) * max0((s IDexpand_right_edge_count))
                      - (3 # 5) * max0((s IDexpand_right_edge_numcols))
                      + (1 # 10) * max0((s IDexpand_right_edge_numcols))^2
                      - max0((s IDexpand_right_edge_row)))%Q
    | _ => (0 # 1)%Q
  end.

Definition expand_right_edge_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-2 0*) F_max0_ge_0 ((s IDexpand_right_edge__tmp));
                     (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_z)));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (-(s IDexpand_right_edge_z))) (F_check_ge (0) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_z))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-2 0*) F_max0_monotonic (F_check_ge ((s IDexpand_right_edge__tmp)
                                                             - (s IDexpand_right_edge_row)) (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)));
                      (*-2 0*) F_max0_ge_0 (-1 + (s IDexpand_right_edge__tmp)
                                            - (s IDexpand_right_edge_row));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0));
                      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))]
    | 17%positive => []
    | 18%positive => [(*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1.3 0*) F_max0_pre_decrement ((s IDexpand_right_edge__tmp)
                                                       - (s IDexpand_right_edge_row)) (1);
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)))]
    | 23%positive => []
    | 24%positive => [(*0 0.05*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)));
                      (*-0.4 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0));
                      (*-0.8 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.7*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*0 1.2*) F_max0_monotonic (F_check_ge ((s IDexpand_right_edge_count)) (-1
                                                                    + (s IDexpand_right_edge_count)));
                      (*-1.2 0*) F_max0_ge_0 (-1
                                              + (s IDexpand_right_edge_count));
                      (*-0.05 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0));
                      (*-0.1 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)));
                      (*-0.4 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)));
                      (*-0.1 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0));
                      (*-0.05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.6 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (-
                                                                    (s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (-
                                                                    (s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_count))) (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge__tmp)
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDexpand_right_edge_count))) (F_check_ge (0) (0));
                      (*-0.8 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_row)) (0))]
    | 31%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.9 -4.51988e-12*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_count))) (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_count))) (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_z))) (F_check_ge ((s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-0.3 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDexpand_right_edge__tmp)
                                                                    - 
                                                                    (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0));
                      (*-0.3 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)));
                      (*-0.3 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)));
                      (*-0.3 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_z)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (-1
                                                                    + (s IDexpand_right_edge_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp))) (F_check_ge (0) (0)));
                      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_z))) (F_check_ge (0) (0)));
                      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_count)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_count))) (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_count))) (F_check_ge ((s IDexpand_right_edge_count)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_numcols))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge__tmp)
                                                                    - (s IDexpand_right_edge_row))) (F_check_ge (0) (0)));
                      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge_numcols))) (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDexpand_right_edge_count))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_row)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_row)));
                      (*-0.6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDexpand_right_edge_numcols)) (0))) (F_max0_ge_0 ((s IDexpand_right_edge_numcols)));
                      (*-0.4 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDexpand_right_edge__tmp))) (F_check_ge ((s IDexpand_right_edge__tmp)) (0))]
    | _ => []
  end.


Theorem expand_right_edge_ai_correct:
  forall s p' s', steps (g_start expand_right_edge) s (g_edges expand_right_edge) p' s' -> expand_right_edge_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem expand_right_edge_pot_correct:
  forall s p' s',
    steps (g_start expand_right_edge) s (g_edges expand_right_edge) p' s' ->
    (expand_right_edge_pot (g_start expand_right_edge) s >= expand_right_edge_pot p' s')%Q.
Proof.
  check_lp expand_right_edge_ai_correct expand_right_edge_hints.
Qed.

