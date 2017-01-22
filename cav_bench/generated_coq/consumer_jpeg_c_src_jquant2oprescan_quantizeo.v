Require Import pasta.Pasta.

Notation IDprescan_quantize_z := 1%positive.
Notation IDprescan_quantize__tmp := 2%positive.
Notation IDprescan_quantize_cinfo_dref_off128 := 3%positive.
Notation IDprescan_quantize_col := 4%positive.
Notation IDprescan_quantize_row := 5%positive.
Notation IDprescan_quantize_width := 6%positive.
Notation IDprescan_quantize_cinfo := 7%positive.
Notation IDprescan_quantize_input_buf := 8%positive.
Notation IDprescan_quantize_num_rows := 9%positive.
Notation IDprescan_quantize_output_buf := 10%positive.
Definition prescan_quantize : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDprescan_quantize_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDprescan_quantize_col) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDprescan_quantize__tmp
             (Some (EVar IDprescan_quantize_num_rows))),5%positive)::
             (5%positive,(AAssign IDprescan_quantize_width
             (Some (EVar IDprescan_quantize_cinfo_dref_off128))),6%positive)::
             (6%positive,(AAssign IDprescan_quantize_row (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDprescan_quantize_row) s) <
             (eval (EVar IDprescan_quantize__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDprescan_quantize_row) s) >=
             (eval (EVar IDprescan_quantize__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDprescan_quantize_col
             (Some (EVar IDprescan_quantize_width))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDprescan_quantize_col) s) >
             (eval (ENum (0)) s))%Z)),24%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDprescan_quantize_col) s) <=
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDprescan_quantize_row
             (Some (EAdd (EVar IDprescan_quantize_row) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDprescan_quantize_z
             (Some (EAdd (ENum (1)) (EVar IDprescan_quantize_z)))),
             23%positive)::(23%positive,AWeaken,9%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (25%positive,ANone,27%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDprescan_quantize_col
             (Some (EAdd (EVar IDprescan_quantize_col) (ENum (-1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDprescan_quantize_z
             (Some (EAdd (ENum (1)) (EVar IDprescan_quantize_z)))),
             32%positive)::(32%positive,AWeaken,16%positive)::nil
|}.

Definition prescan_quantize_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0)%Z
    | 3%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0)%Z
    | 4%positive => (-1 * (s IDprescan_quantize_col) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0)%Z
    | 5%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0)%Z
    | 6%positive => (-1 * (s IDprescan_quantize_col) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0)%Z
    | 7%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0 /\ 1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 8%positive => (-1 * (s IDprescan_quantize_row) <= 0 /\ 1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0 /\ 1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0)%Z
    | 9%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 10%positive => (-1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize__tmp)+ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 11%positive => (1 * (s IDprescan_quantize__tmp)+ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 12%positive => (-1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 14%positive => (-1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0)%Z
    | 16%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_col) <= 0)%Z
    | 18%positive => (1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_col) <= 0)%Z
    | 20%positive => (1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ 1 * (s IDprescan_quantize_col) <= 0)%Z
    | 22%positive => (1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) <= 0 /\ 1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDprescan_quantize_col) + 1 <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDprescan_quantize_col) + 1 <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_col) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0)%Z
    | 30%positive => (-1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) <= 0)%Z
    | 31%positive => (-1 * (s IDprescan_quantize_z) <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_col) <= 0)%Z
    | 32%positive => (-1 * (s IDprescan_quantize_col) <= 0 /\ -1 * (s IDprescan_quantize__tmp)+ 1 * (s IDprescan_quantize_row) + 1 <= 0 /\ -1 * (s IDprescan_quantize_row) <= 0 /\ -1 * (s IDprescan_quantize_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition prescan_quantize_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDprescan_quantize_cinfo_dref_off128)) * max0((s IDprescan_quantize_num_rows))
                     + max0((s IDprescan_quantize_num_rows)))%Q
    | 2%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize_cinfo_dref_off128)) * max0((s IDprescan_quantize_num_rows))
                     + max0((s IDprescan_quantize_num_rows)))%Q
    | 3%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize_cinfo_dref_off128)) * max0((s IDprescan_quantize_num_rows))
                     + max0((s IDprescan_quantize_num_rows)))%Q
    | 4%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize_cinfo_dref_off128)) * max0((s IDprescan_quantize_num_rows))
                     + max0((s IDprescan_quantize_num_rows)))%Q
    | 5%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize__tmp))
                     + max0((s IDprescan_quantize__tmp)) * max0((s IDprescan_quantize_cinfo_dref_off128)))%Q
    | 6%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize__tmp))
                     + max0((s IDprescan_quantize__tmp)) * max0((s IDprescan_quantize_width)))%Q
    | 7%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row))
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 8%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row))
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 9%positive => ((s IDprescan_quantize_z)
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row))
                     + max0((s IDprescan_quantize__tmp)
                            - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 10%positive => ((s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 11%positive => ((s IDprescan_quantize_z))%Q
    | 12%positive => ((s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 13%positive => ((s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 14%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 15%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 16%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 17%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize_col)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize_col)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize_col)))%Q
    | 23%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_z)
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      - max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize_col)))%Q
    | 24%positive => ((s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_col))
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_row) * max0((s IDprescan_quantize_col))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_col))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 25%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 26%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 27%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 28%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 29%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 30%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 31%positive => (-(s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | 32%positive => (-(1 # 1)
                      - (s IDprescan_quantize__tmp) * max0((s IDprescan_quantize_width))
                      - (s IDprescan_quantize_col) * max0(-1
                                                          + (s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_col) * max0((s IDprescan_quantize__tmp))
                      + (s IDprescan_quantize_row) * max0((s IDprescan_quantize_width))
                      + (s IDprescan_quantize_z)
                      - max0(-1 + (s IDprescan_quantize__tmp))
                      + max0(-1 + (s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width))
                      + max0((s IDprescan_quantize__tmp))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row))
                      + max0((s IDprescan_quantize__tmp)
                             - (s IDprescan_quantize_row)) * max0((s IDprescan_quantize_width)))%Q
    | _ => (0 # 1)%Q
  end.

Definition prescan_quantize_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDprescan_quantize__tmp)
                                                             - (s IDprescan_quantize_row)) (-1
                                                                    + (s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDprescan_quantize__tmp)
                                            - (s IDprescan_quantize_row));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_width))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement ((s IDprescan_quantize__tmp)
                                                    - (s IDprescan_quantize_row)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row))) (F_check_ge ((s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_width))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)) (0))) (F_max0_ge_0 ((s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0))]
    | 24%positive => [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDprescan_quantize__tmp))) (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDprescan_quantize__tmp)) (0))) (F_max0_ge_0 ((s IDprescan_quantize__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDprescan_quantize_col)) (0))) (F_max0_ge_0 ((s IDprescan_quantize_col)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize_col))) (F_check_ge ((s IDprescan_quantize_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize__tmp))) (F_check_ge (0) (0)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDprescan_quantize__tmp))) (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDprescan_quantize__tmp)) (0))) (F_max0_ge_0 ((s IDprescan_quantize__tmp)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row))) (F_check_ge (-1
                                                                    + (s IDprescan_quantize__tmp)
                                                                    - (s IDprescan_quantize_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize__tmp))) (F_check_ge ((s IDprescan_quantize__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize__tmp))) (F_check_ge ((s IDprescan_quantize__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize__tmp))) (F_check_ge ((s IDprescan_quantize__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDprescan_quantize_col)) (0))) (F_max0_ge_0 ((s IDprescan_quantize_col)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize_col))) (F_check_ge ((s IDprescan_quantize_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDprescan_quantize__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDprescan_quantize__tmp))) (F_check_ge ((s IDprescan_quantize__tmp)) (0))]
    | _ => []
  end.


Theorem prescan_quantize_ai_correct:
  forall s p' s', steps (g_start prescan_quantize) s (g_edges prescan_quantize) p' s' -> prescan_quantize_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem prescan_quantize_pot_correct:
  forall s p' s',
    steps (g_start prescan_quantize) s (g_edges prescan_quantize) p' s' ->
    (prescan_quantize_pot (g_start prescan_quantize) s >= prescan_quantize_pot p' s')%Q.
Proof.
  check_lp prescan_quantize_ai_correct prescan_quantize_hints.
Qed.

