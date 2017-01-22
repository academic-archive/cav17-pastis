Require Import pasta.Pasta.

Notation IDquantize3_ord_dither_z := 1%positive.
Notation IDquantize3_ord_dither__tmp := 2%positive.
Notation IDquantize3_ord_dither_cinfo_dref_off128 := 3%positive.
Notation IDquantize3_ord_dither_col := 4%positive.
Notation IDquantize3_ord_dither_col_index := 5%positive.
Notation IDquantize3_ord_dither_pixcode := 6%positive.
Notation IDquantize3_ord_dither_row := 7%positive.
Notation IDquantize3_ord_dither_row_index := 8%positive.
Notation IDquantize3_ord_dither_width := 9%positive.
Notation IDquantize3_ord_dither_cinfo := 10%positive.
Notation IDquantize3_ord_dither_input_buf := 11%positive.
Notation IDquantize3_ord_dither_num_rows := 12%positive.
Notation IDquantize3_ord_dither_output_buf := 13%positive.
Definition quantize3_ord_dither : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDquantize3_ord_dither_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDquantize3_ord_dither_col) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDquantize3_ord_dither__tmp
             (Some (EVar IDquantize3_ord_dither_num_rows))),5%positive)::
             (5%positive,(AAssign IDquantize3_ord_dither_width
             (Some (EVar IDquantize3_ord_dither_cinfo_dref_off128))),
             6%positive)::
             (6%positive,(AAssign IDquantize3_ord_dither_row
             (Some (ENum (0)))),7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDquantize3_ord_dither_row) s) <
             (eval (EVar IDquantize3_ord_dither__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDquantize3_ord_dither_row) s) >=
             (eval (EVar IDquantize3_ord_dither__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDquantize3_ord_dither_row_index None),
             14%positive)::
             (14%positive,(AAssign IDquantize3_ord_dither_col_index
             (Some (ENum (0)))),15%positive)::
             (15%positive,(AAssign IDquantize3_ord_dither_col
             (Some (EVar IDquantize3_ord_dither_width))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDquantize3_ord_dither_col) s) >
             (eval (ENum (0)) s))%Z)),27%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDquantize3_ord_dither_col) s) <=
             (eval (ENum (0)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDquantize3_ord_dither_row_index None),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDquantize3_ord_dither_row
             (Some (EAdd (EVar IDquantize3_ord_dither_row) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDquantize3_ord_dither_z
             (Some (EAdd (ENum (1)) (EVar IDquantize3_ord_dither_z)))),
             26%positive)::(26%positive,AWeaken,9%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDquantize3_ord_dither_pixcode None),
             29%positive)::
             (29%positive,(AAssign IDquantize3_ord_dither_pixcode None),
             30%positive)::
             (30%positive,(AAssign IDquantize3_ord_dither_pixcode None),
             31%positive)::
             (31%positive,(AAssign IDquantize3_ord_dither_col_index None),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDquantize3_ord_dither_col
             (Some (EAdd (EVar IDquantize3_ord_dither_col) (ENum (-1))))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDquantize3_ord_dither_z
             (Some (EAdd (ENum (1)) (EVar IDquantize3_ord_dither_z)))),
             37%positive)::(37%positive,AWeaken,18%positive)::nil
|}.

Definition quantize3_ord_dither_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 3%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 4%positive => (-1 * (s IDquantize3_ord_dither_col) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 5%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 6%positive => (-1 * (s IDquantize3_ord_dither_col) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 7%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0 /\ 1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 8%positive => (-1 * (s IDquantize3_ord_dither_row) <= 0 /\ 1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0 /\ 1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 9%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 10%positive => (-1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither__tmp)+ -1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 11%positive => (1 * (s IDquantize3_ord_dither__tmp)+ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 12%positive => (-1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 14%positive => (-1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ 1 * (s IDquantize3_ord_dither_col_index) <= 0 /\ -1 * (s IDquantize3_ord_dither_col_index) <= 0)%Z
    | 16%positive => (-1 * (s IDquantize3_ord_dither_col_index) <= 0 /\ 1 * (s IDquantize3_ord_dither_col_index) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ 1 * (s IDquantize3_ord_dither_col_index) <= 0 /\ -1 * (s IDquantize3_ord_dither_col_index) <= 0)%Z
    | 18%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 20%positive => (1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 22%positive => (1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 24%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 25%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) <= 0)%Z
    | 26%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ 1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDquantize3_ord_dither_col) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDquantize3_ord_dither_col) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDquantize3_ord_dither_col) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_col) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 35%positive => (-1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) <= 0)%Z
    | 36%positive => (-1 * (s IDquantize3_ord_dither_z) <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_col) <= 0)%Z
    | 37%positive => (-1 * (s IDquantize3_ord_dither_col) <= 0 /\ -1 * (s IDquantize3_ord_dither__tmp)+ 1 * (s IDquantize3_ord_dither_row) + 1 <= 0 /\ -1 * (s IDquantize3_ord_dither_row) <= 0 /\ -1 * (s IDquantize3_ord_dither_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition quantize3_ord_dither_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDquantize3_ord_dither_cinfo_dref_off128)) * max0((s IDquantize3_ord_dither_num_rows))
                     + max0((s IDquantize3_ord_dither_num_rows)))%Q
    | 2%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither_cinfo_dref_off128)) * max0((s IDquantize3_ord_dither_num_rows))
                     + max0((s IDquantize3_ord_dither_num_rows)))%Q
    | 3%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither_cinfo_dref_off128)) * max0((s IDquantize3_ord_dither_num_rows))
                     + max0((s IDquantize3_ord_dither_num_rows)))%Q
    | 4%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither_cinfo_dref_off128)) * max0((s IDquantize3_ord_dither_num_rows))
                     + max0((s IDquantize3_ord_dither_num_rows)))%Q
    | 5%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither__tmp))
                     + max0((s IDquantize3_ord_dither__tmp)) * max0((s IDquantize3_ord_dither_cinfo_dref_off128)))%Q
    | 6%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither__tmp))
                     + max0((s IDquantize3_ord_dither__tmp)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 7%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row))
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 8%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row))
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 9%positive => ((s IDquantize3_ord_dither_z)
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row))
                     + max0((s IDquantize3_ord_dither__tmp)
                            - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 10%positive => ((s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 11%positive => ((s IDquantize3_ord_dither_z))%Q
    | 12%positive => ((s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 13%positive => ((s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 14%positive => ((s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 15%positive => ((s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 16%positive => ((s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col)))%Q
    | 17%positive => ((s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col)))%Q
    | 18%positive => ((s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col)))%Q
    | 19%positive => ((s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width)))%Q
    | 23%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither_col)))%Q
    | 24%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither_col)))%Q
    | 25%positive => ((1 # 1)
                      + (s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither_col)))%Q
    | 26%positive => ((s IDquantize3_ord_dither__tmp) * max0((s IDquantize3_ord_dither_col))
                      - (s IDquantize3_ord_dither_row) * max0((s IDquantize3_ord_dither_col))
                      + (s IDquantize3_ord_dither_z)
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      - max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither_col)))%Q
    | 27%positive => ((s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_col)))%Q
    | 28%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 29%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 30%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 31%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 32%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 33%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + max0((s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)))%Q
    | 34%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + (2 # 1) * max0((s IDquantize3_ord_dither__tmp)
                                       - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither_row)))%Q
    | 35%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + (2 # 1) * max0((s IDquantize3_ord_dither__tmp)
                                       - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither_row)))%Q
    | 36%positive => (-(s IDquantize3_ord_dither_col) * max0(-1
                                                             + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + (2 # 1) * max0((s IDquantize3_ord_dither__tmp)
                                       - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither_row)))%Q
    | 37%positive => (-(1 # 1)
                      - (s IDquantize3_ord_dither_col) * max0(-1
                                                              + (s IDquantize3_ord_dither__tmp))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither__tmp)
                                                              - (s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_col) * max0((s IDquantize3_ord_dither_row))
                      + (s IDquantize3_ord_dither_z)
                      - max0(-1 + (s IDquantize3_ord_dither__tmp))
                      + max0(-1 + (s IDquantize3_ord_dither__tmp)
                             - (s IDquantize3_ord_dither_row)) * max0((s IDquantize3_ord_dither_width))
                      + (2 # 1) * max0((s IDquantize3_ord_dither__tmp)
                                       - (s IDquantize3_ord_dither_row))
                      + max0((s IDquantize3_ord_dither_row)))%Q
    | _ => (0 # 1)%Q
  end.

Definition quantize3_ord_dither_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                             - (s IDquantize3_ord_dither_row)) (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDquantize3_ord_dither__tmp)
                                            - (s IDquantize3_ord_dither_row));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_width))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*0 1*) F_max0_pre_decrement ((s IDquantize3_ord_dither__tmp)
                                                    - (s IDquantize3_ord_dither_row)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0))]
    | 27%positive => [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquantize3_ord_dither__tmp))) (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither_col)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither_col))) (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither_col))) (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_pre_decrement ((s IDquantize3_ord_dither__tmp)
                                                     - (s IDquantize3_ord_dither_row)) (1);
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDquantize3_ord_dither__tmp))) (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither__tmp)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquantize3_ord_dither__tmp)
                                                                    - 
                                                                    (s IDquantize3_ord_dither_row))) (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither__tmp))) (F_check_ge ((s IDquantize3_ord_dither__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither__tmp))) (F_check_ge ((s IDquantize3_ord_dither__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither_col)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_max0_ge_0 ((s IDquantize3_ord_dither_col)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_row))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither_col))) (F_check_ge ((s IDquantize3_ord_dither_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither_row))) (F_check_ge ((s IDquantize3_ord_dither_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquantize3_ord_dither_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquantize3_ord_dither_row))) (F_check_ge ((s IDquantize3_ord_dither_row)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquantize3_ord_dither__tmp)
                                                                    - (s IDquantize3_ord_dither_row)))]
    | _ => []
  end.


Theorem quantize3_ord_dither_ai_correct:
  forall s p' s', steps (g_start quantize3_ord_dither) s (g_edges quantize3_ord_dither) p' s' -> quantize3_ord_dither_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem quantize3_ord_dither_pot_correct:
  forall s p' s',
    steps (g_start quantize3_ord_dither) s (g_edges quantize3_ord_dither) p' s' ->
    (quantize3_ord_dither_pot (g_start quantize3_ord_dither) s >= quantize3_ord_dither_pot p' s')%Q.
Proof.
  check_lp quantize3_ord_dither_ai_correct quantize3_ord_dither_hints.
Qed.

