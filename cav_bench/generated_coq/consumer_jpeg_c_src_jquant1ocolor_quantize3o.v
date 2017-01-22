Require Import pasta.Pasta.

Notation IDcolor_quantize3_z := 1%positive.
Notation IDcolor_quantize3__tmp := 2%positive.
Notation IDcolor_quantize3_cinfo_dref_off128 := 3%positive.
Notation IDcolor_quantize3_col := 4%positive.
Notation IDcolor_quantize3_pixcode := 5%positive.
Notation IDcolor_quantize3_row := 6%positive.
Notation IDcolor_quantize3_width := 7%positive.
Notation IDcolor_quantize3_cinfo := 8%positive.
Notation IDcolor_quantize3_input_buf := 9%positive.
Notation IDcolor_quantize3_num_rows := 10%positive.
Notation IDcolor_quantize3_output_buf := 11%positive.
Definition color_quantize3 : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDcolor_quantize3_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcolor_quantize3_col) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcolor_quantize3__tmp
             (Some (EVar IDcolor_quantize3_num_rows))),5%positive)::
             (5%positive,(AAssign IDcolor_quantize3_width
             (Some (EVar IDcolor_quantize3_cinfo_dref_off128))),6%positive)::
             (6%positive,(AAssign IDcolor_quantize3_row (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDcolor_quantize3_row) s) <
             (eval (EVar IDcolor_quantize3__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDcolor_quantize3_row) s) >=
             (eval (EVar IDcolor_quantize3__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDcolor_quantize3_col
             (Some (EVar IDcolor_quantize3_width))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDcolor_quantize3_col) s) >
             (eval (ENum (0)) s))%Z)),24%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDcolor_quantize3_col) s) <=
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcolor_quantize3_row
             (Some (EAdd (EVar IDcolor_quantize3_row) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDcolor_quantize3_z (Some (EAdd (ENum (1))
             (EVar IDcolor_quantize3_z)))),23%positive)::
             (23%positive,AWeaken,9%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDcolor_quantize3_pixcode None),
             26%positive)::
             (26%positive,(AAssign IDcolor_quantize3_pixcode None),
             27%positive)::
             (27%positive,(AAssign IDcolor_quantize3_pixcode None),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDcolor_quantize3_col
             (Some (EAdd (EVar IDcolor_quantize3_col) (ENum (-1))))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcolor_quantize3_z (Some (EAdd (ENum (1))
             (EVar IDcolor_quantize3_z)))),33%positive)::
             (33%positive,AWeaken,16%positive)::nil
|}.

Definition color_quantize3_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 4%positive => (-1 * (s IDcolor_quantize3_col) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 6%positive => (-1 * (s IDcolor_quantize3_col) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0 /\ 1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 8%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ 1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0 /\ 1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 10%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3__tmp)+ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 11%positive => (1 * (s IDcolor_quantize3__tmp)+ -1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 12%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 14%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 16%positive => (-1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 17%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 18%positive => (1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 19%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 20%positive => (1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 21%positive => (-1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ 1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 22%positive => (1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 23%positive => (-1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ 1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDcolor_quantize3_col) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 26%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDcolor_quantize3_col) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 28%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDcolor_quantize3_col) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 30%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 31%positive => (-1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0)%Z
    | 32%positive => (-1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_z) <= 0 /\ -1 * (s IDcolor_quantize3_col) <= 0)%Z
    | 33%positive => (-1 * (s IDcolor_quantize3_col) <= 0 /\ -1 * (s IDcolor_quantize3__tmp)+ 1 * (s IDcolor_quantize3_row) + 1 <= 0 /\ -1 * (s IDcolor_quantize3_row) <= 0 /\ -1 * (s IDcolor_quantize3_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition color_quantize3_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcolor_quantize3_cinfo_dref_off128)) * max0((s IDcolor_quantize3_num_rows))
                     + max0((s IDcolor_quantize3_num_rows)))%Q
    | 2%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3_cinfo_dref_off128)) * max0((s IDcolor_quantize3_num_rows))
                     + max0((s IDcolor_quantize3_num_rows)))%Q
    | 3%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3_cinfo_dref_off128)) * max0((s IDcolor_quantize3_num_rows))
                     + max0((s IDcolor_quantize3_num_rows)))%Q
    | 4%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3_cinfo_dref_off128)) * max0((s IDcolor_quantize3_num_rows))
                     + max0((s IDcolor_quantize3_num_rows)))%Q
    | 5%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3__tmp))
                     + max0((s IDcolor_quantize3__tmp)) * max0((s IDcolor_quantize3_cinfo_dref_off128)))%Q
    | 6%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3__tmp))
                     + max0((s IDcolor_quantize3__tmp)) * max0((s IDcolor_quantize3_width)))%Q
    | 7%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row))
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 8%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row))
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 9%positive => ((s IDcolor_quantize3_z)
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row))
                     + max0((s IDcolor_quantize3__tmp)
                            - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 10%positive => ((s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 11%positive => ((s IDcolor_quantize3_z))%Q
    | 12%positive => ((s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 13%positive => ((s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 14%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 15%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 16%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 17%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3_col)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3_col)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3_col)))%Q
    | 23%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_z)
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      - max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3_col)))%Q
    | 24%positive => ((s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_col))
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_col))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_col))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 25%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 26%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 27%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 28%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 29%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 30%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 31%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 32%positive => (-(s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | 33%positive => (-(1 # 1)
                      - (s IDcolor_quantize3__tmp) * max0((s IDcolor_quantize3_width))
                      - (s IDcolor_quantize3_col) * max0(-1
                                                         + (s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_col) * max0((s IDcolor_quantize3__tmp))
                      + (s IDcolor_quantize3_row) * max0((s IDcolor_quantize3_width))
                      + (s IDcolor_quantize3_z)
                      - max0(-1 + (s IDcolor_quantize3__tmp))
                      + max0(-1 + (s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width))
                      + max0((s IDcolor_quantize3__tmp))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row))
                      + max0((s IDcolor_quantize3__tmp)
                             - (s IDcolor_quantize3_row)) * max0((s IDcolor_quantize3_width)))%Q
    | _ => (0 # 1)%Q
  end.

Definition color_quantize3_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcolor_quantize3__tmp)
                                                             - (s IDcolor_quantize3_row)) (-1
                                                                    + (s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcolor_quantize3__tmp)
                                            - (s IDcolor_quantize3_row));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_width))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement ((s IDcolor_quantize3__tmp)
                                                    - (s IDcolor_quantize3_row)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row))) (F_check_ge ((s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_width))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)) (0))) (F_max0_ge_0 ((s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0))]
    | 24%positive => [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDcolor_quantize3__tmp))) (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcolor_quantize3__tmp)) (0))) (F_max0_ge_0 ((s IDcolor_quantize3__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcolor_quantize3_col)) (0))) (F_max0_ge_0 ((s IDcolor_quantize3_col)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3_col))) (F_check_ge ((s IDcolor_quantize3_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDcolor_quantize3__tmp))) (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDcolor_quantize3__tmp)) (0))) (F_max0_ge_0 ((s IDcolor_quantize3__tmp)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row))) (F_check_ge (-1
                                                                    + (s IDcolor_quantize3__tmp)
                                                                    - (s IDcolor_quantize3_row)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3__tmp))) (F_check_ge ((s IDcolor_quantize3__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3__tmp))) (F_check_ge ((s IDcolor_quantize3__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3__tmp))) (F_check_ge ((s IDcolor_quantize3__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3_col))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcolor_quantize3_col)) (0))) (F_max0_ge_0 ((s IDcolor_quantize3_col)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3_col))) (F_check_ge ((s IDcolor_quantize3_col)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDcolor_quantize3__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcolor_quantize3__tmp))) (F_check_ge ((s IDcolor_quantize3__tmp)) (0))]
    | _ => []
  end.


Theorem color_quantize3_ai_correct:
  forall s p' s', steps (g_start color_quantize3) s (g_edges color_quantize3) p' s' -> color_quantize3_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem color_quantize3_pot_correct:
  forall s p' s',
    steps (g_start color_quantize3) s (g_edges color_quantize3) p' s' ->
    (color_quantize3_pot (g_start color_quantize3) s >= color_quantize3_pot p' s')%Q.
Proof.
  check_lp color_quantize3_ai_correct color_quantize3_hints.
Qed.

