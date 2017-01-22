Require Import pasta.Pasta.

Notation IDh2v1_downsample_z := 1%positive.
Notation IDh2v1_downsample_bias := 2%positive.
Notation IDh2v1_downsample_compptr_dref_off12 := 3%positive.
Notation IDh2v1_downsample_compptr_dref_off28 := 4%positive.
Notation IDh2v1_downsample_outcol := 5%positive.
Notation IDh2v1_downsample_output_cols := 6%positive.
Notation IDh2v1_downsample_outrow := 7%positive.
Notation IDh2v1_downsample_cinfo := 8%positive.
Notation IDh2v1_downsample_compptr := 9%positive.
Notation IDh2v1_downsample_input_data := 10%positive.
Notation IDh2v1_downsample_output_data := 11%positive.
Definition h2v1_downsample : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDh2v1_downsample_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_output_cols) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_outcol) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDh2v1_downsample_output_cols
             (Some (EMul (EVar IDh2v1_downsample_compptr_dref_off28)
             (ENum (8))))),6%positive)::
             (6%positive,(AAssign IDh2v1_downsample_outrow
             (Some (ENum (0)))),7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_outrow) s) <
             (eval (EVar IDh2v1_downsample_compptr_dref_off12) s))%Z)),
             12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_outrow) s) >=
             (eval (EVar IDh2v1_downsample_compptr_dref_off12) s))%Z)),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDh2v1_downsample_bias (Some (ENum (0)))),
             14%positive)::
             (14%positive,(AAssign IDh2v1_downsample_outcol
             (Some (ENum (0)))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_outcol) s) <
             (eval (EVar IDh2v1_downsample_output_cols) s))%Z)),25%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_downsample_outcol) s) >=
             (eval (EVar IDh2v1_downsample_output_cols) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDh2v1_downsample_outrow
             (Some (EAdd (EVar IDh2v1_downsample_outrow) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDh2v1_downsample_z (Some (EAdd (ENum (1))
             (EVar IDh2v1_downsample_z)))),24%positive)::
             (24%positive,AWeaken,9%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDh2v1_downsample_bias None),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDh2v1_downsample_outcol
             (Some (EAdd (EVar IDh2v1_downsample_outcol) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDh2v1_downsample_z (Some (EAdd (ENum (1))
             (EVar IDh2v1_downsample_z)))),32%positive)::
             (32%positive,AWeaken,17%positive)::nil
|}.

Definition h2v1_downsample_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0)%Z
    | 3%positive => (-1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_output_cols) <= 0)%Z
    | 4%positive => (-1 * (s IDh2v1_downsample_output_cols) <= 0 /\ 1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 5%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_output_cols) <= 0)%Z
    | 6%positive => (1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 7%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0)%Z
    | 8%positive => (-1 * (s IDh2v1_downsample_outrow) <= 0 /\ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ 1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 9%positive => (-1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 10%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_compptr_dref_off12)+ -1 * (s IDh2v1_downsample_outrow) <= 0)%Z
    | 11%positive => (1 * (s IDh2v1_downsample_compptr_dref_off12)+ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 12%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 14%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ 1 * (s IDh2v1_downsample_bias) <= 0 /\ -1 * (s IDh2v1_downsample_bias) <= 0)%Z
    | 15%positive => (-1 * (s IDh2v1_downsample_bias) <= 0 /\ 1 * (s IDh2v1_downsample_bias) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ 1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0)%Z
    | 16%positive => (-1 * (s IDh2v1_downsample_outcol) <= 0 /\ 1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ 1 * (s IDh2v1_downsample_bias) <= 0 /\ -1 * (s IDh2v1_downsample_bias) <= 0)%Z
    | 17%positive => (-1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0)%Z
    | 19%positive => (-1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0)%Z
    | 21%positive => (-1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0)%Z
    | 23%positive => (-1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outcol)+ 1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_z) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDh2v1_downsample_outcol) + 1 <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_z) <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_outcol) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDh2v1_downsample_outcol) + 1 <= 0 /\ 1 * (s IDh2v1_downsample_outcol)+ -1 * (s IDh2v1_downsample_output_cols) <= 0 /\ -1 * (s IDh2v1_downsample_outrow) <= 0 /\ -1 * (s IDh2v1_downsample_compptr_dref_off12)+ 1 * (s IDh2v1_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v1_downsample_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition h2v1_downsample_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0(8 * (s IDh2v1_downsample_compptr_dref_off28)))%Q
    | 2%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0(8 * (s IDh2v1_downsample_compptr_dref_off28)))%Q
    | 3%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0(8 * (s IDh2v1_downsample_compptr_dref_off28)))%Q
    | 4%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0(8 * (s IDh2v1_downsample_compptr_dref_off28)))%Q
    | 5%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0(8 * (s IDh2v1_downsample_compptr_dref_off28)))%Q
    | 6%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 7%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 8%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 9%positive => ((s IDh2v1_downsample_z)
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow))
                     + max0((s IDh2v1_downsample_compptr_dref_off12)
                            - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 10%positive => ((s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 11%positive => ((s IDh2v1_downsample_z))%Q
    | 12%positive => ((s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 13%positive => ((s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 14%positive => ((s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 15%positive => ((s IDh2v1_downsample_z)
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols)))%Q
    | 16%positive => ((s IDh2v1_downsample_z)
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols)))%Q
    | 17%positive => ((s IDh2v1_downsample_z)
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols)))%Q
    | 18%positive => ((s IDh2v1_downsample_z)
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 23%positive => ((1 # 1)
                      + (s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 24%positive => ((s IDh2v1_downsample_compptr_dref_off12) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))
                      - (s IDh2v1_downsample_outrow) * max0(-(s IDh2v1_downsample_outcol)
                                                            + (s IDh2v1_downsample_output_cols))
                      + (s IDh2v1_downsample_z)
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      - max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 25%positive => ((s IDh2v1_downsample_z)
                      - max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols))
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0(-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + 
                                                                    (s IDh2v1_downsample_output_cols)))%Q
    | 26%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-1 - (s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)))%Q
    | 27%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-1 - (s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)))%Q
    | 28%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0(-1 - (s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)))%Q
    | 29%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 30%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 31%positive => ((1 # 1) + (s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | 32%positive => ((s IDh2v1_downsample_z)
                      + max0(-1 + (s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow)) * max0((s IDh2v1_downsample_output_cols))
                      + max0((s IDh2v1_downsample_compptr_dref_off12)
                             - (s IDh2v1_downsample_outrow))
                      + max0(-(s IDh2v1_downsample_outcol)
                             + (s IDh2v1_downsample_output_cols)))%Q
    | _ => (0 # 1)%Q
  end.

Definition h2v1_downsample_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDh2v1_downsample_compptr_dref_off12)
                                                             - (s IDh2v1_downsample_outrow)) (-1
                                                                    + (s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDh2v1_downsample_compptr_dref_off12)
                                            - (s IDh2v1_downsample_outrow));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*0 1*) F_max0_pre_decrement ((s IDh2v1_downsample_compptr_dref_off12)
                                                    - (s IDh2v1_downsample_outrow)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow))) (F_check_ge ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDh2v1_downsample_outcol)
                                                                 + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0))]
    | 25%positive => [(*0 1*) F_max0_pre_decrement (-(s IDh2v1_downsample_outcol)
                                                    + (s IDh2v1_downsample_output_cols)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow))) (F_check_ge ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow))) (F_check_ge (-1
                                                                    + (s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDh2v1_downsample_compptr_dref_off12)
                                                                    - (s IDh2v1_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDh2v1_downsample_outcol)
                                                                    + (s IDh2v1_downsample_output_cols))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem h2v1_downsample_ai_correct:
  forall s p' s', steps (g_start h2v1_downsample) s (g_edges h2v1_downsample) p' s' -> h2v1_downsample_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem h2v1_downsample_pot_correct:
  forall s p' s',
    steps (g_start h2v1_downsample) s (g_edges h2v1_downsample) p' s' ->
    (h2v1_downsample_pot (g_start h2v1_downsample) s >= h2v1_downsample_pot p' s')%Q.
Proof.
  check_lp h2v1_downsample_ai_correct h2v1_downsample_hints.
Qed.

