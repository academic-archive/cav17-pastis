Require Import pasta.Pasta.

Notation IDh2v1_fancy_upsample_z := 1%positive.
Notation IDh2v1_fancy_upsample_cinfo_dref_off392 := 2%positive.
Notation IDh2v1_fancy_upsample_colctr := 3%positive.
Notation IDh2v1_fancy_upsample_compptr_dref_off40 := 4%positive.
Notation IDh2v1_fancy_upsample_inrow := 5%positive.
Notation IDh2v1_fancy_upsample_invalue := 6%positive.
Notation IDh2v1_fancy_upsample_cinfo := 7%positive.
Notation IDh2v1_fancy_upsample_compptr := 8%positive.
Notation IDh2v1_fancy_upsample_input_data := 9%positive.
Notation IDh2v1_fancy_upsample_output_data_ptr := 10%positive.
Definition h2v1_fancy_upsample : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDh2v1_fancy_upsample_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_fancy_upsample_colctr) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDh2v1_fancy_upsample_inrow
             (Some (ENum (0)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_fancy_upsample_inrow) s) <
             (eval (EVar IDh2v1_fancy_upsample_cinfo_dref_off392) s))%Z)),
             10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_fancy_upsample_inrow) s) >=
             (eval (EVar IDh2v1_fancy_upsample_cinfo_dref_off392) s))%Z)),
             8%positive)::(8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDh2v1_fancy_upsample_invalue None),
             12%positive)::
             (12%positive,(AAssign IDh2v1_fancy_upsample_colctr
             (Some (ESub (EVar IDh2v1_fancy_upsample_compptr_dref_off40)
             (ENum (2))))),13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_fancy_upsample_colctr) s) >
             (eval (ENum (0)) s))%Z)),24%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDh2v1_fancy_upsample_colctr) s) <=
             (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDh2v1_fancy_upsample_invalue None),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDh2v1_fancy_upsample_inrow
             (Some (EAdd (EVar IDh2v1_fancy_upsample_inrow) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDh2v1_fancy_upsample_z
             (Some (EAdd (ENum (1)) (EVar IDh2v1_fancy_upsample_z)))),
             23%positive)::(23%positive,AWeaken,7%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDh2v1_fancy_upsample_invalue None),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDh2v1_fancy_upsample_colctr
             (Some (EAdd (EVar IDh2v1_fancy_upsample_colctr) (ENum (-1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDh2v1_fancy_upsample_z
             (Some (EAdd (ENum (1)) (EVar IDh2v1_fancy_upsample_z)))),
             31%positive)::(31%positive,AWeaken,15%positive)::nil
|}.

Definition h2v1_fancy_upsample_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0)%Z
    | 3%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) <= 0)%Z
    | 4%positive => (-1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0)%Z
    | 5%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 6%positive => (-1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0)%Z
    | 7%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 8%positive => (-1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 9%positive => (1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 10%positive => (-1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 12%positive => (-1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 14%positive => (-1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0)%Z
    | 17%positive => (1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0)%Z
    | 19%positive => (1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 21%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0)%Z
    | 22%positive => (-1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0)%Z
    | 23%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ 1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDh2v1_fancy_upsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDh2v1_fancy_upsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) <= 0)%Z
    | 29%positive => (-1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_colctr) <= 0)%Z
    | 31%positive => (-1 * (s IDh2v1_fancy_upsample_colctr) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_inrow) <= 0 /\ -1 * (s IDh2v1_fancy_upsample_cinfo_dref_off392)+ 1 * (s IDh2v1_fancy_upsample_inrow) + 1 <= 0 /\ -1 * (s IDh2v1_fancy_upsample_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition h2v1_fancy_upsample_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)))%Q
    | 2%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)))%Q
    | 3%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)))%Q
    | 4%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)))%Q
    | 5%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                            - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 6%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                            - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 7%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                            - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 8%positive => ((s IDh2v1_fancy_upsample_z)
                     + max0(-2 + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                     + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                            - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 9%positive => ((s IDh2v1_fancy_upsample_z))%Q
    | 10%positive => ((s IDh2v1_fancy_upsample_z)
                      + max0(-2
                             + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 11%positive => ((1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + max0(-2
                             + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 12%positive => ((1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + max0(-2
                             + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 13%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 14%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 15%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 16%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 17%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 23%positive => ((s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 24%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow)) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 25%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 26%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 27%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr))
                      + (1 # 2) * max0(-1 + (s IDh2v1_fancy_upsample_colctr)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow)))%Q
    | 28%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (3 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 29%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (3 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 30%positive => (-(1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (3 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | 31%positive => (-(1 # 1)
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_cinfo_dref_off392) * max0((s IDh2v1_fancy_upsample_colctr))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_colctr) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * (s IDh2v1_fancy_upsample_inrow) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (s IDh2v1_fancy_upsample_z)
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0(-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0(-2
                                       + (s IDh2v1_fancy_upsample_compptr_dref_off40)) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      - max0(-1 + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                             - (s IDh2v1_fancy_upsample_inrow))
                      - (1 # 2) * max0(-1
                                       + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))^2
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)) * max0((s IDh2v1_fancy_upsample_colctr))
                      + (3 # 2) * max0((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                       - (s IDh2v1_fancy_upsample_inrow))
                      + (1 # 2) * max0((s IDh2v1_fancy_upsample_colctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition h2v1_fancy_upsample_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                            - (s IDh2v1_fancy_upsample_inrow)) (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                           - (s IDh2v1_fancy_upsample_inrow));
                     (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)))]
    | 9%positive => []
    | 10%positive => [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*0 1*) F_max0_pre_decrement ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                    - (s IDh2v1_fancy_upsample_inrow)) (1);
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v1_fancy_upsample_compptr_dref_off40))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0))]
    | 24%positive => [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_colctr)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_fancy_upsample_colctr))) (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-1.66678e-12 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-0.5 0*) F_max0_pre_decrement ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                       - (s IDh2v1_fancy_upsample_inrow)) (1);
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_max0_ge_0 ((s IDh2v1_fancy_upsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v1_fancy_upsample_cinfo_dref_off392)
                                                                    - (s IDh2v1_fancy_upsample_inrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_colctr))) (F_check_ge ((s IDh2v1_fancy_upsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v1_fancy_upsample_cinfo_dref_off392))) (F_check_ge ((s IDh2v1_fancy_upsample_cinfo_dref_off392)) (0))]
    | _ => []
  end.


Theorem h2v1_fancy_upsample_ai_correct:
  forall s p' s', steps (g_start h2v1_fancy_upsample) s (g_edges h2v1_fancy_upsample) p' s' -> h2v1_fancy_upsample_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem h2v1_fancy_upsample_pot_correct:
  forall s p' s',
    steps (g_start h2v1_fancy_upsample) s (g_edges h2v1_fancy_upsample) p' s' ->
    (h2v1_fancy_upsample_pot (g_start h2v1_fancy_upsample) s >= h2v1_fancy_upsample_pot p' s')%Q.
Proof.
  check_lp h2v1_fancy_upsample_ai_correct h2v1_fancy_upsample_hints.
Qed.

