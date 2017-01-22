Require Import pasta.Pasta.

Notation IDfullsize_smooth_downsample_z := 1%positive.
Notation IDfullsize_smooth_downsample_cinfo_dref_off264 := 2%positive.
Notation IDfullsize_smooth_downsample_colctr := 3%positive.
Notation IDfullsize_smooth_downsample_colsum := 4%positive.
Notation IDfullsize_smooth_downsample_compptr_dref_off12 := 5%positive.
Notation IDfullsize_smooth_downsample_compptr_dref_off28 := 6%positive.
Notation IDfullsize_smooth_downsample_lastcolsum := 7%positive.
Notation IDfullsize_smooth_downsample_memberscale := 8%positive.
Notation IDfullsize_smooth_downsample_membersum := 9%positive.
Notation IDfullsize_smooth_downsample_neighscale := 10%positive.
Notation IDfullsize_smooth_downsample_neighsum := 11%positive.
Notation IDfullsize_smooth_downsample_nextcolsum := 12%positive.
Notation IDfullsize_smooth_downsample_output_cols := 13%positive.
Notation IDfullsize_smooth_downsample_outrow := 14%positive.
Notation IDfullsize_smooth_downsample_cinfo := 15%positive.
Notation IDfullsize_smooth_downsample_compptr := 16%positive.
Notation IDfullsize_smooth_downsample_input_data := 17%positive.
Notation IDfullsize_smooth_downsample_output_data := 18%positive.
Definition fullsize_smooth_downsample : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDfullsize_smooth_downsample_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDfullsize_smooth_downsample_colctr)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDfullsize_smooth_downsample_output_cols
             (Some (EMul (EVar IDfullsize_smooth_downsample_compptr_dref_off28)
             (ENum (8))))),5%positive)::
             (5%positive,(AAssign IDfullsize_smooth_downsample_memberscale
             None),6%positive)::
             (6%positive,(AAssign IDfullsize_smooth_downsample_neighscale
             (Some (EMul (EVar IDfullsize_smooth_downsample_cinfo_dref_off264)
             (ENum (64))))),7%positive)::
             (7%positive,(AAssign IDfullsize_smooth_downsample_outrow
             (Some (ENum (0)))),8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDfullsize_smooth_downsample_outrow) s) <
             (eval (EVar IDfullsize_smooth_downsample_compptr_dref_off12)
             s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDfullsize_smooth_downsample_outrow)
             s) >=
             (eval (EVar IDfullsize_smooth_downsample_compptr_dref_off12)
             s))%Z)),11%positive)::(11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDfullsize_smooth_downsample_colsum None),
             15%positive)::
             (15%positive,(AAssign IDfullsize_smooth_downsample_membersum
             None),16%positive)::
             (16%positive,(AAssign IDfullsize_smooth_downsample_nextcolsum
             None),17%positive)::
             (17%positive,(AAssign IDfullsize_smooth_downsample_neighsum
             (Some (EAdd (EAdd (EVar IDfullsize_smooth_downsample_colsum)
             (ESub (EVar IDfullsize_smooth_downsample_colsum)
             (EVar IDfullsize_smooth_downsample_membersum)))
             (EVar IDfullsize_smooth_downsample_nextcolsum)))),18%positive)::
             (18%positive,(AAssign IDfullsize_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDfullsize_smooth_downsample_membersum)
             (EVar IDfullsize_smooth_downsample_memberscale))
             (EMul (EVar IDfullsize_smooth_downsample_neighsum)
             (EVar IDfullsize_smooth_downsample_neighscale))))),19%positive)::
             (19%positive,(AAssign IDfullsize_smooth_downsample_lastcolsum
             (Some (EVar IDfullsize_smooth_downsample_colsum))),20%positive)::
             (20%positive,(AAssign IDfullsize_smooth_downsample_colsum
             (Some (EVar IDfullsize_smooth_downsample_nextcolsum))),
             21%positive)::
             (21%positive,(AAssign IDfullsize_smooth_downsample_colctr
             (Some (ESub (EVar IDfullsize_smooth_downsample_output_cols)
             (ENum (2))))),22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDfullsize_smooth_downsample_colctr) s) >
             (eval (ENum (0)) s))%Z)),35%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDfullsize_smooth_downsample_colctr)
             s) <= (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDfullsize_smooth_downsample_membersum
             None),27%positive)::
             (27%positive,(AAssign IDfullsize_smooth_downsample_neighsum
             (Some (EAdd (EAdd (EVar IDfullsize_smooth_downsample_lastcolsum)
             (ESub (EVar IDfullsize_smooth_downsample_colsum)
             (EVar IDfullsize_smooth_downsample_membersum)))
             (EVar IDfullsize_smooth_downsample_colsum)))),28%positive)::
             (28%positive,(AAssign IDfullsize_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDfullsize_smooth_downsample_membersum)
             (EVar IDfullsize_smooth_downsample_memberscale))
             (EMul (EVar IDfullsize_smooth_downsample_neighsum)
             (EVar IDfullsize_smooth_downsample_neighscale))))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDfullsize_smooth_downsample_outrow
             (Some (EAdd (EVar IDfullsize_smooth_downsample_outrow)
             (ENum (1))))),31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDfullsize_smooth_downsample_z
             (Some (EAdd (ENum (1)) (EVar IDfullsize_smooth_downsample_z)))),
             34%positive)::(34%positive,AWeaken,10%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDfullsize_smooth_downsample_membersum
             None),37%positive)::
             (37%positive,(AAssign IDfullsize_smooth_downsample_nextcolsum
             None),38%positive)::
             (38%positive,(AAssign IDfullsize_smooth_downsample_neighsum
             (Some (EAdd (EAdd (EVar IDfullsize_smooth_downsample_lastcolsum)
             (ESub (EVar IDfullsize_smooth_downsample_colsum)
             (EVar IDfullsize_smooth_downsample_membersum)))
             (EVar IDfullsize_smooth_downsample_nextcolsum)))),39%positive)::
             (39%positive,(AAssign IDfullsize_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDfullsize_smooth_downsample_membersum)
             (EVar IDfullsize_smooth_downsample_memberscale))
             (EMul (EVar IDfullsize_smooth_downsample_neighsum)
             (EVar IDfullsize_smooth_downsample_neighscale))))),40%positive)::
             (40%positive,(AAssign IDfullsize_smooth_downsample_lastcolsum
             (Some (EVar IDfullsize_smooth_downsample_colsum))),41%positive)::
             (41%positive,(AAssign IDfullsize_smooth_downsample_colsum
             (Some (EVar IDfullsize_smooth_downsample_nextcolsum))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDfullsize_smooth_downsample_colctr
             (Some (EAdd (EVar IDfullsize_smooth_downsample_colctr)
             (ENum (-1))))),44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDfullsize_smooth_downsample_z
             (Some (EAdd (ENum (1)) (EVar IDfullsize_smooth_downsample_z)))),
             47%positive)::(47%positive,AWeaken,24%positive)::nil
|}.

Definition fullsize_smooth_downsample_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 4%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 6%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 8%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 9%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 10%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 11%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 12%positive => (1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 13%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 15%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 17%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 19%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 21%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 23%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 25%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 26%positive => (1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 27%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 28%positive => (1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 29%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 30%positive => (1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 31%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0)%Z
    | 33%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ 1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 37%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 39%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 41%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0)%Z
    | 43%positive => (-1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 45%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) <= 0)%Z
    | 46%positive => (-1 * (s IDfullsize_smooth_downsample_z) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_colctr) <= 0)%Z
    | 47%positive => (-1 * (s IDfullsize_smooth_downsample_colctr) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_outrow) <= 0 /\ -1 * (s IDfullsize_smooth_downsample_compptr_dref_off12)+ 1 * (s IDfullsize_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDfullsize_smooth_downsample_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition fullsize_smooth_downsample_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2
                          + 8 * (s IDfullsize_smooth_downsample_compptr_dref_off28)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 2%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDfullsize_smooth_downsample_compptr_dref_off28)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 3%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDfullsize_smooth_downsample_compptr_dref_off28)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 4%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDfullsize_smooth_downsample_compptr_dref_off28)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 5%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2 + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 6%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2 + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 7%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2 + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)))%Q
    | 8%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2 + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                            - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 9%positive => ((s IDfullsize_smooth_downsample_z)
                     + max0(-2 + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                     + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                            - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 10%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 11%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 12%positive => ((s IDfullsize_smooth_downsample_z))%Q
    | 13%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 14%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 15%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 16%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 17%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 18%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 19%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 20%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 21%positive => ((s IDfullsize_smooth_downsample_z)
                      + max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 22%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0((s IDfullsize_smooth_downsample_colctr))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_outrow) * max0((s IDfullsize_smooth_downsample_colctr))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_colctr)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 23%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0((s IDfullsize_smooth_downsample_colctr))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_outrow) * max0((s IDfullsize_smooth_downsample_colctr))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_colctr)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 24%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0((s IDfullsize_smooth_downsample_colctr))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_outrow) * max0((s IDfullsize_smooth_downsample_colctr))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_colctr)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 25%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0((s IDfullsize_smooth_downsample_colctr))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_outrow) * max0((s IDfullsize_smooth_downsample_colctr))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_colctr)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 26%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1
                             + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_colctr)))%Q
    | 27%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1
                             + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_colctr)))%Q
    | 28%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1
                             + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_colctr)))%Q
    | 29%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1
                             + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_colctr)))%Q
    | 30%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1
                             + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_colctr)))%Q
    | 31%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 32%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 33%positive => ((1 # 1)
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 34%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 35%positive => ((s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0((s IDfullsize_smooth_downsample_colctr))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_outrow) * max0((s IDfullsize_smooth_downsample_colctr))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_colctr)) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 36%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 37%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 38%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 39%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 40%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 41%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 42%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 43%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0(-1 + (s IDfullsize_smooth_downsample_colctr))
                      + max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                             - (s IDfullsize_smooth_downsample_outrow)))%Q
    | 44%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      - max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (2 # 1) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                       - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_outrow)))%Q
    | 45%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      - max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (2 # 1) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                       - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_outrow)))%Q
    | 46%positive => ((1 # 1)
                      - (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      - max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (2 # 1) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                       - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_outrow)))%Q
    | 47%positive => (-(s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_colctr) * max0((s IDfullsize_smooth_downsample_outrow))
                      + (s IDfullsize_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      - (s IDfullsize_smooth_downsample_outrow) * max0(-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))
                      + (s IDfullsize_smooth_downsample_z)
                      - max0(-2
                             + (s IDfullsize_smooth_downsample_output_cols))
                      + max0((s IDfullsize_smooth_downsample_colctr))
                      - max0((s IDfullsize_smooth_downsample_compptr_dref_off12))
                      + (2 # 1) * max0((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                       - (s IDfullsize_smooth_downsample_outrow))
                      + max0((s IDfullsize_smooth_downsample_outrow)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fullsize_smooth_downsample_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                             - (s IDfullsize_smooth_downsample_outrow)) (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                            - (s IDfullsize_smooth_downsample_outrow));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*0 1*) F_max0_pre_decrement ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                    - (s IDfullsize_smooth_downsample_outrow)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0)))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfullsize_smooth_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0))]
    | 35%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfullsize_smooth_downsample_colctr)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_compptr_dref_off12))) (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0)))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                     - (s IDfullsize_smooth_downsample_outrow)) (1);
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - 
                                                                    (s IDfullsize_smooth_downsample_outrow))) (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge ((s IDfullsize_smooth_downsample_colctr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_outrow))) (F_check_ge ((s IDfullsize_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfullsize_smooth_downsample_colctr))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfullsize_smooth_downsample_outrow))) (F_check_ge ((s IDfullsize_smooth_downsample_outrow)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfullsize_smooth_downsample_compptr_dref_off12)) (0))) (F_max0_ge_0 ((s IDfullsize_smooth_downsample_compptr_dref_off12)));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDfullsize_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDfullsize_smooth_downsample_outrow)))]
    | _ => []
  end.


Theorem fullsize_smooth_downsample_ai_correct:
  forall s p' s', steps (g_start fullsize_smooth_downsample) s (g_edges fullsize_smooth_downsample) p' s' -> fullsize_smooth_downsample_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fullsize_smooth_downsample_pot_correct:
  forall s p' s',
    steps (g_start fullsize_smooth_downsample) s (g_edges fullsize_smooth_downsample) p' s' ->
    (fullsize_smooth_downsample_pot (g_start fullsize_smooth_downsample) s >= fullsize_smooth_downsample_pot p' s')%Q.
Proof.
  check_lp fullsize_smooth_downsample_ai_correct fullsize_smooth_downsample_hints.
Qed.

