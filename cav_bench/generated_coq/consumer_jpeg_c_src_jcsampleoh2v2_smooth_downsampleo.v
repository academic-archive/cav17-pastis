Require Import pasta.Pasta.

Notation IDh2v2_smooth_downsample_z := 1%positive.
Notation IDh2v2_smooth_downsample_cinfo_dref_off264 := 2%positive.
Notation IDh2v2_smooth_downsample_colctr := 3%positive.
Notation IDh2v2_smooth_downsample_compptr_dref_off12 := 4%positive.
Notation IDh2v2_smooth_downsample_compptr_dref_off28 := 5%positive.
Notation IDh2v2_smooth_downsample_inrow := 6%positive.
Notation IDh2v2_smooth_downsample_memberscale := 7%positive.
Notation IDh2v2_smooth_downsample_membersum := 8%positive.
Notation IDh2v2_smooth_downsample_neighscale := 9%positive.
Notation IDh2v2_smooth_downsample_neighsum := 10%positive.
Notation IDh2v2_smooth_downsample_output_cols := 11%positive.
Notation IDh2v2_smooth_downsample_outrow := 12%positive.
Notation IDh2v2_smooth_downsample_cinfo := 13%positive.
Notation IDh2v2_smooth_downsample_compptr := 14%positive.
Notation IDh2v2_smooth_downsample_input_data := 15%positive.
Notation IDh2v2_smooth_downsample_output_data := 16%positive.
Definition h2v2_smooth_downsample : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDh2v2_smooth_downsample_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDh2v2_smooth_downsample_colctr) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDh2v2_smooth_downsample_output_cols
             (Some (EMul (EVar IDh2v2_smooth_downsample_compptr_dref_off28)
             (ENum (8))))),5%positive)::
             (5%positive,(AAssign IDh2v2_smooth_downsample_memberscale None),
             6%positive)::
             (6%positive,(AAssign IDh2v2_smooth_downsample_neighscale
             (Some (EMul (EVar IDh2v2_smooth_downsample_cinfo_dref_off264)
             (ENum (16))))),7%positive)::
             (7%positive,(AAssign IDh2v2_smooth_downsample_inrow
             (Some (ENum (0)))),8%positive)::
             (8%positive,(AAssign IDh2v2_smooth_downsample_outrow
             (Some (ENum (0)))),9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDh2v2_smooth_downsample_outrow) s) <
             (eval (EVar IDh2v2_smooth_downsample_compptr_dref_off12)
             s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDh2v2_smooth_downsample_outrow) s) >=
             (eval (EVar IDh2v2_smooth_downsample_compptr_dref_off12)
             s))%Z)),12%positive)::(12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDh2v2_smooth_downsample_membersum None),
             16%positive)::
             (16%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             17%positive)::
             (17%positive,(AAssign IDh2v2_smooth_downsample_neighsum
             (Some (EAdd (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighsum)))),18%positive)::
             (18%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             19%positive)::
             (19%positive,(AAssign IDh2v2_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDh2v2_smooth_downsample_membersum)
             (EVar IDh2v2_smooth_downsample_memberscale))
             (EMul (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighscale))))),20%positive)::
             (20%positive,(AAssign IDh2v2_smooth_downsample_colctr
             (Some (ESub (EVar IDh2v2_smooth_downsample_output_cols)
             (ENum (2))))),21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDh2v2_smooth_downsample_colctr) s) >
             (eval (ENum (0)) s))%Z)),37%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDh2v2_smooth_downsample_colctr) s) <=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDh2v2_smooth_downsample_membersum None),
             26%positive)::
             (26%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             27%positive)::
             (27%positive,(AAssign IDh2v2_smooth_downsample_neighsum
             (Some (EAdd (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighsum)))),28%positive)::
             (28%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             29%positive)::
             (29%positive,(AAssign IDh2v2_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDh2v2_smooth_downsample_membersum)
             (EVar IDh2v2_smooth_downsample_memberscale))
             (EMul (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighscale))))),30%positive)::
             (30%positive,(AAssign IDh2v2_smooth_downsample_inrow
             (Some (EAdd (EVar IDh2v2_smooth_downsample_inrow) (ENum (2))))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDh2v2_smooth_downsample_outrow
             (Some (EAdd (EVar IDh2v2_smooth_downsample_outrow)
             (ENum (1))))),33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDh2v2_smooth_downsample_z
             (Some (EAdd (ENum (1)) (EVar IDh2v2_smooth_downsample_z)))),
             36%positive)::(36%positive,AWeaken,11%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDh2v2_smooth_downsample_membersum None),
             39%positive)::
             (39%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             40%positive)::
             (40%positive,(AAssign IDh2v2_smooth_downsample_neighsum
             (Some (EAdd (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighsum)))),41%positive)::
             (41%positive,(AAssign IDh2v2_smooth_downsample_neighsum None),
             42%positive)::
             (42%positive,(AAssign IDh2v2_smooth_downsample_membersum
             (Some (EAdd (EMul (EVar IDh2v2_smooth_downsample_membersum)
             (EVar IDh2v2_smooth_downsample_memberscale))
             (EMul (EVar IDh2v2_smooth_downsample_neighsum)
             (EVar IDh2v2_smooth_downsample_neighscale))))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDh2v2_smooth_downsample_colctr
             (Some (EAdd (EVar IDh2v2_smooth_downsample_colctr)
             (ENum (-1))))),45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDh2v2_smooth_downsample_z
             (Some (EAdd (ENum (1)) (EVar IDh2v2_smooth_downsample_z)))),
             48%positive)::(48%positive,AWeaken,23%positive)::nil
|}.

Definition h2v2_smooth_downsample_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0)%Z
    | 3%positive => (-1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 4%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0)%Z
    | 5%positive => (-1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 6%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0)%Z
    | 7%positive => (-1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 8%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0)%Z
    | 9%positive => (-1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 10%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0)%Z
    | 11%positive => (-1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 12%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 13%positive => (1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 14%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 16%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 18%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 20%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0)%Z
    | 22%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 25%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 27%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 29%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 31%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0)%Z
    | 32%positive => (-1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 33%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 35%positive => (1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) + 2 <= 0 /\ 1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 46%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_colctr) <= 0)%Z
    | 48%positive => (-1 * (s IDh2v2_smooth_downsample_colctr) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_outrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_inrow) <= 0 /\ -1 * (s IDh2v2_smooth_downsample_compptr_dref_off12)+ 1 * (s IDh2v2_smooth_downsample_outrow) + 1 <= 0 /\ -1 * (s IDh2v2_smooth_downsample_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition h2v2_smooth_downsample_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2
                          + 8 * (s IDh2v2_smooth_downsample_compptr_dref_off28)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 2%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDh2v2_smooth_downsample_compptr_dref_off28)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 3%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDh2v2_smooth_downsample_compptr_dref_off28)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 4%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2
                            + 8 * (s IDh2v2_smooth_downsample_compptr_dref_off28)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 5%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 6%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 7%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 8%positive => ((s IDh2v2_smooth_downsample_z)
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)))%Q
    | 9%positive => (-(s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                 + (s IDh2v2_smooth_downsample_output_cols))
                     + (s IDh2v2_smooth_downsample_z)
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                     + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                     + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                            - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 10%positive => (-(s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                  + (s IDh2v2_smooth_downsample_output_cols))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 11%positive => (-(s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                  + (s IDh2v2_smooth_downsample_output_cols))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 12%positive => (-(s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                  + (s IDh2v2_smooth_downsample_output_cols))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 13%positive => ((s IDh2v2_smooth_downsample_z))%Q
    | 14%positive => (-(s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                  + (s IDh2v2_smooth_downsample_output_cols))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 15%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 16%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 17%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 18%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 19%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 20%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 21%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 22%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 23%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 24%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 25%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 26%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 27%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 28%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 29%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 30%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 31%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 32%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      + (2 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr)))%Q
    | 33%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2)%Q
    | 34%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2)%Q
    | 35%positive => ((1 # 1)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2)%Q
    | 36%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (s IDh2v2_smooth_downsample_outrow) * max0(-2
                                                                   + 
                                                                   (s IDh2v2_smooth_downsample_output_cols))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                             - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2)%Q
    | 37%positive => ((1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + max0((s IDh2v2_smooth_downsample_colctr))
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 38%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 39%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 40%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 41%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 42%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 43%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 44%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 45%positive => ((1 # 1) + (s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 46%positive => ((1 # 1) + (s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 47%positive => ((1 # 1) + (s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | 48%positive => ((s IDh2v2_smooth_downsample_colctr)
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12)
                      - (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))
                      + (1 # 3) * (s IDh2v2_smooth_downsample_compptr_dref_off12) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow)
                      - (1 # 3) * (s IDh2v2_smooth_downsample_outrow) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + (s IDh2v2_smooth_downsample_z)
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0(-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))
                      + max0(-2 + (s IDh2v2_smooth_downsample_output_cols)) * max0((s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))
                      - (1 # 3) * max0(-1
                                       + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow))^2
                      + (2 # 3) * max0((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                       - (s IDh2v2_smooth_downsample_outrow)))%Q
    | _ => (0 # 1)%Q
  end.

Definition h2v2_smooth_downsample_hints (p : node) (s : state) := 
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
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                             - (s IDh2v2_smooth_downsample_outrow)) (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                            - (s IDh2v2_smooth_downsample_outrow));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v2_smooth_downsample_outrow))) (F_check_ge ((s IDh2v2_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0)))]
    | 13%positive => []
    | 14%positive => [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*0 0.333333*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-0.666667 0*) F_max0_pre_decrement ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                            - (s IDh2v2_smooth_downsample_outrow)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDh2v2_smooth_downsample_outrow))) (F_check_ge (-1
                                                                    + (s IDh2v2_smooth_downsample_outrow)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v2_smooth_downsample_compptr_dref_off12)
                                                                    - (s IDh2v2_smooth_downsample_outrow))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v2_smooth_downsample_outrow)) (0))) (F_max0_ge_0 ((s IDh2v2_smooth_downsample_outrow)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDh2v2_smooth_downsample_output_cols))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDh2v2_smooth_downsample_colctr))) (F_check_ge (0) (0))]
    | 37%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDh2v2_smooth_downsample_colctr))) (F_check_ge ((s IDh2v2_smooth_downsample_colctr)) (0))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDh2v2_smooth_downsample_colctr)) (0))) (F_max0_ge_0 ((s IDh2v2_smooth_downsample_colctr)))]
    | _ => []
  end.


Theorem h2v2_smooth_downsample_ai_correct:
  forall s p' s', steps (g_start h2v2_smooth_downsample) s (g_edges h2v2_smooth_downsample) p' s' -> h2v2_smooth_downsample_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem h2v2_smooth_downsample_pot_correct:
  forall s p' s',
    steps (g_start h2v2_smooth_downsample) s (g_edges h2v2_smooth_downsample) p' s' ->
    (h2v2_smooth_downsample_pot (g_start h2v2_smooth_downsample) s >= h2v2_smooth_downsample_pot p' s')%Q.
Proof.
  check_lp h2v2_smooth_downsample_ai_correct h2v2_smooth_downsample_hints.
Qed.

