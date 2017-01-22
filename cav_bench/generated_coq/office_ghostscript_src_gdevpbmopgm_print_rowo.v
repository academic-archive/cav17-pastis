Require Import pasta.Pasta.

Notation IDpgm_print_row_z := 1%positive.
Notation IDpgm_print_row__tmp := 2%positive.
Notation IDpgm_print_row_mask := 3%positive.
Notation IDpgm_print_row_pdev_dref_off1984 := 4%positive.
Notation IDpgm_print_row_pdev_dref_off64 := 5%positive.
Notation IDpgm_print_row_pixel := 6%positive.
Notation IDpgm_print_row_shift := 7%positive.
Notation IDpgm_print_row_x := 8%positive.
Notation IDpgm_print_row_data := 9%positive.
Notation IDpgm_print_row_depth := 10%positive.
Notation IDpgm_print_row_pdev := 11%positive.
Notation IDpgm_print_row_pstream := 12%positive.
Definition pgm_print_row : graph := {|
  g_start := 1%positive;
  g_end := 54%positive;
  g_edges := (1%positive,(AAssign IDpgm_print_row_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpgm_print_row_x)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_pdev_dref_off64) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpgm_print_row__tmp
             (Some (EVar IDpgm_print_row_depth))),6%positive)::
             (6%positive,(AAssign IDpgm_print_row_mask None),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_pdev_dref_off1984) s) <>
             (eval (ENum (0)) s))%Z)),10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_pdev_dref_off1984) s) =
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,13%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row__tmp) s) =
             (eval (ENum (8)) s))%Z)),51%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row__tmp) s) <>
             (eval (ENum (8)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDpgm_print_row_x (Some (ENum (0)))),
             14%positive)::
             (14%positive,(AAssign IDpgm_print_row_shift
             (Some (ESub (ENum (8)) (EVar IDpgm_print_row__tmp)))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDpgm_print_row_x)
             s) < (eval (EVar IDpgm_print_row_pdev_dref_off64) s))%Z)),
             21%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDpgm_print_row_x)
             s) >= (eval (EVar IDpgm_print_row_pdev_dref_off64) s))%Z)),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,54%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_shift) s) <
             (eval (ENum (0)) s))%Z)),33%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_shift) s) >=
             (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDpgm_print_row_pixel None),25%positive)::
             (25%positive,(AAssign IDpgm_print_row_shift
             (Some (ESub (EVar IDpgm_print_row_shift)
             (EVar IDpgm_print_row__tmp)))),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDpgm_print_row_shift)
             (EVar IDpgm_print_row__tmp)) s) < (eval (ENum (0)) s))%Z)),
             29%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDpgm_print_row_shift)
             (EVar IDpgm_print_row__tmp)) s) >= (eval (ENum (0)) s))%Z)),
             28%positive)::(28%positive,AWeaken,32%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDpgm_print_row_shift
             (Some (EAdd (EVar IDpgm_print_row_shift) (ENum (8))))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,ANone,36%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDpgm_print_row_pixel None),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDpgm_print_row_x
             (Some (EAdd (EVar IDpgm_print_row_x) (ENum (1))))),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_pdev_dref_off1984) s) <>
             (eval (ENum (0)) s))%Z)),45%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDpgm_print_row_pdev_dref_off1984) s) =
             (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDpgm_print_row_x)
             s) = (eval (EVar IDpgm_print_row_pdev_dref_off64) s))%Z)),
             44%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDpgm_print_row_x)
             s) <> (eval (EVar IDpgm_print_row_pdev_dref_off64) s))%Z)),
             41%positive)::(41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,47%positive)::
             (44%positive,AWeaken,47%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDpgm_print_row_z (Some (EAdd (ENum (1))
             (EVar IDpgm_print_row_z)))),50%positive)::
             (50%positive,AWeaken,17%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::nil
|}.

Definition pgm_print_row_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 4%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 5%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 6%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 7%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 8%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 9%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0)%Z
    | 10%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 11%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 12%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 13%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 14%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 15%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 16%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 17%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0)%Z
    | 18%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off64)+ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 19%positive => (1 * (s IDpgm_print_row_pdev_dref_off64)+ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0)%Z
    | 20%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off64)+ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 21%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 23%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_shift) <= 0)%Z
    | 24%positive => (-1 * (s IDpgm_print_row_shift) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 25%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_shift) <= 0)%Z
    | 26%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0)%Z
    | 27%positive => (-1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0 /\ 1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0)%Z
    | 29%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ 1 * (s IDpgm_print_row_shift) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDpgm_print_row__tmp)+ 1 * (s IDpgm_print_row_shift) + 1 <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ 1 * (s IDpgm_print_row_shift) + -7 <= 0 /\ -1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) + 8 <= 0)%Z
    | 32%positive => (-1 * (s IDpgm_print_row__tmp)+ -1 * (s IDpgm_print_row_shift) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_shift) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDpgm_print_row_shift) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 35%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_shift) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 37%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 39%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0)%Z
    | 40%positive => (-1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 41%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 43%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpgm_print_row_pdev_dref_off64)+ -1 * (s IDpgm_print_row_x) <= 0)%Z
    | 45%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 47%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0)%Z
    | 49%positive => (-1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_x) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDpgm_print_row_x) + 1 <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64)+ 1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row__tmp) + -8 <= 0 /\ -1 * (s IDpgm_print_row__tmp) + 8 <= 0)%Z
    | 52%positive => (-1 * (s IDpgm_print_row__tmp) + 8 <= 0 /\ 1 * (s IDpgm_print_row__tmp) + -8 <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | 53%positive => (-1 * (s IDpgm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ 1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_x) <= 0 /\ 1 * (s IDpgm_print_row__tmp) + -8 <= 0 /\ -1 * (s IDpgm_print_row__tmp) + 8 <= 0)%Z
    | 54%positive => (-1 * (s IDpgm_print_row_x) <= 0 /\ -1 * (s IDpgm_print_row_z) <= 0 /\ -1 * (s IDpgm_print_row_pdev_dref_off64) <= 0)%Z
    | _ => False
  end.

Definition pgm_print_row_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 2%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 3%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 4%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 5%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 6%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 7%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 8%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 9%positive => ((s IDpgm_print_row_z)
                     + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 10%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 11%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 12%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 13%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 14%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 15%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 16%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 17%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 18%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 19%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 20%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 21%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)
                             - (s IDpgm_print_row_x)))%Q
    | 22%positive => ((1 # 2) + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x))
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 23%positive => ((1 # 2) + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x))
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 24%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 25%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 26%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 27%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 28%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 29%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 30%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 31%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 32%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 33%positive => ((1 # 2) + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x))
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 34%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 35%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 36%positive => ((1 # 2) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 37%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 38%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 39%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 40%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 41%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 42%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 43%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 44%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 45%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 46%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 47%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 48%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 49%positive => ((1 # 1) + (1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 50%positive => ((1 # 2) * (s IDpgm_print_row_pdev_dref_off64)
                      - (1 # 2) * (s IDpgm_print_row_x)
                      + (s IDpgm_print_row_z)
                      + (1 # 2) * max0((s IDpgm_print_row_pdev_dref_off64)
                                       - (s IDpgm_print_row_x)))%Q
    | 51%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 52%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 53%positive => ((s IDpgm_print_row_z)
                      + max0((s IDpgm_print_row_pdev_dref_off64)))%Q
    | 54%positive => ((s IDpgm_print_row_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pgm_print_row_hints (p : node) (s : state) := 
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
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpgm_print_row_pdev_dref_off64)
                                                             - (s IDpgm_print_row_x)) (-1
                                                                    + (s IDpgm_print_row_pdev_dref_off64)
                                                                    - (s IDpgm_print_row_x)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDpgm_print_row_pdev_dref_off64)
                                            - (s IDpgm_print_row_x))]
    | 21%positive => [(*0 0.5*) F_max0_pre_decrement ((s IDpgm_print_row_pdev_dref_off64)
                                                      - (s IDpgm_print_row_x)) (1)]
    | 22%positive => []
    | 23%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDpgm_print_row_pdev_dref_off64)
                                                                    - 
                                                                    (s IDpgm_print_row_x))) (F_check_ge (-1
                                                                    + (s IDpgm_print_row_pdev_dref_off64)
                                                                    - (s IDpgm_print_row_x)) (0))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-0.5 0*) F_max0_pre_decrement ((s IDpgm_print_row_pdev_dref_off64)
                                                       - (s IDpgm_print_row_x)) (1)]
    | 29%positive => [(*-0.5 0*) F_max0_pre_decrement ((s IDpgm_print_row_pdev_dref_off64)
                                                       - (s IDpgm_print_row_x)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpgm_print_row_pdev_dref_off64)
                                                                    - 
                                                                    (s IDpgm_print_row_x))) (F_check_ge ((s IDpgm_print_row_pdev_dref_off64)
                                                                    - (s IDpgm_print_row_x)) (0))]
    | 34%positive => []
    | 35%positive => []
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
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpgm_print_row_pdev_dref_off64)
                                                                    - (s IDpgm_print_row_x)) (0))) (F_max0_ge_0 ((s IDpgm_print_row_pdev_dref_off64)
                                                                    - (s IDpgm_print_row_x)))]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_max0_ge_0 ((s IDpgm_print_row_pdev_dref_off64))]
    | 54%positive => []
    | _ => []
  end.


Theorem pgm_print_row_ai_correct:
  forall s p' s', steps (g_start pgm_print_row) s (g_edges pgm_print_row) p' s' -> pgm_print_row_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pgm_print_row_pot_correct:
  forall s p' s',
    steps (g_start pgm_print_row) s (g_edges pgm_print_row) p' s' ->
    (pgm_print_row_pot (g_start pgm_print_row) s >= pgm_print_row_pot p' s')%Q.
Proof.
  check_lp pgm_print_row_ai_correct pgm_print_row_hints.
Qed.

