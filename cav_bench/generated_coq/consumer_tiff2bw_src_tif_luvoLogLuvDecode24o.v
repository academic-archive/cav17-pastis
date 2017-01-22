Require Import pasta.Pasta.

Notation IDLogLuvDecode24_z := 1%positive.
Notation IDLogLuvDecode24__tmp := 2%positive.
Notation IDLogLuvDecode24__tmp1 := 3%positive.
Notation IDLogLuvDecode24__tmp2 := 4%positive.
Notation IDLogLuvDecode24_cc := 5%positive.
Notation IDLogLuvDecode24_i := 6%positive.
Notation IDLogLuvDecode24_npixels := 7%positive.
Notation IDLogLuvDecode24_tif_dref_off744 := 8%positive.
Notation IDLogLuvDecode24_occ := 9%positive.
Notation IDLogLuvDecode24_op := 10%positive.
Notation IDLogLuvDecode24_s := 11%positive.
Notation IDLogLuvDecode24_tif := 12%positive.
Definition LogLuvDecode24 : graph := {|
  g_start := 1%positive;
  g_end := 48%positive;
  g_edges := (1%positive,(AAssign IDLogLuvDecode24_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDLogLuvDecode24__tmp2
             (Some (EVar IDLogLuvDecode24_occ))),3%positive)::
             (3%positive,(AAssign IDLogLuvDecode24__tmp
             (Some (EVar IDLogLuvDecode24_s))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDLogLuvDecode24__tmp) s) =
             (eval (ENum (0)) s))%Z)),9%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDLogLuvDecode24__tmp) s) <>
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,48%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,15%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,48%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDLogLuvDecode24_npixels None),
             17%positive)::(17%positive,AWeaken,18%positive)::
             (18%positive,ANone,25%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,23%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,48%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,26%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDLogLuvDecode24_cc
             (Some (EVar IDLogLuvDecode24_tif_dref_off744))),27%positive)::
             (27%positive,(AAssign IDLogLuvDecode24_i (Some (ENum (0)))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_i)
             s) < (eval (EVar IDLogLuvDecode24_npixels) s))%Z)),32%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_i)
             s) >= (eval (EVar IDLogLuvDecode24_npixels) s))%Z)),31%positive)::
             (31%positive,AWeaken,37%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_cc)
             s) > (eval (ENum (0)) s))%Z)),49%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_cc)
             s) <= (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDLogLuvDecode24_tif_dref_off744
             (Some (EVar IDLogLuvDecode24_cc))),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_i)
             s) <> (eval (EVar IDLogLuvDecode24_npixels) s))%Z)),44%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDLogLuvDecode24_i)
             s) = (eval (EVar IDLogLuvDecode24_npixels) s))%Z)),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDLogLuvDecode24__tmp1 (Some (ENum (1)))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,AWeaken,48%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDLogLuvDecode24__tmp1 (Some (ENum (0)))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AAssign IDLogLuvDecode24_cc
             (Some (ESub (EVar IDLogLuvDecode24_cc) (ENum (3))))),
             51%positive)::(51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDLogLuvDecode24_i
             (Some (EAdd (EVar IDLogLuvDecode24_i) (ENum (1))))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDLogLuvDecode24_z (Some (EAdd (ENum (1))
             (EVar IDLogLuvDecode24_z)))),56%positive)::
             (56%positive,AWeaken,30%positive)::nil
|}.

Definition LogLuvDecode24_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 3%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 4%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 5%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 6%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 7%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 8%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 9%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 10%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 11%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 13%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 15%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 17%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 19%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 21%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 22%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 23%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 25%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 26%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 27%positive => (1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 28%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0)%Z
    | 29%positive => (-1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 30%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 31%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i)+ 1 * (s IDLogLuvDecode24_npixels) <= 0)%Z
    | 32%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 34%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 36%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ 1 * (s IDLogLuvDecode24_cc) <= 0)%Z
    | 37%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 38%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 39%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 40%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_i)+ 1 * (s IDLogLuvDecode24_npixels) <= 0)%Z
    | 41%positive => (-1 * (s IDLogLuvDecode24_i)+ 1 * (s IDLogLuvDecode24_npixels) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 42%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_i)+ 1 * (s IDLogLuvDecode24_npixels) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp1) + -1 <= 0 /\ -1 * (s IDLogLuvDecode24__tmp1) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDLogLuvDecode24__tmp1) + 1 <= 0 /\ 1 * (s IDLogLuvDecode24__tmp1) + -1 <= 0 /\ -1 * (s IDLogLuvDecode24_i)+ 1 * (s IDLogLuvDecode24_npixels) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 44%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 45%positive => (-1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 46%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp1) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp1) <= 0)%Z
    | 47%positive => (-1 * (s IDLogLuvDecode24__tmp1) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp1) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 48%positive => (-1 * (s IDLogLuvDecode24_z) <= 0)%Z
    | 49%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDLogLuvDecode24_cc) + 1 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 51%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + -2 <= 0)%Z
    | 52%positive => (-1 * (s IDLogLuvDecode24_cc) + -2 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) + 1 <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_i) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 53%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + -2 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_i) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDLogLuvDecode24_i) + 1 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + -2 <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0)%Z
    | 55%positive => (-1 * (s IDLogLuvDecode24__tmp) <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + -2 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_i) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDLogLuvDecode24_i) + 1 <= 0 /\ 1 * (s IDLogLuvDecode24_i)+ -1 * (s IDLogLuvDecode24_npixels) <= 0 /\ -1 * (s IDLogLuvDecode24_cc) + -2 <= 0 /\ 1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24__tmp) <= 0 /\ -1 * (s IDLogLuvDecode24_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition LogLuvDecode24_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 3) * max0(2 + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 2%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 3%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 4%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 5%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 6%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 7%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 8%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 9%positive => ((s IDLogLuvDecode24_z)
                     + (1 # 3) * max0(2
                                      + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 10%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 11%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 12%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 13%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 14%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 15%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 16%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 17%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 18%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 19%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 20%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 21%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 22%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 23%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 24%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 25%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 26%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2
                                       + (s IDLogLuvDecode24_tif_dref_off744)))%Q
    | 27%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 28%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 29%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 30%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 31%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 32%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 33%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 34%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 35%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 36%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 37%positive => ((s IDLogLuvDecode24_z))%Q
    | 38%positive => ((s IDLogLuvDecode24_z))%Q
    | 39%positive => ((s IDLogLuvDecode24_z))%Q
    | 40%positive => ((s IDLogLuvDecode24_z))%Q
    | 41%positive => ((s IDLogLuvDecode24_z))%Q
    | 42%positive => ((s IDLogLuvDecode24_z))%Q
    | 43%positive => ((s IDLogLuvDecode24_z))%Q
    | 44%positive => ((s IDLogLuvDecode24_z))%Q
    | 45%positive => ((s IDLogLuvDecode24_z))%Q
    | 46%positive => ((s IDLogLuvDecode24_z))%Q
    | 47%positive => ((s IDLogLuvDecode24_z))%Q
    | 48%positive => ((s IDLogLuvDecode24_z))%Q
    | 49%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 50%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(-1 + (s IDLogLuvDecode24_cc)))%Q
    | 51%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 52%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 53%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 54%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 55%positive => ((1 # 1) + (s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | 56%positive => ((s IDLogLuvDecode24_z)
                      + (1 # 3) * max0(2 + (s IDLogLuvDecode24_cc)))%Q
    | _ => (0 # 1)%Q
  end.

Definition LogLuvDecode24_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-0.333333 0*) F_max0_ge_0 (2
                                                  + (s IDLogLuvDecode24_tif_dref_off744))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-0.333333 0*) F_max0_ge_0 (2
                                                   + (s IDLogLuvDecode24_tif_dref_off744))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.333333 0*) F_max0_ge_0 (2
                                                   + (s IDLogLuvDecode24_tif_dref_off744))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-0.333333 0*) F_max0_monotonic (F_check_ge (2
                                                                    + 
                                                                    (s IDLogLuvDecode24_cc)) (-1
                                                                    + (s IDLogLuvDecode24_cc)));
                      (*-0.333333 0*) F_max0_ge_0 (-1
                                                   + (s IDLogLuvDecode24_cc))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*0 0.333333*) F_max0_monotonic (F_check_ge (2
                                                                   + 
                                                                   (s IDLogLuvDecode24_cc)) (-1
                                                                    + (s IDLogLuvDecode24_cc)));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDLogLuvDecode24_cc))) (F_check_ge (0) (0))]
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
    | 49%positive => [(*-0.333333 0*) F_max0_pre_decrement (2
                                                            + (s IDLogLuvDecode24_cc)) (3)]
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | _ => []
  end.


Theorem LogLuvDecode24_ai_correct:
  forall s p' s', steps (g_start LogLuvDecode24) s (g_edges LogLuvDecode24) p' s' -> LogLuvDecode24_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem LogLuvDecode24_pot_correct:
  forall s p' s',
    steps (g_start LogLuvDecode24) s (g_edges LogLuvDecode24) p' s' ->
    (LogLuvDecode24_pot (g_start LogLuvDecode24) s >= LogLuvDecode24_pot p' s')%Q.
Proof.
  check_lp LogLuvDecode24_ai_correct LogLuvDecode24_hints.
Qed.

