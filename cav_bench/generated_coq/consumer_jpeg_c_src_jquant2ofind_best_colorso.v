Require Import pasta.Pasta.

Notation IDfind_best_colors_z := 1%positive.
Notation IDfind_best_colors__tmp := 2%positive.
Notation IDfind_best_colors__tmp1 := 3%positive.
Notation IDfind_best_colors__tmp2 := 4%positive.
Notation IDfind_best_colors__tmp3 := 5%positive.
Notation IDfind_best_colors_dist0 := 6%positive.
Notation IDfind_best_colors_dist1 := 7%positive.
Notation IDfind_best_colors_dist2 := 8%positive.
Notation IDfind_best_colors_i := 9%positive.
Notation IDfind_best_colors_ic0 := 10%positive.
Notation IDfind_best_colors_ic1 := 11%positive.
Notation IDfind_best_colors_ic2 := 12%positive.
Notation IDfind_best_colors_icolor := 13%positive.
Notation IDfind_best_colors_inc0 := 14%positive.
Notation IDfind_best_colors_inc1 := 15%positive.
Notation IDfind_best_colors_inc2 := 16%positive.
Notation IDfind_best_colors_xx0 := 17%positive.
Notation IDfind_best_colors_xx1 := 18%positive.
Notation IDfind_best_colors_xx2 := 19%positive.
Notation IDfind_best_colors_bestcolor := 20%positive.
Notation IDfind_best_colors_cinfo := 21%positive.
Notation IDfind_best_colors_colorlist := 22%positive.
Notation IDfind_best_colors_minc0 := 23%positive.
Notation IDfind_best_colors_minc1 := 24%positive.
Notation IDfind_best_colors_minc2 := 25%positive.
Notation IDfind_best_colors_numcolors := 26%positive.
Definition find_best_colors : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDfind_best_colors_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfind_best_colors__tmp3
             (Some (EVar IDfind_best_colors_minc0))),3%positive)::
             (3%positive,(AAssign IDfind_best_colors__tmp2
             (Some (EVar IDfind_best_colors_minc1))),4%positive)::
             (4%positive,(AAssign IDfind_best_colors__tmp1
             (Some (EVar IDfind_best_colors_minc2))),5%positive)::
             (5%positive,(AAssign IDfind_best_colors__tmp
             (Some (EVar IDfind_best_colors_numcolors))),6%positive)::
             (6%positive,(AAssign IDfind_best_colors_i (Some (ENum (127)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDfind_best_colors_i)
             s) >= (eval (ENum (0)) s))%Z)),83%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDfind_best_colors_i)
             s) < (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDfind_best_colors_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_i) s) <
             (eval (EVar IDfind_best_colors__tmp) s))%Z)),17%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_i) s) >=
             (eval (EVar IDfind_best_colors__tmp) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDfind_best_colors_icolor None),
             19%positive)::
             (19%positive,(AAssign IDfind_best_colors_inc0 None),20%positive)::
             (20%positive,(AAssign IDfind_best_colors_dist0
             (Some (EMul (EVar IDfind_best_colors_inc0)
             (EVar IDfind_best_colors_inc0)))),21%positive)::
             (21%positive,(AAssign IDfind_best_colors_inc1 None),22%positive)::
             (22%positive,(AAssign IDfind_best_colors_dist0
             (Some (EAdd (EVar IDfind_best_colors_dist0)
             (EMul (EVar IDfind_best_colors_inc1)
             (EVar IDfind_best_colors_inc1))))),23%positive)::
             (23%positive,(AAssign IDfind_best_colors_inc2 None),24%positive)::
             (24%positive,(AAssign IDfind_best_colors_dist0
             (Some (EAdd (EVar IDfind_best_colors_dist0)
             (EMul (EVar IDfind_best_colors_inc2)
             (EVar IDfind_best_colors_inc2))))),25%positive)::
             (25%positive,(AAssign IDfind_best_colors_inc0
             (Some (EAdd (EMul (EVar IDfind_best_colors_inc0) (ENum (32)))
             (ENum (256))))),26%positive)::
             (26%positive,(AAssign IDfind_best_colors_inc1
             (Some (EAdd (EMul (EVar IDfind_best_colors_inc1) (ENum (24)))
             (ENum (144))))),27%positive)::
             (27%positive,(AAssign IDfind_best_colors_inc2
             (Some (EAdd (EMul (EVar IDfind_best_colors_inc2) (ENum (16)))
             (ENum (64))))),28%positive)::
             (28%positive,(AAssign IDfind_best_colors_xx0
             (Some (EVar IDfind_best_colors_inc0))),29%positive)::
             (29%positive,(AAssign IDfind_best_colors_ic0 (Some (ENum (3)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic0) s) >=
             (eval (ENum (0)) s))%Z)),40%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic0) s) <
             (eval (ENum (0)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDfind_best_colors_i
             (Some (EAdd (EVar IDfind_best_colors_i) (ENum (1))))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDfind_best_colors_z
             (Some (EAdd (ENum (1)) (EVar IDfind_best_colors_z)))),
             39%positive)::(39%positive,AWeaken,14%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDfind_best_colors_dist1
             (Some (EVar IDfind_best_colors_dist0))),42%positive)::
             (42%positive,(AAssign IDfind_best_colors_xx1
             (Some (EVar IDfind_best_colors_inc1))),43%positive)::
             (43%positive,(AAssign IDfind_best_colors_ic1 (Some (ENum (7)))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic1) s) >=
             (eval (ENum (0)) s))%Z)),56%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic1) s) <
             (eval (ENum (0)) s))%Z)),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AAssign IDfind_best_colors_dist0
             (Some (EAdd (EVar IDfind_best_colors_dist0)
             (EVar IDfind_best_colors_xx0)))),49%positive)::
             (49%positive,(AAssign IDfind_best_colors_xx0
             (Some (EAdd (EVar IDfind_best_colors_xx0) (ENum (512))))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDfind_best_colors_ic0
             (Some (EAdd (EVar IDfind_best_colors_ic0) (ENum (-1))))),
             52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDfind_best_colors_z
             (Some (EAdd (ENum (1)) (EVar IDfind_best_colors_z)))),
             55%positive)::(55%positive,AWeaken,32%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AAssign IDfind_best_colors_dist2
             (Some (EVar IDfind_best_colors_dist1))),58%positive)::
             (58%positive,(AAssign IDfind_best_colors_xx2
             (Some (EVar IDfind_best_colors_inc2))),59%positive)::
             (59%positive,(AAssign IDfind_best_colors_ic2 (Some (ENum (3)))),
             60%positive)::(60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic2) s) >=
             (eval (ENum (0)) s))%Z)),72%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EVar IDfind_best_colors_ic2) s) <
             (eval (ENum (0)) s))%Z)),63%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDfind_best_colors_dist1
             (Some (EAdd (EVar IDfind_best_colors_dist1)
             (EVar IDfind_best_colors_xx1)))),65%positive)::
             (65%positive,(AAssign IDfind_best_colors_xx1
             (Some (EAdd (EVar IDfind_best_colors_xx1) (ENum (288))))),
             66%positive)::(66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDfind_best_colors_ic1
             (Some (EAdd (EVar IDfind_best_colors_ic1) (ENum (-1))))),
             68%positive)::(68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDfind_best_colors_z
             (Some (EAdd (ENum (1)) (EVar IDfind_best_colors_z)))),
             71%positive)::(71%positive,AWeaken,46%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,74%positive)::
             (73%positive,ANone,75%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDfind_best_colors_dist2
             (Some (EAdd (EVar IDfind_best_colors_dist2)
             (EVar IDfind_best_colors_xx2)))),76%positive)::
             (76%positive,(AAssign IDfind_best_colors_xx2
             (Some (EAdd (EVar IDfind_best_colors_xx2) (ENum (128))))),
             77%positive)::(77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDfind_best_colors_ic2
             (Some (EAdd (EVar IDfind_best_colors_ic2) (ENum (-1))))),
             79%positive)::(79%positive,ANone,80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDfind_best_colors_z
             (Some (EAdd (ENum (1)) (EVar IDfind_best_colors_z)))),
             82%positive)::(82%positive,AWeaken,62%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,(AAssign IDfind_best_colors_i
             (Some (EAdd (EVar IDfind_best_colors_i) (ENum (-1))))),
             86%positive)::(86%positive,ANone,87%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,(AAssign IDfind_best_colors_z
             (Some (EAdd (ENum (1)) (EVar IDfind_best_colors_z)))),
             89%positive)::(89%positive,AWeaken,9%positive)::nil
|}.

Definition find_best_colors_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_z) <= 0)%Z
    | 4%positive => (1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_z) <= 0)%Z
    | 6%positive => (1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + -127 <= 0 /\ -1 * (s IDfind_best_colors_i) + 127 <= 0)%Z
    | 8%positive => (-1 * (s IDfind_best_colors_i) + 127 <= 0 /\ 1 * (s IDfind_best_colors_i) + -127 <= 0 /\ 1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 9%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + -127 <= 0 /\ -1 * (s IDfind_best_colors_i) + -1 <= 0)%Z
    | 10%positive => (-1 * (s IDfind_best_colors_i) + -1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) + -1 <= 0)%Z
    | 12%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 13%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 14%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 15%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors__tmp)+ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 16%positive => (1 * (s IDfind_best_colors__tmp)+ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 17%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 19%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 21%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 23%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 25%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 27%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 29%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic0) + 3 <= 0)%Z
    | 31%positive => (-1 * (s IDfind_best_colors_ic0) + 3 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDfind_best_colors_ic0) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDfind_best_colors_ic0) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDfind_best_colors_ic0) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic0) <= 0)%Z
    | 41%positive => (-1 * (s IDfind_best_colors_ic0) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic0) <= 0)%Z
    | 43%positive => (-1 * (s IDfind_best_colors_ic0) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic0) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_ic1) + 7 <= 0)%Z
    | 45%positive => (-1 * (s IDfind_best_colors_ic1) + 7 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_ic0) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 47%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + 1 <= 0)%Z
    | 48%positive => (1 * (s IDfind_best_colors_ic1) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 49%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + 1 <= 0)%Z
    | 50%positive => (1 * (s IDfind_best_colors_ic1) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 51%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDfind_best_colors_ic1) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -2 <= 0)%Z
    | 53%positive => (1 * (s IDfind_best_colors_ic0) + -2 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + 1 <= 0)%Z
    | 54%positive => (1 * (s IDfind_best_colors_ic1) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -2 <= 0)%Z
    | 55%positive => (1 * (s IDfind_best_colors_ic0) + -2 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) + 1 <= 0)%Z
    | 56%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic1) <= 0)%Z
    | 57%positive => (-1 * (s IDfind_best_colors_ic1) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 58%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic1) <= 0)%Z
    | 59%positive => (-1 * (s IDfind_best_colors_ic1) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 60%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic1) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + 3 <= 0)%Z
    | 61%positive => (-1 * (s IDfind_best_colors_ic2) + 3 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic1) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 62%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0)%Z
    | 63%positive => (-1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + 1 <= 0)%Z
    | 64%positive => (1 * (s IDfind_best_colors_ic2) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0)%Z
    | 65%positive => (-1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDfind_best_colors_ic2) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0)%Z
    | 67%positive => (-1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + 1 <= 0)%Z
    | 68%positive => (1 * (s IDfind_best_colors_ic2) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -6 <= 0)%Z
    | 69%positive => (1 * (s IDfind_best_colors_ic1) + -6 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDfind_best_colors_ic2) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -6 <= 0)%Z
    | 71%positive => (1 * (s IDfind_best_colors_ic1) + -6 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + 1 <= 0 /\ -1 * (s IDfind_best_colors_z) + 1 <= 0)%Z
    | 72%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic2) <= 0)%Z
    | 73%positive => (-1 * (s IDfind_best_colors_ic2) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 74%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic2) <= 0)%Z
    | 75%positive => (-1 * (s IDfind_best_colors_ic2) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 76%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic2) <= 0)%Z
    | 77%positive => (-1 * (s IDfind_best_colors_ic2) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0)%Z
    | 78%positive => (1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -3 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_ic2) <= 0)%Z
    | 79%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -2 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0)%Z
    | 80%positive => (-1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -2 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 81%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -2 <= 0 /\ -1 * (s IDfind_best_colors_ic2) + -1 <= 0)%Z
    | 82%positive => (-1 * (s IDfind_best_colors_ic2) + -1 <= 0 /\ 1 * (s IDfind_best_colors_ic2) + -2 <= 0 /\ 1 * (s IDfind_best_colors_ic0) + -3 <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors__tmp)+ 1 * (s IDfind_best_colors_i) + 1 <= 0 /\ 1 * (s IDfind_best_colors_ic1) + -7 <= 0 /\ -1 * (s IDfind_best_colors_z) + 1 <= 0)%Z
    | 83%positive => (1 * (s IDfind_best_colors_i) + -127 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 84%positive => (-1 * (s IDfind_best_colors_i) <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + -127 <= 0)%Z
    | 85%positive => (1 * (s IDfind_best_colors_i) + -127 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0 /\ -1 * (s IDfind_best_colors_i) <= 0)%Z
    | 86%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + -126 <= 0 /\ -1 * (s IDfind_best_colors_i) + -1 <= 0)%Z
    | 87%positive => (-1 * (s IDfind_best_colors_i) + -1 <= 0 /\ 1 * (s IDfind_best_colors_i) + -126 <= 0 /\ -1 * (s IDfind_best_colors_z) <= 0)%Z
    | 88%positive => (-1 * (s IDfind_best_colors_z) <= 0 /\ 1 * (s IDfind_best_colors_i) + -126 <= 0 /\ -1 * (s IDfind_best_colors_i) + -1 <= 0)%Z
    | 89%positive => (-1 * (s IDfind_best_colors_i) + -1 <= 0 /\ 1 * (s IDfind_best_colors_i) + -126 <= 0 /\ -1 * (s IDfind_best_colors_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition find_best_colors_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors_numcolors)))%Q
    | 2%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors_numcolors))
                     + max0((s IDfind_best_colors_z)))%Q
    | 3%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors_numcolors))
                     + max0((s IDfind_best_colors_z)))%Q
    | 4%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors_numcolors))
                     + max0((s IDfind_best_colors_z)))%Q
    | 5%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors_numcolors))
                     + max0((s IDfind_best_colors_z)))%Q
    | 6%positive => ((128 # 1)
                     + (165 # 1) * max0((s IDfind_best_colors__tmp))
                     + max0((s IDfind_best_colors_z)))%Q
    | 7%positive => (max0(1 + (s IDfind_best_colors_i))
                     + (165 # 1) * max0((s IDfind_best_colors__tmp))
                     + max0((s IDfind_best_colors_z)))%Q
    | 8%positive => (max0(1 + (s IDfind_best_colors_i))
                     + (165 # 1) * max0((s IDfind_best_colors__tmp))
                     + max0((s IDfind_best_colors_z)))%Q
    | 9%positive => (max0(1 + (s IDfind_best_colors_i))
                     + (165 # 1) * max0((s IDfind_best_colors__tmp))
                     + max0((s IDfind_best_colors_z)))%Q
    | 10%positive => (max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 11%positive => ((165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 12%positive => ((165 # 1) * max0((s IDfind_best_colors__tmp)
                                       - (s IDfind_best_colors_i))
                      + max0((s IDfind_best_colors_z)))%Q
    | 13%positive => ((165 # 1) * max0((s IDfind_best_colors__tmp)
                                       - (s IDfind_best_colors_i))
                      + max0((s IDfind_best_colors_z)))%Q
    | 14%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 15%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 16%positive => ((s IDfind_best_colors_z))%Q
    | 17%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 18%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 19%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 20%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 21%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 22%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 23%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 24%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 25%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 26%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 27%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 28%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 29%positive => ((s IDfind_best_colors_z)
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 30%positive => (-(164 # 1) + (s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 31%positive => (-(164 # 1) + (s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 32%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 33%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 34%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 35%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 36%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 37%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 38%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 39%positive => ((s IDfind_best_colors_z)
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i)))%Q
    | 40%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 41%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 42%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 43%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0)))%Q
    | 44%positive => (-(39 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 45%positive => (-(39 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 46%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 47%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 48%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 49%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 50%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 51%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 52%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 53%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 54%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 55%positive => ((1 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0(1 + (s IDfind_best_colors_ic0))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1)))%Q
    | 56%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 57%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 58%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 59%positive => ((2 # 1) + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 60%positive => (-(3 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 61%positive => (-(3 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 62%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 63%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 64%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 65%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 66%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 67%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 68%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 69%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 70%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 71%positive => ((5 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (5 # 1) * max0(1 + (s IDfind_best_colors_ic1))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0)))%Q
    | 72%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 73%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 74%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 75%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 76%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 77%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 78%positive => ((4 # 1) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1))
                      + (1 # 2) * max0((s IDfind_best_colors_ic2)))%Q
    | 79%positive => ((9 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 80%positive => ((9 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 81%positive => ((9 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 82%positive => ((7 # 2) + (1 # 2) * (s IDfind_best_colors_ic2)
                      + (s IDfind_best_colors_z)
                      + (165 # 1) * max0(-1 + (s IDfind_best_colors__tmp)
                                         - (s IDfind_best_colors_i))
                      + (1 # 2) * max0(1 + (s IDfind_best_colors_ic2))
                      + (41 # 1) * max0((s IDfind_best_colors_ic0))
                      + (5 # 1) * max0((s IDfind_best_colors_ic1)))%Q
    | 83%positive => (max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 84%positive => ((1 # 1) + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_i))
                      + max0((s IDfind_best_colors_z)))%Q
    | 85%positive => ((1 # 1) + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_i))
                      + max0((s IDfind_best_colors_z)))%Q
    | 86%positive => ((1 # 1) + max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 87%positive => ((1 # 1) + max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 88%positive => ((1 # 1) + max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp))
                      + max0((s IDfind_best_colors_z)))%Q
    | 89%positive => ((1 # 1) + max0(-1 + (s IDfind_best_colors_z))
                      + max0(1 + (s IDfind_best_colors_i))
                      + (165 # 1) * max0((s IDfind_best_colors__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition find_best_colors_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDfind_best_colors_i)) ((s IDfind_best_colors_i)));
                      (*-1 0*) F_max0_ge_0 ((s IDfind_best_colors_i))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfind_best_colors_z))) (F_check_ge ((s IDfind_best_colors_z)) (0))]
    | 14%positive => []
    | 15%positive => [(*-165 0*) F_max0_monotonic (F_check_ge ((s IDfind_best_colors__tmp)
                                                               - (s IDfind_best_colors_i)) (-1
                                                                    + (s IDfind_best_colors__tmp)
                                                                    - (s IDfind_best_colors_i)));
                      (*-165 0*) F_max0_ge_0 (-1
                                              + (s IDfind_best_colors__tmp)
                                              - (s IDfind_best_colors_i))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-165 0*) F_max0_pre_decrement ((s IDfind_best_colors__tmp)
                                                       - (s IDfind_best_colors_i)) (1)]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-41 0*) F_max0_monotonic (F_check_ge (1
                                                              + (s IDfind_best_colors_ic0)) ((s IDfind_best_colors_ic0)));
                      (*-41 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfind_best_colors_ic0))) (F_check_ge (0) (0))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-41 0*) F_max0_pre_decrement (1
                                                      + (s IDfind_best_colors_ic0)) (1)]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-5 0*) F_max0_ge_0 (1 + (s IDfind_best_colors_ic1))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-5 0*) F_max0_pre_decrement (1
                                                     + (s IDfind_best_colors_ic1)) (1)]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*0 1*) F_max0_ge_0 (1 + (s IDfind_best_colors_ic2));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDfind_best_colors_ic2)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDfind_best_colors_ic2)))]
    | 72%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfind_best_colors_ic2)) (0))) (F_max0_ge_0 ((s IDfind_best_colors_ic2)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDfind_best_colors_ic2))) (F_check_ge (1
                                                                    + (s IDfind_best_colors_ic2)) (0))]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    + (s IDfind_best_colors_i)) (1)]
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfind_best_colors_z)) (0))) (F_max0_ge_0 ((s IDfind_best_colors_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDfind_best_colors_z))) (F_check_ge (-1
                                                                    + (s IDfind_best_colors_z)) (0))]
    | _ => []
  end.


Theorem find_best_colors_ai_correct:
  forall s p' s', steps (g_start find_best_colors) s (g_edges find_best_colors) p' s' -> find_best_colors_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem find_best_colors_pot_correct:
  forall s p' s',
    steps (g_start find_best_colors) s (g_edges find_best_colors) p' s' ->
    (find_best_colors_pot (g_start find_best_colors) s >= find_best_colors_pot p' s')%Q.
Proof.
  check_lp find_best_colors_ai_correct find_best_colors_hints.
Qed.

