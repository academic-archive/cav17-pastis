Require Import pasta.Pasta.

Notation IDmain1_z := 1%positive.
Notation IDmain1__tmp := 2%positive.
Notation IDmain1_argindex := 3%positive.
Notation IDmain1_bt := 4%positive.
Notation IDmain1_drawing_mode := 5%positive.
Notation IDmain1_max_no_corners := 6%positive.
Notation IDmain1_max_no_edges := 7%positive.
Notation IDmain1_mode := 8%positive.
Notation IDmain1_principle := 9%positive.
Notation IDmain1_susan_quick := 10%positive.
Notation IDmain1_thin_post_proc := 11%positive.
Notation IDmain1_three_by_three := 12%positive.
Notation IDmain1_argc := 13%positive.
Notation IDmain1_argv := 14%positive.
Definition main1 : graph := {|
  g_start := 1%positive;
  g_end := 93%positive;
  g_edges := (1%positive,(AAssign IDmain1_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmain1__tmp (Some (EVar IDmain1_argc))),
             3%positive)::
             (3%positive,(AAssign IDmain1_argindex (Some (ENum (3)))),
             4%positive)::
             (4%positive,(AAssign IDmain1_bt (Some (ENum (20)))),5%positive)::
             (5%positive,(AAssign IDmain1_principle (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDmain1_thin_post_proc (Some (ENum (1)))),
             7%positive)::
             (7%positive,(AAssign IDmain1_three_by_three (Some (ENum (0)))),
             8%positive)::
             (8%positive,(AAssign IDmain1_drawing_mode (Some (ENum (0)))),
             9%positive)::
             (9%positive,(AAssign IDmain1_susan_quick (Some (ENum (0)))),
             10%positive)::
             (10%positive,(AAssign IDmain1_max_no_corners
             (Some (ENum (1850)))),11%positive)::
             (11%positive,(AAssign IDmain1_max_no_edges
             (Some (ENum (2650)))),12%positive)::
             (12%positive,(AAssign IDmain1_mode (Some (ENum (0)))),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) <
             (eval (ENum (3)) s))%Z)),16%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) >=
             (eval (ENum (3)) s))%Z)),15%positive)::
             (15%positive,AWeaken,18%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDmain1_argindex)
             s) < (eval (EVar IDmain1__tmp) s))%Z)),70%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDmain1_argindex)
             s) >= (eval (EVar IDmain1__tmp) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) = (eval (ENum (1)) s))%Z)),24%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) <> (eval (ENum (1)) s))%Z)),23%positive)::
             (23%positive,AWeaken,31%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDmain1_mode) s) =
             (eval (ENum (0)) s))%Z)),27%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDmain1_mode) s) <>
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,31%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDmain1_mode (Some (ENum (1)))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,68%positive)::
             (31%positive,ANone,67%positive)::
             (31%positive,ANone,44%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) <> (eval (ENum (0)) s))%Z)),41%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) = (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDmain1_susan_quick)
             s) <> (eval (ENum (0)) s))%Z)),38%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDmain1_susan_quick)
             s) = (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,40%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,43%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,68%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) <> (eval (ENum (0)) s))%Z)),59%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDmain1_principle)
             s) = (eval (ENum (0)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_three_by_three) s) <>
             (eval (ENum (0)) s))%Z)),51%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_three_by_three) s) =
             (eval (ENum (0)) s))%Z)),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,54%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_thin_post_proc) s) <>
             (eval (ENum (0)) s))%Z)),56%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_thin_post_proc) s) =
             (eval (ENum (0)) s))%Z)),55%positive)::
             (55%positive,AWeaken,58%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,66%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_three_by_three) s) <>
             (eval (ENum (0)) s))%Z)),63%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDmain1_three_by_three) s) =
             (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,65%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,ANone,68%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,AWeaken,93%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,73%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,111%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,ANone,110%positive)::
             (74%positive,ANone,108%positive)::
             (74%positive,ANone,106%positive)::
             (74%positive,ANone,104%positive)::
             (74%positive,ANone,102%positive)::
             (74%positive,ANone,100%positive)::
             (74%positive,ANone,98%positive)::
             (74%positive,ANone,96%positive)::
             (74%positive,ANone,94%positive)::
             (74%positive,ANone,84%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDmain1_argindex
             (Some (EAdd (EVar IDmain1_argindex) (ENum (1))))),76%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmain1_argindex) (ENum (1))) s) >=
             (eval (EVar IDmain1__tmp) s))%Z)),81%positive)::
             (77%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmain1_argindex) (ENum (1))) s) <
             (eval (EVar IDmain1__tmp) s))%Z)),78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AAssign IDmain1_bt None),80%positive)::
             (80%positive,ANone,110%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,AWeaken,93%positive)::
             (84%positive,(AAssign IDmain1_argindex
             (Some (EAdd (EVar IDmain1_argindex) (ENum (1))))),85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmain1_argindex) (ENum (1))) s) >=
             (eval (EVar IDmain1__tmp) s))%Z)),92%positive)::
             (86%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmain1_argindex) (ENum (1))) s) <
             (eval (EVar IDmain1__tmp) s))%Z)),87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,ANone,89%positive)::
             (88%positive,ANone,91%positive)::
             (89%positive,(AAssign IDmain1_three_by_three (Some (ENum (1)))),
             90%positive)::(90%positive,ANone,91%positive)::
             (91%positive,ANone,110%positive)::
             (92%positive,AWeaken,93%positive)::
             (94%positive,(AAssign IDmain1_susan_quick (Some (ENum (1)))),
             95%positive)::(95%positive,ANone,110%positive)::
             (96%positive,(AAssign IDmain1_three_by_three (Some (ENum (1)))),
             97%positive)::(97%positive,ANone,110%positive)::
             (98%positive,(AAssign IDmain1_drawing_mode (Some (ENum (1)))),
             99%positive)::(99%positive,ANone,110%positive)::
             (100%positive,(AAssign IDmain1_thin_post_proc
             (Some (ENum (0)))),101%positive)::
             (101%positive,ANone,110%positive)::
             (102%positive,(AAssign IDmain1_principle (Some (ENum (1)))),
             103%positive)::(103%positive,ANone,110%positive)::
             (104%positive,(AAssign IDmain1_mode (Some (ENum (2)))),
             105%positive)::(105%positive,ANone,110%positive)::
             (106%positive,(AAssign IDmain1_mode (Some (ENum (1)))),
             107%positive)::(107%positive,ANone,110%positive)::
             (108%positive,(AAssign IDmain1_mode (Some (ENum (0)))),
             109%positive)::(109%positive,ANone,110%positive)::
             (110%positive,ANone,111%positive)::
             (111%positive,(AAssign IDmain1_argindex
             (Some (EAdd (EVar IDmain1_argindex) (ENum (1))))),112%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,ANone,114%positive)::
             (114%positive,(AAssign IDmain1_z (Some (EAdd (ENum (1))
             (EVar IDmain1_z)))),115%positive)::
             (115%positive,AWeaken,20%positive)::nil
|}.

Definition main1_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 4%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0)%Z
    | 5%positive => (-1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0)%Z
    | 6%positive => (-1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0)%Z
    | 7%positive => (-1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0)%Z
    | 9%positive => (-1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0)%Z
    | 10%positive => (-1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0)%Z
    | 11%positive => (-1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0)%Z
    | 12%positive => (-1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 13%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 14%positive => (-1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 15%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1__tmp) + 3 <= 0)%Z
    | 16%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1__tmp) + -2 <= 0)%Z
    | 17%positive => (1 * (s IDmain1__tmp) + -2 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 18%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 19%positive => (-1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_thin_post_proc) + 1 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_bt) + 20 <= 0 /\ 1 * (s IDmain1_bt) + -20 <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_argindex) + -3 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 20%positive => (-1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 21%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 22%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 23%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 24%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 26%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_mode) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_mode) <= 0)%Z
    | 28%positive => (1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 29%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_mode) + -1 <= 0 /\ -1 * (s IDmain1_mode) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDmain1_mode) + 1 <= 0 /\ 1 * (s IDmain1_mode) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 31%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 32%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 33%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 34%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0)%Z
    | 35%positive => (1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 36%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_susan_quick) <= 0)%Z
    | 37%positive => (1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 38%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_susan_quick) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDmain1_susan_quick) + 1 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 40%positive => (-1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0)%Z
    | 41%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 43%positive => (-1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 44%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 45%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 46%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0)%Z
    | 47%positive => (1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 48%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0)%Z
    | 49%positive => (1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 50%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0)%Z
    | 51%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDmain1_three_by_three) + 1 <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 53%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 55%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) <= 0 /\ -1 * (s IDmain1_thin_post_proc) <= 0)%Z
    | 56%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0)%Z
    | 57%positive => (1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 58%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ 1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0)%Z
    | 59%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 61%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ 1 * (s IDmain1_three_by_three) <= 0)%Z
    | 62%positive => (1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 63%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_three_by_three) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDmain1_three_by_three) + 1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 65%positive => (-1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0)%Z
    | 67%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 68%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0)%Z
    | 69%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 70%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 72%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0)%Z
    | 75%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0)%Z
    | 77%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 78%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0)%Z
    | 79%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 80%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0)%Z
    | 81%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) + -1 <= 0)%Z
    | 82%positive => (1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) + -1 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 83%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) + -1 <= 0)%Z
    | 84%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 85%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0)%Z
    | 86%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 87%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0)%Z
    | 88%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 89%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0)%Z
    | 90%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_three_by_three) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) + 1 <= 0)%Z
    | 91%positive => (-1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 2 <= 0)%Z
    | 92%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) + -1 <= 0)%Z
    | 93%positive => (-1 * (s IDmain1_argindex) + 3 <= 0 /\ 1 * (s IDmain1__tmp)+ -1 * (s IDmain1_argindex) + -1 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 94%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 95%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_susan_quick) + -1 <= 0 /\ -1 * (s IDmain1_susan_quick) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 97%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_three_by_three) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) + 1 <= 0)%Z
    | 98%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 99%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_drawing_mode) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) + 1 <= 0)%Z
    | 100%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 101%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_thin_post_proc) <= 0 /\ -1 * (s IDmain1_thin_post_proc) <= 0)%Z
    | 102%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 103%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_principle) + -1 <= 0 /\ -1 * (s IDmain1_principle) + 1 <= 0)%Z
    | 104%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_mode) + -2 <= 0 /\ -1 * (s IDmain1_mode) + 2 <= 0)%Z
    | 106%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 107%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_mode) + -1 <= 0 /\ -1 * (s IDmain1_mode) + 1 <= 0)%Z
    | 108%positive => (-1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 109%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 110%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0)%Z
    | 111%positive => (-1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) + 1 <= 0 /\ -1 * (s IDmain1_argindex) + 3 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 112%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0)%Z
    | 113%positive => (-1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0)%Z
    | 114%positive => (-1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_argindex) + 4 <= 0)%Z
    | 115%positive => (-1 * (s IDmain1_argindex) + 4 <= 0 /\ -1 * (s IDmain1__tmp)+ 1 * (s IDmain1_argindex) <= 0 /\ -1 * (s IDmain1_max_no_corners) + 1850 <= 0 /\ 1 * (s IDmain1_max_no_corners) + -1850 <= 0 /\ -1 * (s IDmain1_drawing_mode) <= 0 /\ 1 * (s IDmain1_thin_post_proc) + -1 <= 0 /\ -1 * (s IDmain1_principle) <= 0 /\ -1 * (s IDmain1_three_by_three) <= 0 /\ -1 * (s IDmain1_susan_quick) <= 0 /\ 1 * (s IDmain1_max_no_edges) + -2650 <= 0 /\ -1 * (s IDmain1_max_no_edges) + 2650 <= 0 /\ -1 * (s IDmain1_mode) <= 0 /\ -1 * (s IDmain1_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition main1_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2 + (s IDmain1_argc)))%Q
    | 2%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1_argc)))%Q
    | 3%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 4%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 5%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 6%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 7%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 8%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 9%positive => ((s IDmain1_z)
                     + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 10%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 11%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 12%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 13%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 14%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 15%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 16%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 17%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 18%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 19%positive => ((s IDmain1_z)
                      + max0(1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 20%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 21%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 22%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 23%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 24%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 25%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 26%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 27%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 28%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 29%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 30%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 31%positive => ((s IDmain1_z))%Q
    | 32%positive => ((s IDmain1_z))%Q
    | 33%positive => ((s IDmain1_z))%Q
    | 34%positive => ((s IDmain1_z))%Q
    | 35%positive => ((s IDmain1_z))%Q
    | 36%positive => ((s IDmain1_z))%Q
    | 37%positive => ((s IDmain1_z))%Q
    | 38%positive => ((s IDmain1_z))%Q
    | 39%positive => ((s IDmain1_z))%Q
    | 40%positive => ((s IDmain1_z))%Q
    | 41%positive => ((s IDmain1_z))%Q
    | 42%positive => ((s IDmain1_z))%Q
    | 43%positive => ((s IDmain1_z))%Q
    | 44%positive => ((s IDmain1_z))%Q
    | 45%positive => ((s IDmain1_z))%Q
    | 46%positive => ((s IDmain1_z))%Q
    | 47%positive => ((s IDmain1_z))%Q
    | 48%positive => ((s IDmain1_z))%Q
    | 49%positive => ((s IDmain1_z))%Q
    | 50%positive => ((s IDmain1_z))%Q
    | 51%positive => ((s IDmain1_z))%Q
    | 52%positive => ((s IDmain1_z))%Q
    | 53%positive => ((s IDmain1_z))%Q
    | 54%positive => ((s IDmain1_z))%Q
    | 55%positive => ((s IDmain1_z))%Q
    | 56%positive => ((s IDmain1_z))%Q
    | 57%positive => ((s IDmain1_z))%Q
    | 58%positive => ((s IDmain1_z))%Q
    | 59%positive => ((s IDmain1_z))%Q
    | 60%positive => ((s IDmain1_z))%Q
    | 61%positive => ((s IDmain1_z))%Q
    | 62%positive => ((s IDmain1_z))%Q
    | 63%positive => ((s IDmain1_z))%Q
    | 64%positive => ((s IDmain1_z))%Q
    | 65%positive => ((s IDmain1_z))%Q
    | 66%positive => ((s IDmain1_z))%Q
    | 67%positive => ((s IDmain1_z))%Q
    | 68%positive => ((s IDmain1_z))%Q
    | 69%positive => ((s IDmain1_z))%Q
    | 70%positive => ((s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 71%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 72%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 73%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 74%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 75%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 76%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 77%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 78%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 79%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 80%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 81%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 82%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 83%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 84%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 85%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 86%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 87%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 88%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 89%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 90%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 91%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 92%positive => ((1 # 1) + (s IDmain1_z)
                      + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 93%positive => ((s IDmain1_z))%Q
    | 94%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 95%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 96%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 97%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 98%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 99%positive => ((1 # 1) + (s IDmain1_z)
                      + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 100%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 101%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 102%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 103%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 104%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 105%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 106%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 107%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 108%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 109%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 110%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 111%positive => ((1 # 1) + (s IDmain1_z)
                       + max0(-1 + (s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 112%positive => ((1 # 1) + (s IDmain1_z)
                       + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 113%positive => ((1 # 1) + (s IDmain1_z)
                       + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 114%positive => ((1 # 1) + (s IDmain1_z)
                       + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | 115%positive => ((s IDmain1_z)
                       + max0((s IDmain1__tmp) - (s IDmain1_argindex)))%Q
    | _ => (0 # 1)%Q
  end.

Definition main1_hints (p : node) (s : state) := 
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
    | 19%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) ((s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) (-1
                                                                    + (s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDmain1__tmp)
                                            - (s IDmain1_argindex))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) (-1
                                                                    + (s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDmain1__tmp)
                                            - (s IDmain1_argindex))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) (-1
                                                                    + (s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDmain1__tmp)
                                            - (s IDmain1_argindex))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
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
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_max0_pre_decrement ((s IDmain1__tmp)
                                                     - (s IDmain1_argindex)) (1)]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDmain1__tmp)
                                                     - (s IDmain1_argindex)) (1)]
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) (-1
                                                                    + (s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDmain1__tmp)
                                            - (s IDmain1_argindex))]
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDmain1__tmp)
                                                     - (s IDmain1_argindex)) (1)]
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain1__tmp)
                                                             - (s IDmain1_argindex)) (-1
                                                                    + (s IDmain1__tmp)
                                                                    - (s IDmain1_argindex)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDmain1__tmp)
                                            - (s IDmain1_argindex))]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | _ => []
  end.


Theorem main1_ai_correct:
  forall s p' s', steps (g_start main1) s (g_edges main1) p' s' -> main1_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem main1_pot_correct:
  forall s p' s',
    steps (g_start main1) s (g_edges main1) p' s' ->
    (main1_pot (g_start main1) s >= main1_pot p' s')%Q.
Proof.
  check_lp main1_ai_correct main1_hints.
Qed.

