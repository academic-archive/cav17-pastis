Require Import pasta.Pasta.

Notation IDmain1_z := 1%positive.
Notation IDmain1__tmp := 2%positive.
Notation IDmain1_by := 3%positive.
Notation IDmain1_ch := 4%positive.
Notation IDmain1_err := 5%positive.
Notation IDmain1_i := 6%positive.
Notation IDmain1_key_len := 7%positive.
Notation IDmain1_argc := 8%positive.
Notation IDmain1_argv := 9%positive.
Definition main1 : graph := {|
  g_start := 1%positive;
  g_end := 98%positive;
  g_edges := (1%positive,(AAssign IDmain1_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmain1__tmp (Some (EVar IDmain1_argc))),
             3%positive)::
             (3%positive,(AAssign IDmain1_i (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDmain1_by (Some (ENum (0)))),5%positive)::
             (5%positive,(AAssign IDmain1_key_len (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDmain1_err (Some (ENum (0)))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) <>
             (eval (ENum (5)) s))%Z)),86%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp) s) =
             (eval (ENum (5)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,13%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,87%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDmain1_i (Some (ENum (0)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmain1_i) s) <
             (eval (ENum (64)) s))%Z)),18%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmain1_i) s) >=
             (eval (ENum (64)) s))%Z)),17%positive)::
             (17%positive,AWeaken,23%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => True)),55%positive)::
             (21%positive,(AGuard (fun s => True)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,52%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDmain1_i) s) <
             (eval (ENum (32)) s))%Z)),48%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDmain1_i) s) >=
             (eval (ENum (32)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,49%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDmain1_key_len None),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,35%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDmain1_err (Some (ENum (-5)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,90%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,40%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDmain1_err (Some (ENum (-6)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,AWeaken,90%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,44%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDmain1_err None),43%positive)::
             (43%positive,ANone,46%positive)::
             (44%positive,(AAssign IDmain1_err None),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,90%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDmain1_err (Some (ENum (-4)))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,AWeaken,90%positive)::
             (52%positive,(AAssign IDmain1_err (Some (ENum (-3)))),
             53%positive)::(53%positive,ANone,54%positive)::
             (54%positive,AWeaken,90%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AAssign IDmain1_ch None),57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) >=
             (eval (ENum (48)) s))%Z)),60%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) <
             (eval (ENum (48)) s))%Z)),59%positive)::
             (59%positive,AWeaken,63%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) <=
             (eval (ENum (57)) s))%Z)),75%positive)::
             (61%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) >
             (eval (ENum (57)) s))%Z)),62%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) >=
             (eval (ENum (65)) s))%Z)),65%positive)::
             (63%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) <
             (eval (ENum (65)) s))%Z)),64%positive)::
             (64%positive,AWeaken,68%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) <=
             (eval (ENum (70)) s))%Z)),71%positive)::
             (66%positive,(AGuard (fun s => ((eval (EVar IDmain1_ch) s) >
             (eval (ENum (70)) s))%Z)),67%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AAssign IDmain1_err (Some (ENum (-2)))),
             69%positive)::(69%positive,ANone,70%positive)::
             (70%positive,AWeaken,90%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,(AAssign IDmain1_by None),73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,ANone,78%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AAssign IDmain1_by None),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDmain1_i (Some (EAdd (EVar IDmain1_i)
             (ENum (1))))),79%positive)::(79%positive,AWeaken,80%positive)::
             (80%positive,ANone,81%positive)::
             (80%positive,ANone,82%positive)::
             (81%positive,ANone,82%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,ANone,84%positive)::
             (84%positive,(AAssign IDmain1_z (Some (EAdd (ENum (1))
             (EVar IDmain1_z)))),85%positive)::
             (85%positive,AWeaken,16%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,(AAssign IDmain1_err (Some (ENum (-1)))),
             88%positive)::(88%positive,ANone,89%positive)::
             (89%positive,AWeaken,90%positive)::
             (90%positive,ANone,92%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,AWeaken,94%positive)::
             (92%positive,ANone,93%positive)::
             (93%positive,AWeaken,94%positive)::
             (94%positive,ANone,96%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,AWeaken,98%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,AWeaken,98%positive)::nil
|}.

Definition main1_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 4%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0)%Z
    | 5%positive => (-1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0)%Z
    | 6%positive => (-1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 7%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0)%Z
    | 8%positive => (-1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 9%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0)%Z
    | 10%positive => (-1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 11%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0)%Z
    | 12%positive => (-1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 13%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0)%Z
    | 14%positive => (-1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0)%Z
    | 15%positive => (-1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0)%Z
    | 16%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 17%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) + 64 <= 0)%Z
    | 18%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0)%Z
    | 19%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 20%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0)%Z
    | 21%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 22%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0)%Z
    | 23%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 24%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 25%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 26%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 27%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 28%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 29%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 30%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 31%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0)%Z
    | 32%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 33%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ 1 * (s IDmain1_err) + 5 <= 0 /\ -1 * (s IDmain1_err) + -5 <= 0)%Z
    | 34%positive => (-1 * (s IDmain1_err) + -5 <= 0 /\ 1 * (s IDmain1_err) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 35%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 36%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0)%Z
    | 37%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 38%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ 1 * (s IDmain1_err) + 6 <= 0 /\ -1 * (s IDmain1_err) + -6 <= 0)%Z
    | 39%positive => (-1 * (s IDmain1_err) + -6 <= 0 /\ 1 * (s IDmain1_err) + 6 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 40%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 41%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0)%Z
    | 42%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 43%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0)%Z
    | 44%positive => (1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 45%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0)%Z
    | 46%positive => (1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 32 <= 0)%Z
    | 47%positive => (-1 * (s IDmain1_i) + 32 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0)%Z
    | 48%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -31 <= 0)%Z
    | 49%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 50%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ 1 * (s IDmain1_err) + 4 <= 0 /\ -1 * (s IDmain1_err) + -4 <= 0)%Z
    | 51%positive => (-1 * (s IDmain1_err) + -4 <= 0 /\ 1 * (s IDmain1_err) + 4 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 52%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 53%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) + 3 <= 0 /\ -1 * (s IDmain1_err) + -3 <= 0)%Z
    | 54%positive => (-1 * (s IDmain1_err) + -3 <= 0 /\ 1 * (s IDmain1_err) + 3 <= 0 /\ -1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 55%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0)%Z
    | 56%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 57%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0)%Z
    | 58%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 59%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ 1 * (s IDmain1_ch) + -47 <= 0)%Z
    | 60%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0)%Z
    | 61%positive => (-1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 62%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 58 <= 0)%Z
    | 63%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 64%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ 1 * (s IDmain1_ch) + -64 <= 0)%Z
    | 65%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 65 <= 0)%Z
    | 66%positive => (-1 * (s IDmain1_ch) + 65 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 67%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 71 <= 0)%Z
    | 68%positive => (1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 69%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ 1 * (s IDmain1_err) + 2 <= 0 /\ -1 * (s IDmain1_err) + -2 <= 0)%Z
    | 70%positive => (-1 * (s IDmain1_err) + -2 <= 0 /\ 1 * (s IDmain1_err) + 2 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 71%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 65 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0)%Z
    | 72%positive => (1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 65 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 73%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 65 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0)%Z
    | 74%positive => (1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 65 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 75%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -57 <= 0)%Z
    | 76%positive => (1 * (s IDmain1_ch) + -57 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 77%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -57 <= 0)%Z
    | 78%positive => (1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_i) + -63 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 79%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 1 <= 0)%Z
    | 80%positive => (-1 * (s IDmain1_i) + 1 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 81%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 1 <= 0)%Z
    | 82%positive => (-1 * (s IDmain1_i) + 1 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 83%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 1 <= 0)%Z
    | 84%positive => (-1 * (s IDmain1_i) + 1 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 85%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1__tmp) + -5 <= 0 /\ -1 * (s IDmain1__tmp) + 5 <= 0 /\ -1 * (s IDmain1_ch) + 48 <= 0 /\ 1 * (s IDmain1_ch) + -70 <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) + 1 <= 0 /\ -1 * (s IDmain1_z) + 1 <= 0)%Z
    | 86%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_err) <= 0)%Z
    | 87%positive => (-1 * (s IDmain1_err) <= 0 /\ 1 * (s IDmain1_err) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 88%positive => (-1 * (s IDmain1_key_len) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_err) + 1 <= 0 /\ -1 * (s IDmain1_err) + -1 <= 0)%Z
    | 89%positive => (-1 * (s IDmain1_err) + -1 <= 0 /\ 1 * (s IDmain1_err) + 1 <= 0 /\ -1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_by) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_key_len) <= 0 /\ -1 * (s IDmain1_key_len) <= 0)%Z
    | 90%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 91%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 92%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 93%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 94%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 95%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 96%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 97%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ 1 * (s IDmain1_i) + -64 <= 0)%Z
    | 98%positive => (1 * (s IDmain1_i) + -64 <= 0 /\ -1 * (s IDmain1_i) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | _ => False
  end.

Definition main1_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((64 # 1))%Q
    | 2%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 3%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 4%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 5%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 6%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 7%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 8%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 9%positive => ((s IDmain1_i) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 10%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 11%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 12%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 13%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 14%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 15%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 16%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 17%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 18%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 19%positive => ((64 # 1) - (s IDmain1_i) + max0((s IDmain1_z)))%Q
    | 20%positive => ((64 # 1) - (s IDmain1_i) + max0((s IDmain1_z)))%Q
    | 21%positive => ((64 # 1) - (s IDmain1_i) + max0((s IDmain1_z)))%Q
    | 22%positive => ((64 # 1) - (s IDmain1_i) + max0((s IDmain1_z)))%Q
    | 23%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 24%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 25%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 26%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 27%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 28%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 29%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 30%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 31%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 32%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 33%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 34%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 35%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 36%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 37%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 38%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 39%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 40%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 41%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 42%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 43%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 44%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 45%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 46%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 47%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i))
                      + (62 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 48%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 49%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 50%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 51%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 52%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 53%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 54%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z)
                      - max0(63 - (s IDmain1_i)))%Q
    | 55%positive => ((64 # 1) - (s IDmain1_i) + max0((s IDmain1_z)))%Q
    | 56%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 57%positive => ((64 # 1) - (s IDmain1_i) + (s IDmain1_z))%Q
    | 58%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 59%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 60%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 61%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 62%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 63%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 64%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 65%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 66%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 67%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 68%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(63 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 69%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(63 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 70%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(63 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 71%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 72%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 73%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 74%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 75%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 76%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 77%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 78%positive => ((1 # 1) + (s IDmain1_i) + (s IDmain1_z)
                      + max0(63 - (s IDmain1_i)) - max0((s IDmain1_i)))%Q
    | 79%positive => ((s IDmain1_i) + (s IDmain1_z)
                      - max0(-1 + (s IDmain1_i)) + max0(64 - (s IDmain1_i)))%Q
    | 80%positive => ((1 # 1) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 81%positive => ((1 # 1) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 82%positive => ((1 # 1) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 83%positive => ((1 # 1) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 84%positive => ((1 # 1) + (s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 85%positive => ((s IDmain1_z) + max0(64 - (s IDmain1_i)))%Q
    | 86%positive => ((s IDmain1_i) + (s IDmain1_z)
                      + max0(64 - (s IDmain1_i)))%Q
    | 87%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 88%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 89%positive => ((64 # 1) + (s IDmain1_z))%Q
    | 90%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 91%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 92%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 93%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 94%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 95%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 96%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 97%positive => ((64 # 63) + (s IDmain1_i) + (s IDmain1_z)
                      - (1 # 63) * max0(64 - (s IDmain1_i))
                      - (64 # 63) * max0((s IDmain1_i)))%Q
    | 98%positive => ((s IDmain1_z))%Q
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
    | 9%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (64
                                                                  - (s IDmain1_i))) (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_ge_0 (63 - (s IDmain1_i))]
    | 18%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain1_z)) (0))) (F_max0_ge_0 ((s IDmain1_z)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_ge_0 (63 - (s IDmain1_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_z))) (F_check_ge ((s IDmain1_z)) (0))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1.01587 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_i))) (F_check_ge ((s IDmain1_i)) (0));
                      (*0 0.984127*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDmain1_i)) (63
                                                                    - (s IDmain1_i)))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDmain1_i)) (63
                                                                    - (s IDmain1_i)))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDmain1_i)) (63
                                                                    - (s IDmain1_i)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 -9.1254e-11*) F_max0_monotonic (F_check_ge (64
                                                                    - (s IDmain1_i)) (63
                                                                    - (s IDmain1_i)));
                      (*-1.01587 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_i))) (F_check_ge ((s IDmain1_i)) (0));
                      (*-0.984127 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-1 -9.28486e-11*) F_max0_monotonic (F_check_ge (64
                                                                    - (s IDmain1_i)) (63
                                                                    - (s IDmain1_i)));
                      (*-1.01587 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_i))) (F_check_ge ((s IDmain1_i)) (0));
                      (*-0.984127 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 55%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_z))) (F_check_ge ((s IDmain1_z)) (0))]
    | 56%positive => []
    | 57%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain1_i))) (F_check_ge ((s IDmain1_i)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 58%positive => []
    | 59%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1)]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1)]
    | 63%positive => []
    | 64%positive => [(*0 0.015873*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain1_i))) (F_check_ge (0) (0));
                      (*-1.01587 0*) F_binom_monotonic 1 (F_max0_ge_0 (63
                                                                    - (s IDmain1_i))) (F_check_ge (0) (0))]
    | 65%positive => []
    | 66%positive => []
    | 67%positive => [(*-1.01587 0*) F_max0_ge_0 (63 - (s IDmain1_i));
                      (*-0.015873 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain1_i))) (F_check_ge (0) (0))]
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*0 0.015873*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1)]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*0 1*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1)]
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmain1_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmain1_i)))]
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (64
                                                                   - 
                                                                   (s IDmain1_i))) (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))]
    | 86%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (63
                                                                  - (s IDmain1_i))) (F_check_ge (63
                                                                    - (s IDmain1_i)) (0))]
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*-1.01587 0*) F_max0_pre_decrement (64 - (s IDmain1_i)) (1);
                      (*-1.01587 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain1_i))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)));
                      (*0 1.01587*) F_binom_monotonic 1 (F_max0_ge_0 (63
                                                                    - (s IDmain1_i))) (F_check_ge (0) (0))]
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => [(*-1.01587 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain1_i)) (0))) (F_max0_ge_0 ((s IDmain1_i)));
                      (*-0.015873 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 96%positive => []
    | 97%positive => [(*-1.01587 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain1_i)) (0))) (F_max0_ge_0 ((s IDmain1_i)));
                      (*-0.015873 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDmain1_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDmain1_i)))]
    | 98%positive => []
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

