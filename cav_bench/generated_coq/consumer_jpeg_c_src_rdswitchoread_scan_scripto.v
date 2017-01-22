Require Import pasta.Pasta.

Notation IDread_scan_script_z := 1%positive.
Notation IDread_scan_script__tmp := 2%positive.
Notation IDread_scan_script_ncomps := 3%positive.
Notation IDread_scan_script_scanno := 4%positive.
Notation IDread_scan_script_cinfo := 5%positive.
Notation IDread_scan_script_filename := 6%positive.
Definition read_scan_script : graph := {|
  g_start := 1%positive;
  g_end := 85%positive;
  g_edges := (1%positive,(AAssign IDread_scan_script_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,82%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDread_scan_script_scanno
             (Some (ENum (0)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,21%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,18%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_scanno) s) >
             (eval (ENum (0)) s))%Z)),13%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_scanno) s) <=
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,15%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (1)))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,85%positive)::
             (18%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (0)))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,85%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_scanno) s) >=
             (eval (ENum (100)) s))%Z)),78%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_scanno) s) <
             (eval (ENum (100)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDread_scan_script_ncomps
             (Some (ENum (1)))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,61%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,32%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,49%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (33%positive,ANone,60%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,60%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (37%positive,ANone,59%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,59%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (41%positive,ANone,58%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,58%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,47%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,66%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,50%positive)::
             (49%positive,ANone,52%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,57%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDread_scan_script_scanno
             (Some (EAdd (EVar IDread_scan_script_scanno) (ENum (1))))),
             53%positive)::(53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDread_scan_script_z
             (Some (EAdd (ENum (1)) (EVar IDread_scan_script_z)))),
             56%positive)::(56%positive,AWeaken,7%positive)::
             (57%positive,ANone,66%positive)::
             (58%positive,ANone,66%positive)::
             (59%positive,ANone,66%positive)::
             (60%positive,ANone,66%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_ncomps) s) >=
             (eval (ENum (4)) s))%Z)),74%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EVar IDread_scan_script_ncomps) s) <
             (eval (ENum (4)) s))%Z)),63%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,ANone,69%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (0)))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,85%positive)::
             (69%positive,(AAssign IDread_scan_script_ncomps
             (Some (EAdd (EVar IDread_scan_script_ncomps) (ENum (1))))),
             70%positive)::(70%positive,ANone,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,(AAssign IDread_scan_script_z
             (Some (EAdd (ENum (1)) (EVar IDread_scan_script_z)))),
             73%positive)::(73%positive,AWeaken,27%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (0)))),76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,AWeaken,85%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (0)))),80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,AWeaken,85%positive)::
             (82%positive,(AAssign IDread_scan_script__tmp
             (Some (ENum (0)))),83%positive)::
             (83%positive,ANone,84%positive)::
             (84%positive,AWeaken,85%positive)::nil
|}.

Definition read_scan_script_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 6%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 7%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 8%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 9%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 10%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 11%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 12%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 13%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDread_scan_script_scanno) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 15%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 16%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script__tmp) + -1 <= 0 /\ -1 * (s IDread_scan_script__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDread_scan_script__tmp) + 1 <= 0 /\ 1 * (s IDread_scan_script__tmp) + -1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 18%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 19%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 21%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 22%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 23%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_scanno) + -99 <= 0)%Z
    | 24%positive => (1 * (s IDread_scan_script_scanno) + -99 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 25%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_scanno) + -99 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -1 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -1 <= 0 /\ 1 * (s IDread_scan_script_scanno) + -99 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 27%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 28%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 29%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 30%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 31%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 32%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 33%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 34%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 35%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 37%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 39%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 40%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 41%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 42%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 43%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 44%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 45%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 46%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 47%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 48%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 49%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 50%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 51%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 52%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 53%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDread_scan_script_scanno) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 55%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDread_scan_script_scanno) + 1 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_z) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 58%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 59%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 60%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 61%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 62%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 63%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -3 <= 0)%Z
    | 64%positive => (1 * (s IDread_scan_script_ncomps) + -3 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 65%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -3 <= 0)%Z
    | 66%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 67%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | 68%positive => (-1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0)%Z
    | 69%positive => (-1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -3 <= 0)%Z
    | 70%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 2 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 71%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 2 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 72%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 2 <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 73%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 2 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) + 1 <= 0)%Z
    | 74%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 4 <= 0)%Z
    | 75%positive => (-1 * (s IDread_scan_script_ncomps) + 4 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 76%positive => (1 * (s IDread_scan_script_ncomps) + -4 <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 4 <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | 77%positive => (-1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script_ncomps) + 4 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) <= 0 /\ 1 * (s IDread_scan_script_ncomps) + -4 <= 0)%Z
    | 78%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 100 <= 0)%Z
    | 79%positive => (-1 * (s IDread_scan_script_scanno) + 100 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 80%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 100 <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | 81%positive => (-1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script_scanno) + 100 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 82%positive => (1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 83%positive => (-1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script_z) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | 84%positive => (-1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script__tmp) <= 0 /\ 1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script_z) <= 0)%Z
    | 85%positive => (1 * (s IDread_scan_script__tmp) + -1 <= 0 /\ -1 * (s IDread_scan_script_z) <= 0 /\ -1 * (s IDread_scan_script__tmp) <= 0)%Z
    | _ => False
  end.

Definition read_scan_script_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((400 # 1))%Q
    | 2%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 3%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 4%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 5%positive => ((s IDread_scan_script_z)
                     + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 6%positive => ((s IDread_scan_script_z)
                     + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 7%positive => ((s IDread_scan_script_z)
                     + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 8%positive => ((s IDread_scan_script_z)
                     + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 9%positive => ((s IDread_scan_script_z))%Q
    | 10%positive => ((s IDread_scan_script_z))%Q
    | 11%positive => ((s IDread_scan_script_z))%Q
    | 12%positive => ((s IDread_scan_script_z))%Q
    | 13%positive => ((s IDread_scan_script_z))%Q
    | 14%positive => ((s IDread_scan_script_z))%Q
    | 15%positive => ((s IDread_scan_script_z))%Q
    | 16%positive => ((s IDread_scan_script_z))%Q
    | 17%positive => ((s IDread_scan_script_z))%Q
    | 18%positive => ((s IDread_scan_script_z))%Q
    | 19%positive => ((s IDread_scan_script_z))%Q
    | 20%positive => ((s IDread_scan_script_z))%Q
    | 21%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 22%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 23%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 24%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 25%positive => ((1 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      + (s IDread_scan_script_z)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 26%positive => ((1 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      + (s IDread_scan_script_z)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 27%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 28%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 29%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 30%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 31%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 32%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 33%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 34%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 35%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 36%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 37%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 38%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 39%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 40%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 41%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 42%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 43%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 44%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 45%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 46%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 47%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 48%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 49%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 50%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 51%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 52%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 53%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 54%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 55%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 56%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + max0(-1 + (s IDread_scan_script_z))
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 57%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 58%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 59%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 60%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 61%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 62%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 63%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 64%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 65%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 66%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 67%positive => ((7 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + max0(1 - (s IDread_scan_script__tmp))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 68%positive => ((7 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + max0(1 - (s IDread_scan_script__tmp))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 69%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 70%positive => ((5 # 1) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-2 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 71%positive => ((5 # 1) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-2 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 72%positive => ((5 # 1) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-2 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 73%positive => ((5 # 1) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-2 + (s IDread_scan_script_ncomps))
                      + max0(-1 + (s IDread_scan_script_z))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno)))%Q
    | 74%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 75%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 76%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 77%positive => ((9 # 2) - (1 # 2) * (s IDread_scan_script_ncomps)
                      - (1 # 2) * max0(-1 + (s IDread_scan_script_ncomps))
                      + (4 # 1) * max0(99 - (s IDread_scan_script_scanno))
                      + max0((s IDread_scan_script_z)))%Q
    | 78%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 79%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 80%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 81%positive => ((s IDread_scan_script_z)
                      + (4 # 1) * max0(100 - (s IDread_scan_script_scanno)))%Q
    | 82%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 83%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 84%positive => ((400 # 1) + (s IDread_scan_script_z))%Q
    | 85%positive => ((s IDread_scan_script_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_scan_script_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-4 0*) F_max0_ge_0 (100
                                           - (s IDread_scan_script_scanno))]
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
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-4 0*) F_max0_pre_decrement (100
                                                     - (s IDread_scan_script_scanno)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_scan_script_z)) (0))) (F_max0_ge_0 ((s IDread_scan_script_z)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
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
    | 56%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                 - (s IDread_scan_script_ncomps))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDread_scan_script_ncomps)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDread_scan_script_z))) (F_check_ge (-1
                                                                    + (s IDread_scan_script_z)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDread_scan_script_ncomps)))]
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
    | 68%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDread_scan_script_ncomps)) (3
                                                                    - (s IDread_scan_script_ncomps)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDread_scan_script_ncomps));
                      (*-4 0*) F_max0_ge_0 (99
                                            - (s IDread_scan_script_scanno));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_scan_script_z))) (F_check_ge ((s IDread_scan_script_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDread_scan_script_ncomps)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDread_scan_script__tmp))) (F_check_ge (0) (0));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDread_scan_script_ncomps)))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_scan_script_z)) (0))) (F_max0_ge_0 ((s IDread_scan_script_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDread_scan_script_z))) (F_check_ge (-1
                                                                    + (s IDread_scan_script_z)) (0));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDread_scan_script_ncomps))) (F_check_ge (-1
                                                                    + (s IDread_scan_script_ncomps)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDread_scan_script_ncomps)))]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDread_scan_script_ncomps)) (3
                                                                    - (s IDread_scan_script_ncomps)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDread_scan_script_ncomps));
                      (*-4 0*) F_max0_ge_0 (99
                                            - (s IDread_scan_script_scanno));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_scan_script_z))) (F_check_ge ((s IDread_scan_script_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDread_scan_script_ncomps)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDread_scan_script_ncomps)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDread_scan_script_ncomps)))]
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (100
                                                             - (s IDread_scan_script_scanno)) (99
                                                                    - (s IDread_scan_script_scanno)));
                      (*-4 0*) F_max0_ge_0 (99
                                            - (s IDread_scan_script_scanno))]
    | 82%positive => []
    | 83%positive => []
    | 84%positive => [(*-400 0*) F_one]
    | 85%positive => []
    | _ => []
  end.


Theorem read_scan_script_ai_correct:
  forall s p' s', steps (g_start read_scan_script) s (g_edges read_scan_script) p' s' -> read_scan_script_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_scan_script_pot_correct:
  forall s p' s',
    steps (g_start read_scan_script) s (g_edges read_scan_script) p' s' ->
    (read_scan_script_pot (g_start read_scan_script) s >= read_scan_script_pot p' s')%Q.
Proof.
  check_lp read_scan_script_ai_correct read_scan_script_hints.
Qed.

