Require Import pasta.Pasta.

Notation IDBZ2_bzReadOpen_z := 1%positive.
Notation IDBZ2_bzReadOpen__tmp := 2%positive.
Notation IDBZ2_bzReadOpen__tmp1 := 3%positive.
Notation IDBZ2_bzReadOpen__tmp2 := 4%positive.
Notation IDBZ2_bzReadOpen_ret := 5%positive.
Notation IDBZ2_bzReadOpen_bzerror := 6%positive.
Notation IDBZ2_bzReadOpen_f := 7%positive.
Notation IDBZ2_bzReadOpen_nUnused := 8%positive.
Notation IDBZ2_bzReadOpen_small := 9%positive.
Notation IDBZ2_bzReadOpen_unused := 10%positive.
Notation IDBZ2_bzReadOpen_verbosity := 11%positive.
Definition BZ2_bzReadOpen : graph := {|
  g_start := 1%positive;
  g_end := 105%positive;
  g_edges := (1%positive,(AAssign IDBZ2_bzReadOpen_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDBZ2_bzReadOpen__tmp1
             (Some (EVar IDBZ2_bzReadOpen_verbosity))),3%positive)::
             (3%positive,(AAssign IDBZ2_bzReadOpen__tmp
             (Some (EVar IDBZ2_bzReadOpen_small))),4%positive)::
             (4%positive,(AAssign IDBZ2_bzReadOpen__tmp2
             (Some (EVar IDBZ2_bzReadOpen_nUnused))),5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,8%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,AWeaken,10%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,12%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,14%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,96%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp) s) <>
             (eval (ENum (0)) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp) s) =
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp) s) <>
             (eval (ENum (1)) s))%Z)),95%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp) s) =
             (eval (ENum (1)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp1) s) <
             (eval (ENum (0)) s))%Z)),94%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp1) s) >
             (eval (ENum (4)) s))%Z)),93%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp1) s) <=
             (eval (ENum (4)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,27%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,30%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) <>
             (eval (ENum (0)) s))%Z)),92%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) =
             (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,32%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,37%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) <
             (eval (ENum (0)) s))%Z)),91%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) >=
             (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) >
             (eval (ENum (5000)) s))%Z)),90%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) <=
             (eval (ENum (5000)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,81%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,72%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,43%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,45%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,46%positive)::
             (45%positive,ANone,47%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) >
             (eval (ENum (0)) s))%Z)),66%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen__tmp2) s) <=
             (eval (ENum (0)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AAssign IDBZ2_bzReadOpen_ret None),52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen_ret) s) <>
             (eval (ENum (0)) s))%Z)),57%positive)::
             (53%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_bzReadOpen_ret) s) =
             (eval (ENum (0)) s))%Z)),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,105%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,ANone,60%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,62%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,63%positive)::
             (62%positive,ANone,64%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,AWeaken,105%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,(AAssign IDBZ2_bzReadOpen__tmp2
             (Some (EAdd (EVar IDBZ2_bzReadOpen__tmp2) (ENum (-1))))),
             68%positive)::(68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDBZ2_bzReadOpen_z (Some (EAdd (ENum (1))
             (EVar IDBZ2_bzReadOpen_z)))),71%positive)::
             (71%positive,AWeaken,49%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,75%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,AWeaken,77%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,ANone,78%positive)::
             (77%positive,ANone,79%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,AWeaken,105%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,ANone,84%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,AWeaken,86%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,ANone,87%positive)::
             (86%positive,ANone,88%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,AWeaken,105%positive)::
             (90%positive,AWeaken,97%positive)::
             (91%positive,AWeaken,97%positive)::
             (92%positive,AWeaken,97%positive)::
             (93%positive,AWeaken,97%positive)::
             (94%positive,AWeaken,97%positive)::
             (95%positive,AWeaken,97%positive)::
             (96%positive,AWeaken,97%positive)::
             (97%positive,ANone,99%positive)::
             (97%positive,ANone,98%positive)::
             (98%positive,AWeaken,101%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,AWeaken,101%positive)::
             (101%positive,ANone,102%positive)::
             (101%positive,ANone,103%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,ANone,104%positive)::
             (104%positive,AWeaken,105%positive)::nil
|}.

Definition BZ2_bzReadOpen_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 4%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 5%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 6%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 7%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 8%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 9%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 10%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 11%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 12%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 13%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 14%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 15%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 16%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 17%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 19%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 20%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 22%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0)%Z
    | 23%positive => (-1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 24%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 25%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 26%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 27%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 28%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 29%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 30%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 31%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 32%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 34%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 35%positive => (-1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 36%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) + -5000 <= 0)%Z
    | 37%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 38%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 39%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 40%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 41%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 42%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 43%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 44%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 45%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 46%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 47%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 48%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 49%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 50%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 51%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 52%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 53%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 54%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_ret) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_ret) <= 0)%Z
    | 55%positive => (-1 * (s IDBZ2_bzReadOpen_ret) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_ret) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 56%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_ret) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_ret) <= 0)%Z
    | 57%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 58%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 59%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 60%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 61%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 62%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 63%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 64%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 65%positive => (1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 66%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDBZ2_bzReadOpen__tmp2) + 1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 68%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 69%positive => (-1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0)%Z
    | 70%positive => (1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) <= 0)%Z
    | 71%positive => (-1 * (s IDBZ2_bzReadOpen__tmp2) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 73%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 74%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 75%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 76%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 77%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 78%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 79%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 80%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 81%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 82%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 83%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 84%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 85%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 86%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 87%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 88%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 89%positive => (1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 90%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp2) + 5001 <= 0)%Z
    | 91%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp2) + 1 <= 0)%Z
    | 92%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + -4 <= 0)%Z
    | 93%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp1) + 5 <= 0)%Z
    | 94%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp) + -1 <= 0 /\ -1 * (s IDBZ2_bzReadOpen__tmp) <= 0 /\ 1 * (s IDBZ2_bzReadOpen__tmp1) + 1 <= 0)%Z
    | 95%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 96%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 97%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 98%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 99%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 100%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 101%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 102%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 103%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ 1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 104%positive => (1 * (s IDBZ2_bzReadOpen_z) <= 0 /\ -1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | 105%positive => (-1 * (s IDBZ2_bzReadOpen_z) <= 0)%Z
    | _ => False
  end.

Definition BZ2_bzReadOpen_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDBZ2_bzReadOpen_nUnused)))%Q
    | 2%positive => (max0((s IDBZ2_bzReadOpen_nUnused)))%Q
    | 3%positive => (max0((s IDBZ2_bzReadOpen_nUnused)))%Q
    | 4%positive => (max0((s IDBZ2_bzReadOpen_nUnused)))%Q
    | 5%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 6%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 7%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 8%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 9%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 10%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 11%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 12%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 13%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 14%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 15%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 16%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 17%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 18%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 19%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 20%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 21%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 22%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 23%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 24%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 25%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 26%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 27%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 28%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 29%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 30%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 31%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 32%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 33%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 34%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 35%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 36%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 37%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 38%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 39%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 40%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 41%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 42%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 43%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 44%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 45%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 46%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 47%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 48%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 49%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 50%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 51%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 52%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 53%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 54%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 55%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 56%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 57%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 58%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 59%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 60%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 61%positive => ((s IDBZ2_bzReadOpen_z)
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 62%positive => ((s IDBZ2_bzReadOpen_z))%Q
    | 63%positive => ((s IDBZ2_bzReadOpen_z))%Q
    | 64%positive => ((s IDBZ2_bzReadOpen_z))%Q
    | 65%positive => ((s IDBZ2_bzReadOpen_z))%Q
    | 66%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 67%positive => ((1 # 1) + max0(-1 + (s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 68%positive => ((1 # 1) + max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 69%positive => ((1 # 1) + max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 70%positive => ((1 # 1) + max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 71%positive => ((1 # 1) + max0(-1 + (s IDBZ2_bzReadOpen_z))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 72%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 73%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 74%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 75%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 76%positive => (max0((s IDBZ2_bzReadOpen__tmp2))
                      + max0((s IDBZ2_bzReadOpen_z)))%Q
    | 77%positive => (max0((s IDBZ2_bzReadOpen_z)))%Q
    | 78%positive => (max0((s IDBZ2_bzReadOpen_z)))%Q
    | 79%positive => (max0((s IDBZ2_bzReadOpen_z)))%Q
    | 80%positive => (max0((s IDBZ2_bzReadOpen_z)))%Q
    | 81%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 82%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 83%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 84%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 85%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 86%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 87%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 88%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 89%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 90%positive => (-(s IDBZ2_bzReadOpen__tmp)
                      + max0((s IDBZ2_bzReadOpen__tmp))
                      + max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 91%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 92%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 93%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 94%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 95%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 96%positive => (max0((s IDBZ2_bzReadOpen__tmp2)))%Q
    | 97%positive => (0)%Q
    | 98%positive => (0)%Q
    | 99%positive => (0)%Q
    | 100%positive => (0)%Q
    | 101%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 102%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 103%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 104%positive => (-max0(-(s IDBZ2_bzReadOpen_z)))%Q
    | 105%positive => ((s IDBZ2_bzReadOpen_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition BZ2_bzReadOpen_hints (p : node) (s : state) := 
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
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBZ2_bzReadOpen__tmp)) (0))) (F_max0_ge_0 ((s IDBZ2_bzReadOpen__tmp)))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBZ2_bzReadOpen__tmp)) (0))) (F_max0_ge_0 ((s IDBZ2_bzReadOpen__tmp)))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBZ2_bzReadOpen_z)) (0))) (F_max0_ge_0 ((s IDBZ2_bzReadOpen_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBZ2_bzReadOpen_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDBZ2_bzReadOpen_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDBZ2_bzReadOpen_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDBZ2_bzReadOpen__tmp))) (F_check_ge ((s IDBZ2_bzReadOpen__tmp)) (0))]
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
    | 52%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDBZ2_bzReadOpen_z))) (F_check_ge ((s IDBZ2_bzReadOpen_z)) (0))]
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_max0_pre_decrement ((s IDBZ2_bzReadOpen__tmp2)) (1)]
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBZ2_bzReadOpen_z)) (0))) (F_max0_ge_0 ((s IDBZ2_bzReadOpen_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDBZ2_bzReadOpen_z))) (F_check_ge (-1
                                                                    + (s IDBZ2_bzReadOpen_z)) (0))]
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 75%positive => []
    | 76%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDBZ2_bzReadOpen_z))) (F_check_ge ((s IDBZ2_bzReadOpen_z)) (0))]
    | 81%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDBZ2_bzReadOpen__tmp))) (F_check_ge ((s IDBZ2_bzReadOpen__tmp)) (0))]
    | 82%positive => []
    | 83%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBZ2_bzReadOpen_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBZ2_bzReadOpen_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDBZ2_bzReadOpen__tmp2))) (F_check_ge (0) (0))]
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDBZ2_bzReadOpen_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDBZ2_bzReadOpen_z)))]
    | 90%positive => [(*-1 0*) F_max0_pre_decrement ((s IDBZ2_bzReadOpen__tmp2)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_bzReadOpen__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDBZ2_bzReadOpen__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDBZ2_bzReadOpen__tmp)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDBZ2_bzReadOpen__tmp)))]
    | 91%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 92%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 93%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 94%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 95%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 96%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDBZ2_bzReadOpen__tmp2)) (-1
                                                                    + (s IDBZ2_bzReadOpen__tmp2)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDBZ2_bzReadOpen__tmp2))]
    | 97%positive => []
    | 98%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBZ2_bzReadOpen_z))) (F_check_ge (0) (0))]
    | 99%positive => []
    | 100%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBZ2_bzReadOpen_z))) (F_check_ge (0) (0))]
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDBZ2_bzReadOpen_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDBZ2_bzReadOpen_z)))]
    | 105%positive => []
    | _ => []
  end.


Theorem BZ2_bzReadOpen_ai_correct:
  forall s p' s', steps (g_start BZ2_bzReadOpen) s (g_edges BZ2_bzReadOpen) p' s' -> BZ2_bzReadOpen_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BZ2_bzReadOpen_pot_correct:
  forall s p' s',
    steps (g_start BZ2_bzReadOpen) s (g_edges BZ2_bzReadOpen) p' s' ->
    (BZ2_bzReadOpen_pot (g_start BZ2_bzReadOpen) s >= BZ2_bzReadOpen_pot p' s')%Q.
Proof.
  check_lp BZ2_bzReadOpen_ai_correct BZ2_bzReadOpen_hints.
Qed.

