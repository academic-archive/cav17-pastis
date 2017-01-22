Require Import pasta.Pasta.

Notation IDmakepossibilities_z := 1%positive.
Notation IDmakepossibilities_compoundflag := 2%positive.
Notation IDmakepossibilities_easypossibilities := 3%positive.
Notation IDmakepossibilities_i := 4%positive.
Notation IDmakepossibilities_maxposslen := 5%positive.
Notation IDmakepossibilities_pcount := 6%positive.
Notation IDmakepossibilities_sortit := 7%positive.
Notation IDmakepossibilities_tryhardflag := 8%positive.
Notation IDmakepossibilities_word := 9%positive.
Definition makepossibilities : graph := {|
  g_start := 1%positive;
  g_end := 71%positive;
  g_edges := (1%positive,(AAssign IDmakepossibilities_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmakepossibilities_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_i) s) <
             (eval (ENum (100)) s))%Z)),72%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_i) s) >=
             (eval (ENum (100)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDmakepossibilities_pcount
             (Some (ENum (0)))),8%positive)::
             (8%positive,(AAssign IDmakepossibilities_maxposslen
             (Some (ENum (0)))),9%positive)::
             (9%positive,(AAssign IDmakepossibilities_easypossibilities
             (Some (ENum (0)))),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <
             (eval (ENum (100)) s))%Z)),13%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >=
             (eval (ENum (100)) s))%Z)),12%positive)::
             (12%positive,AWeaken,16%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <
             (eval (ENum (100)) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >=
             (eval (ENum (100)) s))%Z)),17%positive)::
             (17%positive,AWeaken,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <
             (eval (ENum (100)) s))%Z)),23%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >=
             (eval (ENum (100)) s))%Z)),22%positive)::
             (22%positive,AWeaken,26%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <
             (eval (ENum (100)) s))%Z)),28%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >=
             (eval (ENum (100)) s))%Z)),27%positive)::
             (27%positive,AWeaken,31%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_compoundflag) s) <>
             (eval (ENum (1)) s))%Z)),33%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_compoundflag) s) =
             (eval (ENum (1)) s))%Z)),32%positive)::
             (32%positive,AWeaken,38%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <
             (eval (ENum (100)) s))%Z)),36%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >=
             (eval (ENum (100)) s))%Z)),35%positive)::
             (35%positive,AWeaken,38%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDmakepossibilities_easypossibilities
             (Some (EVar IDmakepossibilities_pcount))),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_easypossibilities)
             s) = (eval (ENum (0)) s))%Z)),45%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_easypossibilities)
             s) <> (eval (ENum (0)) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_tryhardflag) s) <>
             (eval (ENum (0)) s))%Z)),44%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_tryhardflag) s) =
             (eval (ENum (0)) s))%Z)),43%positive)::
             (43%positive,AWeaken,48%positive)::
             (44%positive,AWeaken,46%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_sortit) s) <>
             (eval (ENum (0)) s))%Z)),53%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_sortit) s) =
             (eval (ENum (0)) s))%Z)),49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >
             (eval (EVar IDmakepossibilities_easypossibilities) s))%Z)),
             52%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <=
             (eval (EVar IDmakepossibilities_easypossibilities) s))%Z)),
             51%positive)::(51%positive,AWeaken,71%positive)::
             (52%positive,AWeaken,54%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <>
             (eval (ENum (0)) s))%Z)),56%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) =
             (eval (ENum (0)) s))%Z)),55%positive)::
             (55%positive,AWeaken,71%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_easypossibilities)
             s) > (eval (ENum (0)) s))%Z)),59%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_easypossibilities)
             s) <= (eval (ENum (0)) s))%Z)),58%positive)::
             (58%positive,AWeaken,65%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_sortit) s) <>
             (eval (ENum (0)) s))%Z)),62%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_sortit) s) =
             (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,65%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) >
             (eval (EVar IDmakepossibilities_easypossibilities) s))%Z)),
             67%positive)::
             (65%positive,(AGuard
             (fun s => ((eval (EVar IDmakepossibilities_pcount) s) <=
             (eval (EVar IDmakepossibilities_easypossibilities) s))%Z)),
             66%positive)::(66%positive,AWeaken,69%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,AWeaken,71%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDmakepossibilities_i
             (Some (EAdd (EVar IDmakepossibilities_i) (ENum (1))))),
             75%positive)::(75%positive,ANone,76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDmakepossibilities_z
             (Some (EAdd (ENum (1)) (EVar IDmakepossibilities_z)))),
             78%positive)::(78%positive,AWeaken,5%positive)::nil
|}.

Definition makepossibilities_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) <= 0 /\ -1 * (s IDmakepossibilities_i) <= 0)%Z
    | 4%positive => (-1 * (s IDmakepossibilities_i) <= 0 /\ 1 * (s IDmakepossibilities_i) <= 0 /\ 1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0)%Z
    | 6%positive => (1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0)%Z
    | 7%positive => (-1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0)%Z
    | 8%positive => (1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 9%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 10%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 11%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 12%positive => (False)%Z
    | 13%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 14%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 15%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 16%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 17%positive => (False)%Z
    | 18%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 19%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 20%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 21%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 22%positive => (False)%Z
    | 23%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 24%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 25%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 26%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 27%positive => (False)%Z
    | 28%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 29%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 30%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 31%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 32%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_compoundflag) + -1 <= 0 /\ -1 * (s IDmakepossibilities_compoundflag) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 34%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 35%positive => (False)%Z
    | 36%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 37%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0)%Z
    | 38%positive => (-1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 39%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 40%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 41%positive => (False)%Z
    | 42%positive => (False)%Z
    | 43%positive => (False)%Z
    | 44%positive => (False)%Z
    | 45%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 46%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 47%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 48%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 49%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_sortit) <= 0 /\ -1 * (s IDmakepossibilities_sortit) <= 0)%Z
    | 50%positive => (-1 * (s IDmakepossibilities_sortit) <= 0 /\ 1 * (s IDmakepossibilities_sortit) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 51%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_sortit) <= 0 /\ -1 * (s IDmakepossibilities_sortit) <= 0)%Z
    | 52%positive => (False)%Z
    | 53%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 54%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 55%positive => (-1 * (s IDmakepossibilities_pcount) <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_easypossibilities) <= 0)%Z
    | 56%positive => (False)%Z
    | 57%positive => (False)%Z
    | 58%positive => (False)%Z
    | 59%positive => (False)%Z
    | 60%positive => (False)%Z
    | 61%positive => (False)%Z
    | 62%positive => (False)%Z
    | 63%positive => (False)%Z
    | 64%positive => (False)%Z
    | 65%positive => (False)%Z
    | 66%positive => (False)%Z
    | 67%positive => (False)%Z
    | 68%positive => (False)%Z
    | 69%positive => (False)%Z
    | 70%positive => (False)%Z
    | 71%positive => (-1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ 1 * (s IDmakepossibilities_easypossibilities) <= 0 /\ -1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_maxposslen) <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 100 <= 0 /\ 1 * (s IDmakepossibilities_pcount) <= 0 /\ -1 * (s IDmakepossibilities_pcount) <= 0)%Z
    | 72%positive => (-1 * (s IDmakepossibilities_i) <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -99 <= 0)%Z
    | 73%positive => (1 * (s IDmakepossibilities_i) + -99 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) <= 0)%Z
    | 74%positive => (-1 * (s IDmakepossibilities_i) <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0 /\ 1 * (s IDmakepossibilities_i) + -99 <= 0)%Z
    | 75%positive => (-1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 1 <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0)%Z
    | 76%positive => (1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_i) + 1 <= 0 /\ -1 * (s IDmakepossibilities_z) <= 0)%Z
    | 77%positive => (-1 * (s IDmakepossibilities_z) <= 0 /\ -1 * (s IDmakepossibilities_i) + 1 <= 0 /\ 1 * (s IDmakepossibilities_i) + -100 <= 0)%Z
    | 78%positive => (1 * (s IDmakepossibilities_i) + -100 <= 0 /\ -1 * (s IDmakepossibilities_i) + 1 <= 0 /\ -1 * (s IDmakepossibilities_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition makepossibilities_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((100 # 1))%Q
    | 2%positive => ((100 # 1) + (s IDmakepossibilities_z))%Q
    | 3%positive => ((100 # 1) - (s IDmakepossibilities_i)
                     + (s IDmakepossibilities_z))%Q
    | 4%positive => ((100 # 1) - (s IDmakepossibilities_i)
                     + (s IDmakepossibilities_z))%Q
    | 5%positive => ((100 # 1) - (s IDmakepossibilities_i)
                     + (s IDmakepossibilities_z))%Q
    | 6%positive => ((100 # 1) - (s IDmakepossibilities_i)
                     + (s IDmakepossibilities_z))%Q
    | 7%positive => (-(1 # 1) + (s IDmakepossibilities_z)
                     + (1 # 99) * max0(-1 + (s IDmakepossibilities_i))
                     + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 8%positive => (-(1 # 1) + (s IDmakepossibilities_z)
                     + (1 # 99) * max0(-1 + (s IDmakepossibilities_i))
                     + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 9%positive => (-(1 # 1) + (s IDmakepossibilities_z)
                     + (1 # 99) * max0(-1 + (s IDmakepossibilities_i))
                     + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 10%positive => (-(1 # 1) + (s IDmakepossibilities_z)
                      + (1 # 99) * max0(-1 + (s IDmakepossibilities_i))
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 11%positive => (-(100 # 99) + (1 # 99) * (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z)
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 12%positive => (-(100 # 99) + (1 # 99) * (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z)
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 13%positive => (-(100 # 99) + (1 # 99) * (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z)
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 14%positive => ((s IDmakepossibilities_z)
                      + (1 # 99) * max0(-100 + (s IDmakepossibilities_i))
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 15%positive => ((s IDmakepossibilities_z)
                      + (1 # 99) * max0(-100 + (s IDmakepossibilities_i))
                      + (100 # 99) * max0(100 - (s IDmakepossibilities_i)))%Q
    | 16%positive => ((s IDmakepossibilities_z))%Q
    | 17%positive => ((s IDmakepossibilities_z))%Q
    | 18%positive => ((s IDmakepossibilities_z))%Q
    | 19%positive => ((s IDmakepossibilities_z))%Q
    | 20%positive => ((s IDmakepossibilities_z))%Q
    | 21%positive => ((s IDmakepossibilities_z))%Q
    | 22%positive => ((s IDmakepossibilities_z))%Q
    | 23%positive => ((s IDmakepossibilities_z))%Q
    | 24%positive => ((s IDmakepossibilities_z))%Q
    | 25%positive => ((s IDmakepossibilities_z))%Q
    | 26%positive => ((s IDmakepossibilities_z))%Q
    | 27%positive => ((s IDmakepossibilities_z))%Q
    | 28%positive => ((s IDmakepossibilities_z))%Q
    | 29%positive => ((s IDmakepossibilities_z))%Q
    | 30%positive => ((s IDmakepossibilities_z))%Q
    | 31%positive => ((s IDmakepossibilities_z))%Q
    | 32%positive => ((s IDmakepossibilities_z))%Q
    | 33%positive => ((s IDmakepossibilities_z))%Q
    | 34%positive => ((s IDmakepossibilities_z))%Q
    | 35%positive => ((s IDmakepossibilities_z))%Q
    | 36%positive => ((s IDmakepossibilities_z))%Q
    | 37%positive => ((s IDmakepossibilities_z))%Q
    | 38%positive => ((s IDmakepossibilities_z))%Q
    | 39%positive => ((s IDmakepossibilities_z))%Q
    | 40%positive => ((s IDmakepossibilities_z))%Q
    | 41%positive => ((s IDmakepossibilities_z))%Q
    | 42%positive => ((s IDmakepossibilities_z))%Q
    | 43%positive => ((s IDmakepossibilities_z))%Q
    | 44%positive => ((s IDmakepossibilities_z))%Q
    | 45%positive => ((s IDmakepossibilities_z))%Q
    | 46%positive => ((s IDmakepossibilities_z))%Q
    | 47%positive => ((s IDmakepossibilities_z))%Q
    | 48%positive => ((s IDmakepossibilities_z))%Q
    | 49%positive => ((s IDmakepossibilities_z))%Q
    | 50%positive => ((s IDmakepossibilities_z))%Q
    | 51%positive => ((s IDmakepossibilities_z))%Q
    | 52%positive => ((s IDmakepossibilities_z))%Q
    | 53%positive => ((s IDmakepossibilities_z))%Q
    | 54%positive => ((s IDmakepossibilities_z))%Q
    | 55%positive => ((s IDmakepossibilities_z))%Q
    | 56%positive => ((s IDmakepossibilities_z))%Q
    | 57%positive => ((s IDmakepossibilities_z))%Q
    | 58%positive => ((s IDmakepossibilities_z))%Q
    | 59%positive => ((s IDmakepossibilities_z))%Q
    | 60%positive => ((s IDmakepossibilities_z))%Q
    | 61%positive => ((s IDmakepossibilities_z))%Q
    | 62%positive => ((s IDmakepossibilities_z))%Q
    | 63%positive => ((s IDmakepossibilities_z))%Q
    | 64%positive => ((s IDmakepossibilities_z))%Q
    | 65%positive => ((s IDmakepossibilities_z))%Q
    | 66%positive => ((s IDmakepossibilities_z))%Q
    | 67%positive => ((s IDmakepossibilities_z))%Q
    | 68%positive => ((s IDmakepossibilities_z))%Q
    | 69%positive => ((s IDmakepossibilities_z))%Q
    | 70%positive => ((s IDmakepossibilities_z))%Q
    | 71%positive => ((s IDmakepossibilities_z))%Q
    | 72%positive => ((100 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 73%positive => ((100 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 74%positive => ((100 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 75%positive => ((101 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 76%positive => ((101 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 77%positive => ((101 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | 78%positive => ((100 # 1) - (s IDmakepossibilities_i)
                      + (s IDmakepossibilities_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition makepossibilities_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 1.0101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                                    - (s IDmakepossibilities_i)) (0))) (F_max0_ge_0 (100
                                                                    - (s IDmakepossibilities_i)));
                     (*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmakepossibilities_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmakepossibilities_i)))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDmakepossibilities_i))) (F_check_ge (-1
                                                                    + (s IDmakepossibilities_i)) (0))]
    | 11%positive => []
    | 12%positive => [(*-1.0101 0*) F_max0_pre_decrement (100
                                                          - (s IDmakepossibilities_i)) (1);
                      (*-1.0101 0*) F_max0_ge_0 (99
                                                 - (s IDmakepossibilities_i));
                      (*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmakepossibilities_i))) (F_check_ge (0) (0));
                      (*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmakepossibilities_i)) (0))) (F_max0_ge_0 ((s IDmakepossibilities_i)))]
    | 13%positive => [(*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDmakepossibilities_i)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDmakepossibilities_i)))]
    | 14%positive => []
    | 15%positive => [(*-1.0101 0*) F_max0_monotonic (F_check_ge (100
                                                                  - (s IDmakepossibilities_i)) (99
                                                                    - (s IDmakepossibilities_i)));
                      (*-1.0101 0*) F_max0_ge_0 (99
                                                 - (s IDmakepossibilities_i));
                      (*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDmakepossibilities_i))) (F_check_ge (0) (0))]
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
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | _ => []
  end.


Theorem makepossibilities_ai_correct:
  forall s p' s', steps (g_start makepossibilities) s (g_edges makepossibilities) p' s' -> makepossibilities_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem makepossibilities_pot_correct:
  forall s p' s',
    steps (g_start makepossibilities) s (g_edges makepossibilities) p' s' ->
    (makepossibilities_pot (g_start makepossibilities) s >= makepossibilities_pot p' s')%Q.
Proof.
  check_lp makepossibilities_ai_correct makepossibilities_hints.
Qed.

