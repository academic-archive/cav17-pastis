Require Import pasta.Pasta.

Notation IDfill_window_z := 1%positive.
Notation IDfill_window_block_start := 2%positive.
Notation IDfill_window_eofile := 3%positive.
Notation IDfill_window_lookahead := 4%positive.
Notation IDfill_window_m := 5%positive.
Notation IDfill_window_match_start := 6%positive.
Notation IDfill_window_more := 7%positive.
Notation IDfill_window_n := 8%positive.
Notation IDfill_window_strstart := 9%positive.
Definition fill_window : graph := {|
  g_start := 1%positive;
  g_end := 72%positive;
  g_edges := (1%positive,(AAssign IDfill_window_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDfill_window_more)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDfill_window_m)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDfill_window_more None),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDfill_window_more)
             s) = (eval (ENum (-1)) s))%Z)),55%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDfill_window_more)
             s) <> (eval (ENum (-1)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDfill_window_more)
             s) <= (eval (ENum (1)) s))%Z)),12%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDfill_window_more)
             s) > (eval (ENum (1)) s))%Z)),11%positive)::
             (11%positive,AWeaken,27%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDfill_window_match_start
             (Some (ESub (EVar IDfill_window_match_start) (ENum (8192))))),
             14%positive)::
             (14%positive,(AAssign IDfill_window_strstart
             (Some (ESub (EVar IDfill_window_strstart) (ENum (8192))))),
             15%positive)::
             (15%positive,(AAssign IDfill_window_block_start
             (Some (ESub (EVar IDfill_window_block_start) (ENum (8192))))),
             16%positive)::
             (16%positive,(AAssign IDfill_window_n (Some (ENum (0)))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,42%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDfill_window_n (Some (ENum (0)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) < (eval (ENum (8192)) s))%Z)),28%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) >= (eval (ENum (8192)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDfill_window_more
             (Some (EAdd (EVar IDfill_window_more) (ENum (8192))))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,ANone,58%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDfill_window_m None),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDfill_window_m)
             s) >= (eval (ENum (8192)) s))%Z)),34%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDfill_window_m)
             s) < (eval (ENum (8192)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,36%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDfill_window_n
             (Some (EAdd (EVar IDfill_window_n) (ENum (1))))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDfill_window_z (Some (EAdd (ENum (1))
             (EVar IDfill_window_z)))),41%positive)::
             (41%positive,AWeaken,23%positive)::
             (42%positive,(AAssign IDfill_window_m None),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AGuard (fun s => ((eval (EVar IDfill_window_m)
             s) >= (eval (ENum (8192)) s))%Z)),47%positive)::
             (44%positive,(AGuard (fun s => ((eval (EVar IDfill_window_m)
             s) < (eval (ENum (8192)) s))%Z)),45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,49%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDfill_window_n
             (Some (EAdd (EVar IDfill_window_n) (ENum (1))))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDfill_window_z (Some (EAdd (ENum (1))
             (EVar IDfill_window_z)))),54%positive)::
             (54%positive,AWeaken,19%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AAssign IDfill_window_more
             (Some (EAdd (EVar IDfill_window_more) (ENum (-1))))),
             57%positive)::(57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDfill_window_n None),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) = (eval (ENum (0)) s))%Z)),68%positive)::
             (60%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) <> (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) = (eval (ENum (-1)) s))%Z)),67%positive)::
             (62%positive,(AGuard (fun s => ((eval (EVar IDfill_window_n)
             s) <> (eval (ENum (-1)) s))%Z)),63%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDfill_window_lookahead
             (Some (EAdd (EVar IDfill_window_lookahead)
             (EVar IDfill_window_n)))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,72%positive)::
             (67%positive,AWeaken,69%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AAssign IDfill_window_eofile (Some (ENum (1)))),
             70%positive)::(70%positive,ANone,71%positive)::
             (71%positive,AWeaken,72%positive)::nil
|}.

Definition fill_window_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0)%Z
    | 4%positive => (-1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_more) <= 0)%Z
    | 5%positive => (-1 * (s IDfill_window_more) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0)%Z
    | 6%positive => (-1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_more) <= 0)%Z
    | 7%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0)%Z
    | 8%positive => (-1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 9%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0)%Z
    | 10%positive => (-1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 11%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_more) + 2 <= 0)%Z
    | 12%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 14%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 15%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 16%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 17%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_n) <= 0)%Z
    | 18%positive => (-1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 19%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 21%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_n) <= 0)%Z
    | 22%positive => (-1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 23%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0)%Z
    | 24%positive => (1 * (s IDfill_window_n) + -8192 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) + 8192 <= 0)%Z
    | 25%positive => (-1 * (s IDfill_window_n) + 8192 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0)%Z
    | 26%positive => (1 * (s IDfill_window_n) + -8192 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) + 8192 <= 0 /\ 1 * (s IDfill_window_more) + -8193 <= 0)%Z
    | 27%positive => (-1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 28%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0)%Z
    | 29%positive => (1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 30%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0)%Z
    | 31%positive => (1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 32%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0 /\ 1 * (s IDfill_window_m) + -8191 <= 0)%Z
    | 33%positive => (1 * (s IDfill_window_m) + -8191 <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 34%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_m) + 8192 <= 0)%Z
    | 35%positive => (-1 * (s IDfill_window_m) + 8192 <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 36%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8191 <= 0)%Z
    | 37%positive => (1 * (s IDfill_window_n) + -8191 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 38%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0 /\ -1 * (s IDfill_window_n) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDfill_window_n) + 1 <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 40%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0 /\ -1 * (s IDfill_window_n) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDfill_window_n) + 1 <= 0 /\ 1 * (s IDfill_window_n) + -8192 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 43%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 44%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 45%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ 1 * (s IDfill_window_m) + -8191 <= 0)%Z
    | 46%positive => (1 * (s IDfill_window_m) + -8191 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 47%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_m) + 8192 <= 0)%Z
    | 48%positive => (-1 * (s IDfill_window_m) + 8192 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 49%positive => (-1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 50%positive => (1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 51%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDfill_window_n) + 1 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 53%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_n) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDfill_window_n) + 1 <= 0 /\ 1 * (s IDfill_window_more) + -1 <= 0 /\ -1 * (s IDfill_window_z) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ 1 * (s IDfill_window_more) + 1 <= 0 /\ -1 * (s IDfill_window_more) + -1 <= 0)%Z
    | 56%positive => (-1 * (s IDfill_window_more) + -1 <= 0 /\ 1 * (s IDfill_window_more) + 1 <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 57%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_z) <= 0 /\ -1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_m) <= 0 /\ -1 * (s IDfill_window_more) + -2 <= 0 /\ 1 * (s IDfill_window_more) + 2 <= 0)%Z
    | 58%positive => (-1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 59%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 60%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 61%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 62%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 63%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 64%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 65%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 66%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | 67%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) + 1 <= 0 /\ -1 * (s IDfill_window_n) + -1 <= 0)%Z
    | 68%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_n) <= 0)%Z
    | 69%positive => (-1 * (s IDfill_window_n) + -1 <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 70%positive => (-1 * (s IDfill_window_z) <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_n) + -1 <= 0 /\ 1 * (s IDfill_window_eofile) + -1 <= 0 /\ -1 * (s IDfill_window_eofile) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDfill_window_eofile) + 1 <= 0 /\ 1 * (s IDfill_window_eofile) + -1 <= 0 /\ -1 * (s IDfill_window_n) + -1 <= 0 /\ 1 * (s IDfill_window_n) <= 0 /\ -1 * (s IDfill_window_z) <= 0)%Z
    | 72%positive => (-1 * (s IDfill_window_z) <= 0)%Z
    | _ => False
  end.

Definition fill_window_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (0)%Q
    | 2%positive => ((1 # 2) * (s IDfill_window_fresh.7) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_fresh.7)) * max0((s IDfill_window_z)))%Q
    | 3%positive => ((1 # 2) * (s IDfill_window_fresh.7) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_fresh.7)) * max0((s IDfill_window_z)))%Q
    | 4%positive => ((1 # 2) * (s IDfill_window_fresh.7) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_fresh.7)) * max0((s IDfill_window_z)))%Q
    | 5%positive => ((1 # 2) * (s IDfill_window_fresh.7) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_fresh.7)) * max0((s IDfill_window_z)))%Q
    | 6%positive => ((1 # 2) * (s IDfill_window_fresh.7) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_fresh.7)) * max0((s IDfill_window_z)))%Q
    | 7%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 8%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 9%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                     - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 10%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 11%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 12%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 13%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 14%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 15%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 16%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 17%positive => ((1 # 2)
                      + (1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      + (0 # 1) * (s IDfill_window_z)
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      + (0 # 1) * max0(8192 - (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0((s IDfill_window_z)))%Q
    | 18%positive => ((1 # 2)
                      + (1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      + (0 # 1) * (s IDfill_window_z)
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      + (0 # 1) * max0(8192 - (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0((s IDfill_window_z)))%Q
    | 19%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_z)))%Q
    | 20%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_z)))%Q
    | 21%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_n) * max0((s IDfill_window_z))
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0(1 + (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_n))
                      + (0 # 1) * max0((s IDfill_window_z)))%Q
    | 22%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_n) * max0((s IDfill_window_z))
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0(1 + (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_n))
                      + (0 # 1) * max0((s IDfill_window_z)))%Q
    | 23%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_n) * max0((s IDfill_window_z))
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n))
                      - (0 # 1) * max0(1 + (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_n))
                      + (0 # 1) * max0((s IDfill_window_z)))%Q
    | 24%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_n) * max0((s IDfill_window_z))
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n))
                      - (0 # 1) * max0(1 + (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_n))
                      + (0 # 1) * max0((s IDfill_window_z)))%Q
    | 25%positive => (max0((s IDfill_window_z)))%Q
    | 26%positive => (max0((s IDfill_window_z)))%Q
    | 27%positive => (max0((s IDfill_window_z)))%Q
    | 28%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_n) * max0((s IDfill_window_z))
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n))
                      - (0 # 1) * max0(1 + (s IDfill_window_n)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_n))
                      + (0 # 1) * max0((s IDfill_window_z)))%Q
    | 29%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 30%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 31%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 32%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 33%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 34%positive => ((1 # 2)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 35%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 36%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 37%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0(1
                                                             + (s IDfill_window_n)))%Q
    | 38%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0((s IDfill_window_n)))%Q
    | 39%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0((s IDfill_window_n)))%Q
    | 40%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0((s IDfill_window_n)))%Q
    | 41%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (0 # 1) * (s IDfill_window_z) * max0((s IDfill_window_n))
                      - (0 # 1) * max0((s IDfill_window_n)))%Q
    | 42%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_z)))%Q
    | 43%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z)
                      + (1 # 2) * (s IDfill_window_z) * max0(-2
                                                             + (s IDfill_window_more))
                      + (1 # 2) * (s IDfill_window_z) * max0(1
                                                             - (s IDfill_window_more))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(1 - (s IDfill_window_more)) * max0((s IDfill_window_z))
                      - (0 # 1) * max0((s IDfill_window_z)))%Q
    | 44%positive => ((1 # 2))%Q
    | 45%positive => ((1 # 2))%Q
    | 46%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 47%positive => ((1 # 2))%Q
    | 48%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 49%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 50%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 51%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 52%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 53%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 54%positive => ((1 # 2) + (0 # 1) * (s IDfill_window_z))%Q
    | 55%positive => ((1 # 2) * (s IDfill_window_more) * max0((s IDfill_window_z))
                      - (1 # 2) * max0(-2 + (s IDfill_window_more)) * max0((s IDfill_window_z)))%Q
    | 56%positive => (max0((s IDfill_window_z)))%Q
    | 57%positive => (max0((s IDfill_window_z)))%Q
    | 58%positive => (max0((s IDfill_window_z)))%Q
    | 59%positive => (max0((s IDfill_window_z)))%Q
    | 60%positive => (max0((s IDfill_window_z)))%Q
    | 61%positive => (max0((s IDfill_window_z)))%Q
    | 62%positive => (max0((s IDfill_window_z)))%Q
    | 63%positive => (max0((s IDfill_window_z)))%Q
    | 64%positive => (max0((s IDfill_window_z)))%Q
    | 65%positive => (max0((s IDfill_window_z)))%Q
    | 66%positive => (max0((s IDfill_window_z)))%Q
    | 67%positive => (max0((s IDfill_window_z)))%Q
    | 68%positive => (max0((s IDfill_window_z)))%Q
    | 69%positive => ((s IDfill_window_z))%Q
    | 70%positive => ((s IDfill_window_z))%Q
    | 71%positive => ((s IDfill_window_z))%Q
    | 72%positive => ((s IDfill_window_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition fill_window_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDfill_window_more)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - (s IDfill_window_more))) (F_check_ge (1
                                                                    - (s IDfill_window_more)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-6.10352e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (8192
                                                                    - (s IDfill_window_n))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDfill_window_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDfill_window_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)))]
    | 23%positive => []
    | 24%positive => [(*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-8192
                                                                    + (s IDfill_window_n)) (0))) (F_max0_ge_0 (-8192
                                                                    + (s IDfill_window_n)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-8192
                                                                    + (s IDfill_window_n))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDfill_window_n))) (F_check_ge (0) (0)));
                      (*-0.000122063 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_n)) (0))) (F_max0_ge_0 ((s IDfill_window_n)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*4.75762e-07 0*) F_one;
                      (*0 0.000122055*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDfill_window_n)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDfill_window_n)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-4.75697e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 33%positive => []
    | 34%positive => [(*0 -4.75697e-07*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0));
                      (*0 4.75697e-07*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_n))) (F_check_ge ((s IDfill_window_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_n))) (F_check_ge (0) (0)));
                      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDfill_window_n))) (F_check_ge (0) (0)));
                      (*4.75697e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 42%positive => []
    | 43%positive => [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))]
    | 44%positive => []
    | 45%positive => [(*2.65597e-07 0*) F_one;
                      (*2.65597e-07 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0));
                      (*-2.65597e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 46%positive => []
    | 47%positive => [(*0 -2.65597e-07*) F_one;
                      (*0 -2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0));
                      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0))]
    | 55%positive => [(*-1.5 0*) F_binom_monotonic 2 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDfill_window_more))) (F_check_ge (-1
                                                                    - (s IDfill_window_more)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDfill_window_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDfill_window_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDfill_window_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDfill_window_z))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDfill_window_more))) (F_check_ge (0) (0)));
                      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfill_window_z)) (0))) (F_max0_ge_0 ((s IDfill_window_z)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDfill_window_z))) (F_check_ge (0) (0)))]
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
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 67%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 68%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDfill_window_z))) (F_check_ge ((s IDfill_window_z)) (0))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | _ => []
  end.


Theorem fill_window_ai_correct:
  forall s p' s', steps (g_start fill_window) s (g_edges fill_window) p' s' -> fill_window_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fill_window_pot_correct:
  forall s p' s',
    steps (g_start fill_window) s (g_edges fill_window) p' s' ->
    (fill_window_pot (g_start fill_window) s >= fill_window_pot p' s')%Q.
Proof.
  check_lp fill_window_ai_correct fill_window_hints.
Qed.

