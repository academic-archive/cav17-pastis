Require Import pasta.Pasta.

Notation IDIII_imdct_l_z := 1%positive.
Notation IDIII_imdct_l__tmp := 2%positive.
Notation IDIII_imdct_l_i := 3%positive.
Notation IDIII_imdct_l_X := 4%positive.
Notation IDIII_imdct_l_block_type := 5%positive.
Notation IDIII_imdct_l_z := 6%positive.
Definition III_imdct_l : graph := {|
  g_start := 1%positive;
  g_end := 98%positive;
  g_edges := (1%positive,(AAssign IDIII_imdct_l_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDIII_imdct_l__tmp
             (Some (EVar IDIII_imdct_l_block_type))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,97%positive)::(6%positive,ANone,83%positive)::
             (6%positive,ANone,45%positive)::(6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDIII_imdct_l_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (6)) s))%Z)),38%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (6)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDIII_imdct_l_i (Some (ENum (6)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (12)) s))%Z)),31%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (12)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDIII_imdct_l_i (Some (ENum (18)))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (36)) s))%Z)),24%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (36)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,98%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),30%positive)::
             (30%positive,AWeaken,20%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),37%positive)::
             (37%positive,AWeaken,15%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),44%positive)::
             (44%positive,AWeaken,10%positive)::
             (45%positive,(AAssign IDIII_imdct_l_i (Some (ENum (0)))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (18)) s))%Z)),76%positive)::
             (48%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (18)) s))%Z)),49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AAssign IDIII_imdct_l_i (Some (ENum (24)))),
             51%positive)::(51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (30)) s))%Z)),69%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (30)) s))%Z)),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AAssign IDIII_imdct_l_i (Some (ENum (30)))),
             56%positive)::(56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (36)) s))%Z)),62%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (36)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,98%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),68%positive)::
             (68%positive,AWeaken,58%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),75%positive)::
             (75%positive,AWeaken,53%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (1))))),79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),82%positive)::
             (82%positive,AWeaken,48%positive)::
             (83%positive,(AAssign IDIII_imdct_l_i (Some (ENum (0)))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) < (eval (ENum (36)) s))%Z)),90%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_l_i)
             s) >= (eval (ENum (36)) s))%Z)),87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,AWeaken,98%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,(AAssign IDIII_imdct_l_i
             (Some (EAdd (EVar IDIII_imdct_l_i) (ENum (4))))),93%positive)::
             (93%positive,ANone,94%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,(AAssign IDIII_imdct_l_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_l_z)))),96%positive)::
             (96%positive,AWeaken,86%positive)::
             (97%positive,AWeaken,98%positive)::nil
|}.

Definition III_imdct_l_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 4%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 6%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 7%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 8%positive => (1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 9%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 10%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0)%Z
    | 11%positive => (1 * (s IDIII_imdct_l_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 6 <= 0)%Z
    | 12%positive => (-1 * (s IDIII_imdct_l_i) + 6 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0)%Z
    | 13%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 6 <= 0)%Z
    | 14%positive => (-1 * (s IDIII_imdct_l_i) + 6 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 15%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 6 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -12 <= 0)%Z
    | 16%positive => (1 * (s IDIII_imdct_l_i) + -12 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 12 <= 0)%Z
    | 17%positive => (-1 * (s IDIII_imdct_l_i) + 12 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -12 <= 0)%Z
    | 18%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 18 <= 0)%Z
    | 19%positive => (-1 * (s IDIII_imdct_l_i) + 18 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 20%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 18 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 21%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 22%positive => (-1 * (s IDIII_imdct_l_i) + 36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 23%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 24%positive => (-1 * (s IDIII_imdct_l_i) + 18 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 25%positive => (1 * (s IDIII_imdct_l_i) + -35 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 18 <= 0)%Z
    | 26%positive => (-1 * (s IDIII_imdct_l_i) + 18 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 27%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 19 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 28%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 19 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 29%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 19 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 30%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 19 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDIII_imdct_l_i) + 6 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -11 <= 0)%Z
    | 32%positive => (1 * (s IDIII_imdct_l_i) + -11 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 6 <= 0)%Z
    | 33%positive => (-1 * (s IDIII_imdct_l_i) + 6 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -11 <= 0)%Z
    | 34%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 7 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -12 <= 0)%Z
    | 35%positive => (1 * (s IDIII_imdct_l_i) + -12 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 7 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 36%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 7 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -12 <= 0)%Z
    | 37%positive => (1 * (s IDIII_imdct_l_i) + -12 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 7 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -5 <= 0)%Z
    | 39%positive => (1 * (s IDIII_imdct_l_i) + -5 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 40%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -5 <= 0)%Z
    | 41%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0)%Z
    | 42%positive => (1 * (s IDIII_imdct_l_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 43%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -6 <= 0)%Z
    | 44%positive => (1 * (s IDIII_imdct_l_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 46%positive => (1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 47%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 48%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0)%Z
    | 49%positive => (1 * (s IDIII_imdct_l_i) + -18 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 18 <= 0)%Z
    | 50%positive => (-1 * (s IDIII_imdct_l_i) + 18 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0)%Z
    | 51%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -24 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 24 <= 0)%Z
    | 52%positive => (-1 * (s IDIII_imdct_l_i) + 24 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -24 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 53%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 24 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0)%Z
    | 54%positive => (1 * (s IDIII_imdct_l_i) + -30 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 30 <= 0)%Z
    | 55%positive => (-1 * (s IDIII_imdct_l_i) + 30 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0)%Z
    | 56%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 30 <= 0)%Z
    | 57%positive => (-1 * (s IDIII_imdct_l_i) + 30 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 58%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 30 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 59%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 60%positive => (-1 * (s IDIII_imdct_l_i) + 36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 61%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 62%positive => (-1 * (s IDIII_imdct_l_i) + 30 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 63%positive => (1 * (s IDIII_imdct_l_i) + -35 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 30 <= 0)%Z
    | 64%positive => (-1 * (s IDIII_imdct_l_i) + 30 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 65%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 31 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 66%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 31 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 67%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 31 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -36 <= 0)%Z
    | 68%positive => (1 * (s IDIII_imdct_l_i) + -36 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 31 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 69%positive => (-1 * (s IDIII_imdct_l_i) + 24 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -29 <= 0)%Z
    | 70%positive => (1 * (s IDIII_imdct_l_i) + -29 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 24 <= 0)%Z
    | 71%positive => (-1 * (s IDIII_imdct_l_i) + 24 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -29 <= 0)%Z
    | 72%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 25 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0)%Z
    | 73%positive => (1 * (s IDIII_imdct_l_i) + -30 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 25 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 74%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 25 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -30 <= 0)%Z
    | 75%positive => (1 * (s IDIII_imdct_l_i) + -30 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 25 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -17 <= 0)%Z
    | 77%positive => (1 * (s IDIII_imdct_l_i) + -17 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 78%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -17 <= 0)%Z
    | 79%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0)%Z
    | 80%positive => (1 * (s IDIII_imdct_l_i) + -18 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 81%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -18 <= 0)%Z
    | 82%positive => (1 * (s IDIII_imdct_l_i) + -18 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 83%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 84%positive => (1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 85%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 86%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -39 <= 0)%Z
    | 87%positive => (1 * (s IDIII_imdct_l_i) + -39 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 88%positive => (-1 * (s IDIII_imdct_l_i) + 36 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -39 <= 0)%Z
    | 89%positive => (1 * (s IDIII_imdct_l_i) + -39 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 36 <= 0)%Z
    | 90%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 91%positive => (1 * (s IDIII_imdct_l_i) + -35 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 92%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_i) + -35 <= 0)%Z
    | 93%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 4 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -39 <= 0)%Z
    | 94%positive => (1 * (s IDIII_imdct_l_i) + -39 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 4 <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | 95%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) + 4 <= 0 /\ 1 * (s IDIII_imdct_l_i) + -39 <= 0)%Z
    | 96%positive => (1 * (s IDIII_imdct_l_i) + -39 <= 0 /\ -1 * (s IDIII_imdct_l_i) + 4 <= 0 /\ -1 * (s IDIII_imdct_l_z) + 1 <= 0)%Z
    | 97%positive => (-1 * (s IDIII_imdct_l_z) <= 0 /\ 1 * (s IDIII_imdct_l_z) <= 0 /\ -1 * (s IDIII_imdct_l_i) <= 0)%Z
    | 98%positive => (-1 * (s IDIII_imdct_l_i) <= 0 /\ -1 * (s IDIII_imdct_l_z) <= 0)%Z
    | _ => False
  end.

Definition III_imdct_l_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((30 # 1))%Q
    | 2%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 3%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 4%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 5%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 6%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 7%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 8%positive => ((24 # 1) + max0(6 - (s IDIII_imdct_l_i))
                     + max0((s IDIII_imdct_l_z)))%Q
    | 9%positive => ((24 # 1) + max0(6 - (s IDIII_imdct_l_i))
                     + max0((s IDIII_imdct_l_z)))%Q
    | 10%positive => ((24 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 11%positive => ((24 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 12%positive => ((24 # 1) + (s IDIII_imdct_l_z))%Q
    | 13%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 14%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 15%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 16%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 17%positive => ((18 # 1) + (s IDIII_imdct_l_z))%Q
    | 18%positive => (-(4 # 1) + (s IDIII_imdct_l_z)
                      - max0(35 - (s IDIII_imdct_l_i))
                      + max0(36 - (s IDIII_imdct_l_i))
                      + max0(39 - (s IDIII_imdct_l_i)))%Q
    | 19%positive => (-(4 # 1) + (s IDIII_imdct_l_z)
                      - max0(35 - (s IDIII_imdct_l_i))
                      + max0(36 - (s IDIII_imdct_l_i))
                      + max0(39 - (s IDIII_imdct_l_i)))%Q
    | 20%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 21%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 22%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 23%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 24%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 25%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(35 - (s IDIII_imdct_l_i)))%Q
    | 26%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(35 - (s IDIII_imdct_l_i)))%Q
    | 27%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 28%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 29%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 30%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 31%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 32%positive => ((2 # 1) + (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(11 - (s IDIII_imdct_l_i))
                      + max0(17 - (s IDIII_imdct_l_i)))%Q
    | 33%positive => ((2 # 1) + (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(11 - (s IDIII_imdct_l_i))
                      + max0(17 - (s IDIII_imdct_l_i)))%Q
    | 34%positive => ((1 # 1) + (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 35%positive => ((1 # 1) + (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 36%positive => ((1 # 1) + (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 37%positive => ((s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(12 - (s IDIII_imdct_l_i))
                      + max0(18 - (s IDIII_imdct_l_i)))%Q
    | 38%positive => ((24 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 39%positive => ((25 # 1) + (s IDIII_imdct_l_z)
                      + max0(5 - (s IDIII_imdct_l_i)))%Q
    | 40%positive => ((25 # 1) + (s IDIII_imdct_l_z)
                      + max0(5 - (s IDIII_imdct_l_i)))%Q
    | 41%positive => ((25 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 42%positive => ((25 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 43%positive => ((25 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 44%positive => ((24 # 1) + (s IDIII_imdct_l_z)
                      + max0(6 - (s IDIII_imdct_l_i)))%Q
    | 45%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 46%positive => ((30 # 1) - (s IDIII_imdct_l_i)
                      + max0((s IDIII_imdct_l_z)))%Q
    | 47%positive => ((30 # 1) - (s IDIII_imdct_l_i)
                      + max0((s IDIII_imdct_l_z)))%Q
    | 48%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 49%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 50%positive => ((12 # 1) + (s IDIII_imdct_l_z))%Q
    | 51%positive => ((6 # 35) * (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 52%positive => ((6 # 35) * (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 53%positive => ((6 # 35) * (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 54%positive => ((6 # 35) * (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 55%positive => ((6 # 1) + (s IDIII_imdct_l_z))%Q
    | 56%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 57%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 58%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 59%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 60%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 61%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 62%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 63%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(35 - (s IDIII_imdct_l_i)))%Q
    | 64%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(35 - (s IDIII_imdct_l_i)))%Q
    | 65%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 66%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 67%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 68%positive => ((s IDIII_imdct_l_z) + max0(36 - (s IDIII_imdct_l_i)))%Q
    | 69%positive => ((6 # 35) * (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z)
                      + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 70%positive => ((1 # 1) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(29 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 71%positive => ((1 # 1) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(29 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 72%positive => ((29 # 35) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(36 - (s IDIII_imdct_l_i)))%Q
    | 73%positive => ((29 # 35) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(36 - (s IDIII_imdct_l_i)))%Q
    | 74%positive => ((29 # 35) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(36 - (s IDIII_imdct_l_i)))%Q
    | 75%positive => (-(6 # 35) + (6 # 35) * (s IDIII_imdct_l_i)
                      + (s IDIII_imdct_l_z) + max0(30 - (s IDIII_imdct_l_i))
                      + (6 # 35) * max0(36 - (s IDIII_imdct_l_i)))%Q
    | 76%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 77%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 78%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 79%positive => ((31 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 80%positive => ((31 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 81%positive => ((31 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 82%positive => ((30 # 1) - (s IDIII_imdct_l_i) + (s IDIII_imdct_l_z))%Q
    | 83%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 84%positive => ((9 # 8) * max0(18 - (s IDIII_imdct_l_i))
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i))
                      + max0((s IDIII_imdct_l_z)))%Q
    | 85%positive => ((9 # 8) * max0(18 - (s IDIII_imdct_l_i))
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i))
                      + max0((s IDIII_imdct_l_z)))%Q
    | 86%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 87%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 88%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 89%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 90%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 91%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + (1 # 4) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 92%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + (1 # 4) * max0(35 - (s IDIII_imdct_l_i)))%Q
    | 93%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 94%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 95%positive => ((1 # 1) + (s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 96%positive => ((s IDIII_imdct_l_z)
                      + (1 # 4) * max0(39 - (s IDIII_imdct_l_i)))%Q
    | 97%positive => ((30 # 1) + max0((s IDIII_imdct_l_z)))%Q
    | 98%positive => ((s IDIII_imdct_l_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_imdct_l_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_imdct_l_z))) (F_check_ge ((s IDIII_imdct_l_z)) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (6
                                                             - (s IDIII_imdct_l_i)) (5
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (5 - (s IDIII_imdct_l_i))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (12
                                                             - (s IDIII_imdct_l_i)) (11
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (11 - (s IDIII_imdct_l_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (18
                                                                   - 
                                                                   (s IDIII_imdct_l_i))) (F_check_ge (18
                                                                    - (s IDIII_imdct_l_i)) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_pre_decrement (39 - (s IDIII_imdct_l_i)) (4)]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDIII_imdct_l_i)) (35
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (35 - (s IDIII_imdct_l_i))]
    | 24%positive => [(*-1 0*) F_max0_pre_decrement (36 - (s IDIII_imdct_l_i)) (1)]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_pre_decrement (12 - (s IDIII_imdct_l_i)) (1);
                      (*-1 0*) F_max0_pre_decrement (18 - (s IDIII_imdct_l_i)) (1)]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*0 1*) F_max0_pre_decrement (6 - (s IDIII_imdct_l_i)) (1)]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_imdct_l_z))) (F_check_ge ((s IDIII_imdct_l_z)) (0))]
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (18
                                                             - (s IDIII_imdct_l_i)) (17
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (17 - (s IDIII_imdct_l_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                                    - (s IDIII_imdct_l_i)) (0))) (F_max0_ge_0 (18
                                                                    - (s IDIII_imdct_l_i)))]
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (30
                                                             - (s IDIII_imdct_l_i)) (29
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (29 - (s IDIII_imdct_l_i));
                      (*-0.171429 0*) F_binom_monotonic 1 (F_max0_ge_arg (35
                                                                    - (s IDIII_imdct_l_i))) (F_check_ge (35
                                                                    - (s IDIII_imdct_l_i)) (0))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDIII_imdct_l_i)) (35
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1 0*) F_max0_ge_0 (35 - (s IDIII_imdct_l_i))]
    | 62%positive => [(*-1 0*) F_max0_pre_decrement (36 - (s IDIII_imdct_l_i)) (1)]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => [(*-1 0*) F_max0_pre_decrement (30 - (s IDIII_imdct_l_i)) (1)]
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-0.171429 0*) F_max0_pre_decrement (36
                                                            - (s IDIII_imdct_l_i)) (1)]
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1.125 0*) F_max0_monotonic (F_check_ge (18
                                                                 - (s IDIII_imdct_l_i)) (17
                                                                    - (s IDIII_imdct_l_i)));
                      (*-1.125 0*) F_max0_ge_0 (17 - (s IDIII_imdct_l_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_imdct_l_z))) (F_check_ge ((s IDIII_imdct_l_z)) (0))]
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (39
                                                                - (s IDIII_imdct_l_i)) (35
                                                                    - (s IDIII_imdct_l_i)));
                      (*-0.25 0*) F_max0_ge_0 (35 - (s IDIII_imdct_l_i))]
    | 90%positive => [(*0 0.25*) F_max0_pre_decrement (39
                                                       - (s IDIII_imdct_l_i)) (4)]
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => [(*-30 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDIII_imdct_l_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDIII_imdct_l_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDIII_imdct_l_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDIII_imdct_l_z)))]
    | 98%positive => []
    | _ => []
  end.


Theorem III_imdct_l_ai_correct:
  forall s p' s', steps (g_start III_imdct_l) s (g_edges III_imdct_l) p' s' -> III_imdct_l_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_imdct_l_pot_correct:
  forall s p' s',
    steps (g_start III_imdct_l) s (g_edges III_imdct_l) p' s' ->
    (III_imdct_l_pot (g_start III_imdct_l) s >= III_imdct_l_pot p' s')%Q.
Proof.
  check_lp III_imdct_l_ai_correct III_imdct_l_hints.
Qed.

