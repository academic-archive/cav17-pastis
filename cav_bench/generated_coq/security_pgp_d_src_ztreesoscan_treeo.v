Require Import pasta.Pasta.

Notation IDscan_tree_z := 1%positive.
Notation IDscan_tree__tmp := 2%positive.
Notation IDscan_tree_count := 3%positive.
Notation IDscan_tree_curlen := 4%positive.
Notation IDscan_tree_max_count := 5%positive.
Notation IDscan_tree_min_count := 6%positive.
Notation IDscan_tree_n := 7%positive.
Notation IDscan_tree_nextlen := 8%positive.
Notation IDscan_tree_prevlen := 9%positive.
Notation IDscan_tree_tree_dref_off0_off2 := 10%positive.
Notation IDscan_tree_max_code := 11%positive.
Notation IDscan_tree_tree := 12%positive.
Definition scan_tree : graph := {|
  g_start := 1%positive;
  g_end := 20%positive;
  g_edges := (1%positive,(AAssign IDscan_tree_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDscan_tree__tmp
             (Some (EVar IDscan_tree_max_code))),3%positive)::
             (3%positive,(AAssign IDscan_tree_prevlen (Some (ENum (-1)))),
             4%positive)::
             (4%positive,(AAssign IDscan_tree_nextlen
             (Some (EVar IDscan_tree_tree_dref_off0_off2))),5%positive)::
             (5%positive,(AAssign IDscan_tree_count (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDscan_tree_max_count (Some (ENum (7)))),
             7%positive)::
             (7%positive,(AAssign IDscan_tree_min_count (Some (ENum (4)))),
             8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_nextlen)
             s) = (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_nextlen)
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,15%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDscan_tree_max_count
             (Some (ENum (138)))),13%positive)::
             (13%positive,(AAssign IDscan_tree_min_count (Some (ENum (3)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDscan_tree_n (Some (ENum (0)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_n) s) <=
             (eval (EVar IDscan_tree__tmp) s))%Z)),21%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_n) s) >
             (eval (EVar IDscan_tree__tmp) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDscan_tree_curlen
             (Some (EVar IDscan_tree_nextlen))),23%positive)::
             (23%positive,(AAssign IDscan_tree_nextlen None),24%positive)::
             (24%positive,(AAssign IDscan_tree_count
             (Some (EAdd (EVar IDscan_tree_count) (ENum (1))))),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDscan_tree_count) (ENum (1))) s) <
             (eval (EVar IDscan_tree_max_count) s))%Z)),28%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDscan_tree_count) (ENum (1)))
             s) >= (eval (EVar IDscan_tree_max_count) s))%Z)),27%positive)::
             (27%positive,AWeaken,31%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) = (eval (EVar IDscan_tree_nextlen) s))%Z)),71%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) <> (eval (EVar IDscan_tree_nextlen) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_count)
             s) < (eval (EVar IDscan_tree_min_count) s))%Z)),48%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_count)
             s) >= (eval (EVar IDscan_tree_min_count) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) <> (eval (ENum (0)) s))%Z)),41%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) = (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_count)
             s) <= (eval (ENum (10)) s))%Z)),38%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_count)
             s) > (eval (ENum (10)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,40%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,47%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) <> (eval (EVar IDscan_tree_prevlen) s))%Z)),44%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) = (eval (EVar IDscan_tree_prevlen) s))%Z)),43%positive)::
             (43%positive,AWeaken,46%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,50%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDscan_tree_count (Some (ENum (0)))),
             52%positive)::
             (52%positive,(AAssign IDscan_tree_prevlen
             (Some (EVar IDscan_tree_curlen))),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_nextlen)
             s) = (eval (ENum (0)) s))%Z)),66%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_nextlen)
             s) <> (eval (ENum (0)) s))%Z)),55%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) = (eval (EVar IDscan_tree_nextlen) s))%Z)),61%positive)::
             (56%positive,(AGuard (fun s => ((eval (EVar IDscan_tree_curlen)
             s) <> (eval (EVar IDscan_tree_nextlen) s))%Z)),57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AAssign IDscan_tree_max_count (Some (ENum (7)))),
             59%positive)::
             (59%positive,(AAssign IDscan_tree_min_count (Some (ENum (4)))),
             60%positive)::(60%positive,ANone,65%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AAssign IDscan_tree_max_count (Some (ENum (6)))),
             63%positive)::
             (63%positive,(AAssign IDscan_tree_min_count (Some (ENum (3)))),
             64%positive)::(64%positive,ANone,65%positive)::
             (65%positive,ANone,70%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,(AAssign IDscan_tree_max_count
             (Some (ENum (138)))),68%positive)::
             (68%positive,(AAssign IDscan_tree_min_count (Some (ENum (3)))),
             69%positive)::(69%positive,ANone,70%positive)::
             (70%positive,ANone,73%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDscan_tree_n
             (Some (EAdd (EVar IDscan_tree_n) (ENum (1))))),74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDscan_tree_z (Some (EAdd (ENum (1))
             (EVar IDscan_tree_z)))),77%positive)::
             (77%positive,AWeaken,18%positive)::nil
|}.

Definition scan_tree_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0)%Z
    | 3%positive => (-1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0)%Z
    | 4%positive => (1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0)%Z
    | 5%positive => (-1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0)%Z
    | 6%positive => (1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 7%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0)%Z
    | 8%positive => (-1 * (s IDscan_tree_max_count) + 7 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0)%Z
    | 9%positive => (-1 * (s IDscan_tree_min_count) + 4 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0)%Z
    | 10%positive => (-1 * (s IDscan_tree_max_count) + 7 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0)%Z
    | 11%positive => (-1 * (s IDscan_tree_max_count) + 7 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0)%Z
    | 12%positive => (-1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0)%Z
    | 13%positive => (1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_max_count) + 138 <= 0)%Z
    | 14%positive => (-1 * (s IDscan_tree_max_count) + 138 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_min_count) + -3 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0)%Z
    | 15%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0)%Z
    | 16%positive => (1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_n) <= 0)%Z
    | 17%positive => (-1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_prevlen) + 1 <= 0 /\ -1 * (s IDscan_tree_prevlen) + -1 <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0)%Z
    | 18%positive => (-1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 19%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree__tmp)+ -1 * (s IDscan_tree_n) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDscan_tree__tmp)+ -1 * (s IDscan_tree_n) + 1 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 21%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0)%Z
    | 22%positive => (-1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 23%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0)%Z
    | 24%positive => (-1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 25%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 27%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_max_count) + -1 <= 0)%Z
    | 28%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0)%Z
    | 29%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 30%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0)%Z
    | 31%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 32%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0)%Z
    | 33%positive => (-1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 34%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0)%Z
    | 35%positive => (-1 * (s IDscan_tree_curlen) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_count) + 11 <= 0)%Z
    | 37%positive => (-1 * (s IDscan_tree_count) + 11 <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0 /\ 1 * (s IDscan_tree_count) + -10 <= 0)%Z
    | 39%positive => (1 * (s IDscan_tree_count) + -10 <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 40%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_curlen) <= 0 /\ -1 * (s IDscan_tree_curlen) <= 0)%Z
    | 41%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0)%Z
    | 42%positive => (-1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 43%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_prevlen) <= 0 /\ -1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_prevlen) <= 0)%Z
    | 44%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0)%Z
    | 45%positive => (-1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 46%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0)%Z
    | 47%positive => (-1 * (s IDscan_tree_count)+ 1 * (s IDscan_tree_min_count) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 48%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_min_count) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_min_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 50%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 1 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 52%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 53%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 54%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 55%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 56%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 57%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 58%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 59%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_max_count) + 7 <= 0)%Z
    | 60%positive => (-1 * (s IDscan_tree_max_count) + 7 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 4 <= 0)%Z
    | 61%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0)%Z
    | 62%positive => (-1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 63%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_max_count) + -6 <= 0 /\ -1 * (s IDscan_tree_max_count) + 6 <= 0)%Z
    | 64%positive => (-1 * (s IDscan_tree_max_count) + 6 <= 0 /\ 1 * (s IDscan_tree_max_count) + -6 <= 0 /\ -1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -3 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0)%Z
    | 65%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_max_count) + -7 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_max_count) + 6 <= 0)%Z
    | 66%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0)%Z
    | 67%positive => (-1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0)%Z
    | 68%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_max_count) + 138 <= 0)%Z
    | 69%positive => (-1 * (s IDscan_tree_max_count) + 138 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -3 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0)%Z
    | 70%positive => (-1 * (s IDscan_tree_max_count) + 6 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0)%Z
    | 71%positive => (1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ -1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0)%Z
    | 72%positive => (-1 * (s IDscan_tree_curlen)+ 1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_curlen)+ -1 * (s IDscan_tree_nextlen) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_count) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0)%Z
    | 73%positive => (-1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_n) <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0)%Z
    | 74%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_n) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) + -1 <= 0)%Z
    | 75%positive => (-1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) + -1 <= 0 /\ -1 * (s IDscan_tree_n) + 1 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0)%Z
    | 76%positive => (1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_z) <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ -1 * (s IDscan_tree_n) + 1 <= 0 /\ -1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) + -1 <= 0)%Z
    | 77%positive => (-1 * (s IDscan_tree__tmp)+ 1 * (s IDscan_tree_n) + -1 <= 0 /\ -1 * (s IDscan_tree_n) + 1 <= 0 /\ -1 * (s IDscan_tree_count) <= 0 /\ 1 * (s IDscan_tree_min_count) + -4 <= 0 /\ -1 * (s IDscan_tree_min_count) + 3 <= 0 /\ 1 * (s IDscan_tree_max_count) + -138 <= 0 /\ 1 * (s IDscan_tree_count)+ -1 * (s IDscan_tree_max_count) + 2 <= 0 /\ -1 * (s IDscan_tree_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition scan_tree_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDscan_tree_max_code)))%Q
    | 2%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree_max_code)))%Q
    | 3%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 4%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 5%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 6%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 7%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 8%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 9%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 10%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 11%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 12%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 13%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 14%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 15%positive => ((s IDscan_tree_z) + max0(1 + (s IDscan_tree__tmp)))%Q
    | 16%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 17%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 18%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 19%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 20%positive => ((s IDscan_tree_z))%Q
    | 21%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 22%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 23%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 24%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 25%positive => ((s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp) - (s IDscan_tree_n)))%Q
    | 26%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 27%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 28%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 29%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 30%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 31%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 32%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 33%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 34%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 35%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 36%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 37%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 38%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 39%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 40%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 41%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 42%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 43%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 44%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 45%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 46%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 47%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 48%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 49%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 50%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 51%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 52%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 53%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 54%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 55%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 56%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 57%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 58%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 59%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 60%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 61%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 62%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 63%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 64%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 65%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 66%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 67%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 68%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 69%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 70%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 71%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 72%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 73%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 74%positive => ((1 # 1) - (s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 75%positive => ((1 # 1) - (s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 76%positive => ((1 # 1) - (s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | 77%positive => (-(s IDscan_tree_n) + (s IDscan_tree_z)
                      + max0(1 + (s IDscan_tree__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition scan_tree_hints (p : node) (s : state) := 
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
                                                             + (s IDscan_tree__tmp)
                                                             - (s IDscan_tree_n)) ((s IDscan_tree__tmp)
                                                                    - (s IDscan_tree_n)));
                      (*-1 0*) F_max0_ge_0 ((s IDscan_tree__tmp)
                                            - (s IDscan_tree_n))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDscan_tree__tmp)
                                                                   - 
                                                                   (s IDscan_tree_n))) (F_check_ge (1
                                                                    + (s IDscan_tree__tmp)
                                                                    - (s IDscan_tree_n)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDscan_tree__tmp)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDscan_tree__tmp)))]
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
    | 77%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDscan_tree__tmp)
                                                                    - (s IDscan_tree_n)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDscan_tree__tmp)
                                                                    - (s IDscan_tree_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDscan_tree__tmp))) (F_check_ge (1
                                                                    + (s IDscan_tree__tmp)) (0))]
    | _ => []
  end.


Theorem scan_tree_ai_correct:
  forall s p' s', steps (g_start scan_tree) s (g_edges scan_tree) p' s' -> scan_tree_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem scan_tree_pot_correct:
  forall s p' s',
    steps (g_start scan_tree) s (g_edges scan_tree) p' s' ->
    (scan_tree_pot (g_start scan_tree) s >= scan_tree_pot p' s')%Q.
Proof.
  check_lp scan_tree_ai_correct scan_tree_hints.
Qed.

