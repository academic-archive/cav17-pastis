Require Import pasta.Pasta.

Notation IDs_exD_process_z := 1%positive.
Notation IDs_exD_process__tmp := 2%positive.
Notation IDs_exD_process__tmp1 := 3%positive.
Notation IDs_exD_process_count := 4%positive.
Notation IDs_exD_process_i := 5%positive.
Notation IDs_exD_process_rcount := 6%positive.
Notation IDs_exD_process_skip := 7%positive.
Notation IDs_exD_process_status := 8%positive.
Notation IDs_exD_process_wcount := 9%positive.
Notation IDs_exD_process_last := 10%positive.
Notation IDs_exD_process_pr := 11%positive.
Notation IDs_exD_process_pw := 12%positive.
Notation IDs_exD_process_st := 13%positive.
Definition s_exD_process : graph := {|
  g_start := 1%positive;
  g_end := 76%positive;
  g_edges := (1%positive,(AAssign IDs_exD_process_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDs_exD_process__tmp1
             (Some (EVar IDs_exD_process_last))),3%positive)::
             (3%positive,(AAssign IDs_exD_process_skip None),4%positive)::
             (4%positive,(AAssign IDs_exD_process_rcount None),5%positive)::
             (5%positive,(AAssign IDs_exD_process_wcount None),6%positive)::
             (6%positive,(AAssign IDs_exD_process_status (Some (ENum (0)))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_wcount) s) <
             (eval (EVar IDs_exD_process_rcount) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_wcount) s) >=
             (eval (EVar IDs_exD_process_rcount) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,14%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDs_exD_process_status (Some (ENum (1)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDs_exD_process_count None),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,36%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_rcount) s) <
             (eval (ENum (8)) s))%Z)),72%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_rcount) s) >=
             (eval (ENum (8)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDs_exD_process_i (Some (ENum (1)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDs_exD_process_i)
             s) <= (eval (ENum (8)) s))%Z)),26%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDs_exD_process_i)
             s) > (eval (ENum (8)) s))%Z)),25%positive)::
             (25%positive,AWeaken,34%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,66%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,66%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (31%positive,ANone,33%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,41%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDs_exD_process_status None),38%positive)::
             (38%positive,(AAssign IDs_exD_process_count None),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,52%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,44%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,48%positive)::
             (44%positive,(AAssign IDs_exD_process_count None),45%positive)::
             (45%positive,(AAssign IDs_exD_process_status (Some (ENum (0)))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (48%positive,ANone,50%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_skip) s) >=
             (eval (EVar IDs_exD_process_count) s))%Z)),54%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_skip) s) <
             (eval (EVar IDs_exD_process_count) s))%Z)),53%positive)::
             (53%positive,AWeaken,57%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_skip) s) <>
             (eval (ENum (0)) s))%Z)),59%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDs_exD_process_skip) s) =
             (eval (ENum (0)) s))%Z)),56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AAssign IDs_exD_process_count
             (Some (ESub (EVar IDs_exD_process_count)
             (EVar IDs_exD_process_skip)))),58%positive)::
             (58%positive,ANone,63%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDs_exD_process_count (Some (ENum (0)))),
             61%positive)::
             (61%positive,(AAssign IDs_exD_process_status (Some (ENum (0)))),
             62%positive)::(62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDs_exD_process__tmp
             (Some (EVar IDs_exD_process_status))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,AWeaken,76%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDs_exD_process_i
             (Some (EAdd (EVar IDs_exD_process_i) (ENum (1))))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDs_exD_process_z (Some (EAdd (ENum (1))
             (EVar IDs_exD_process_z)))),71%positive)::
             (71%positive,AWeaken,24%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,(AAssign IDs_exD_process__tmp (Some (ENum (0)))),
             74%positive)::(74%positive,ANone,75%positive)::
             (75%positive,AWeaken,76%positive)::nil
|}.

Definition s_exD_process_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 3%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0)%Z
    | 4%positive => (1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 5%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0)%Z
    | 6%positive => (1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 7%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 8%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 9%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_rcount)+ -1 * (s IDs_exD_process_wcount) <= 0)%Z
    | 10%positive => (1 * (s IDs_exD_process_rcount)+ -1 * (s IDs_exD_process_wcount) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 11%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount)+ 1 * (s IDs_exD_process_wcount) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDs_exD_process_rcount)+ 1 * (s IDs_exD_process_wcount) + 1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 13%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_rcount)+ 1 * (s IDs_exD_process_wcount) + 1 <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 15%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 16%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 17%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 18%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 19%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 20%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0)%Z
    | 21%positive => (-1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 22%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ 1 * (s IDs_exD_process_i) + -1 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDs_exD_process_i) + 1 <= 0 /\ 1 * (s IDs_exD_process_i) + -1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 24%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0)%Z
    | 25%positive => (1 * (s IDs_exD_process_i) + -9 <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 9 <= 0)%Z
    | 26%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -8 <= 0)%Z
    | 27%positive => (1 * (s IDs_exD_process_i) + -8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 28%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -8 <= 0)%Z
    | 29%positive => (1 * (s IDs_exD_process_i) + -8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 30%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDs_exD_process_i) + -8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 32%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -8 <= 0)%Z
    | 33%positive => (1 * (s IDs_exD_process_i) + -8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 34%positive => (1 * (s IDs_exD_process_i) + -9 <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 35%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0)%Z
    | 36%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 37%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 38%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 39%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 40%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 41%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 42%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 43%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 44%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 45%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 46%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 47%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 48%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 49%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 50%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 51%positive => (-1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 52%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 53%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_count)+ 1 * (s IDs_exD_process_skip) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_count)+ -1 * (s IDs_exD_process_skip) <= 0)%Z
    | 55%positive => (1 * (s IDs_exD_process_count)+ -1 * (s IDs_exD_process_skip) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 56%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_count)+ -1 * (s IDs_exD_process_skip) <= 0 /\ 1 * (s IDs_exD_process_skip) <= 0 /\ -1 * (s IDs_exD_process_skip) <= 0)%Z
    | 57%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 58%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 59%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_count)+ -1 * (s IDs_exD_process_skip) <= 0)%Z
    | 60%positive => (1 * (s IDs_exD_process_count)+ -1 * (s IDs_exD_process_skip) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 61%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_count) <= 0 /\ -1 * (s IDs_exD_process_count) <= 0)%Z
    | 62%positive => (-1 * (s IDs_exD_process_count) <= 0 /\ 1 * (s IDs_exD_process_count) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_status) <= 0)%Z
    | 63%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 64%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 65%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | 66%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -8 <= 0)%Z
    | 67%positive => (1 * (s IDs_exD_process_i) + -8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_i) + 1 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 68%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0 /\ -1 * (s IDs_exD_process_i) + 2 <= 0)%Z
    | 69%positive => (-1 * (s IDs_exD_process_i) + 2 <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0)%Z
    | 70%positive => (1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0 /\ -1 * (s IDs_exD_process_i) + 2 <= 0)%Z
    | 71%positive => (-1 * (s IDs_exD_process_i) + 2 <= 0 /\ 1 * (s IDs_exD_process_i) + -9 <= 0 /\ -1 * (s IDs_exD_process_rcount) + 8 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_z) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_rcount) + -7 <= 0)%Z
    | 73%positive => (1 * (s IDs_exD_process_rcount) + -7 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 74%positive => (-1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_rcount) + -7 <= 0 /\ 1 * (s IDs_exD_process__tmp) <= 0 /\ -1 * (s IDs_exD_process__tmp) <= 0)%Z
    | 75%positive => (-1 * (s IDs_exD_process__tmp) <= 0 /\ 1 * (s IDs_exD_process__tmp) <= 0 /\ 1 * (s IDs_exD_process_rcount) + -7 <= 0 /\ -1 * (s IDs_exD_process_status) <= 0 /\ 1 * (s IDs_exD_process_status) + -1 <= 0 /\ 1 * (s IDs_exD_process_z) <= 0 /\ -1 * (s IDs_exD_process_z) <= 0)%Z
    | 76%positive => (-1 * (s IDs_exD_process_z) <= 0)%Z
    | _ => False
  end.

Definition s_exD_process_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 3%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 4%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 5%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 6%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 7%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 8%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 9%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 10%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 11%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 12%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 13%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 14%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 15%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 16%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 17%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 18%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 19%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 20%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 21%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 22%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 23%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 24%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 25%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 26%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 27%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 28%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 29%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 30%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 31%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 32%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 33%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 34%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 35%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 36%positive => ((s IDs_exD_process_z))%Q
    | 37%positive => ((s IDs_exD_process_z))%Q
    | 38%positive => ((s IDs_exD_process_z))%Q
    | 39%positive => ((s IDs_exD_process_z))%Q
    | 40%positive => ((s IDs_exD_process_z))%Q
    | 41%positive => ((s IDs_exD_process_z))%Q
    | 42%positive => ((s IDs_exD_process_z))%Q
    | 43%positive => ((s IDs_exD_process_z))%Q
    | 44%positive => ((s IDs_exD_process_z))%Q
    | 45%positive => ((s IDs_exD_process_z))%Q
    | 46%positive => ((s IDs_exD_process_z))%Q
    | 47%positive => ((s IDs_exD_process_z))%Q
    | 48%positive => ((s IDs_exD_process_z))%Q
    | 49%positive => ((s IDs_exD_process_z))%Q
    | 50%positive => ((s IDs_exD_process_z))%Q
    | 51%positive => ((s IDs_exD_process_z))%Q
    | 52%positive => ((s IDs_exD_process_z))%Q
    | 53%positive => ((s IDs_exD_process_z))%Q
    | 54%positive => ((s IDs_exD_process_z))%Q
    | 55%positive => ((s IDs_exD_process_z))%Q
    | 56%positive => ((s IDs_exD_process_z))%Q
    | 57%positive => ((s IDs_exD_process_z))%Q
    | 58%positive => ((s IDs_exD_process_z))%Q
    | 59%positive => ((s IDs_exD_process_z))%Q
    | 60%positive => ((s IDs_exD_process_z))%Q
    | 61%positive => ((s IDs_exD_process_z))%Q
    | 62%positive => ((s IDs_exD_process_z))%Q
    | 63%positive => ((s IDs_exD_process_z))%Q
    | 64%positive => ((s IDs_exD_process_z))%Q
    | 65%positive => ((s IDs_exD_process_z))%Q
    | 66%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 67%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(8 - (s IDs_exD_process_i)))%Q
    | 68%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(9 - (s IDs_exD_process_i)))%Q
    | 69%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(9 - (s IDs_exD_process_i)))%Q
    | 70%positive => ((1 # 1) + (s IDs_exD_process_z)
                      + max0(9 - (s IDs_exD_process_i)))%Q
    | 71%positive => ((s IDs_exD_process_z) + max0(9 - (s IDs_exD_process_i)))%Q
    | 72%positive => ((8 # 1) + (s IDs_exD_process_z))%Q
    | 73%positive => ((8 # 1) * (s IDs_exD_process_status)
                      + (s IDs_exD_process_z)
                      + (8 # 1) * max0(1 - (s IDs_exD_process_status)))%Q
    | 74%positive => ((8 # 1) * (s IDs_exD_process_status)
                      + (s IDs_exD_process_z)
                      + (8 # 1) * max0(1 - (s IDs_exD_process_status)))%Q
    | 75%positive => ((8 # 1) * (s IDs_exD_process_status)
                      + (s IDs_exD_process_z)
                      + (8 # 1) * max0(1 - (s IDs_exD_process_status)))%Q
    | 76%positive => ((s IDs_exD_process_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition s_exD_process_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-8 0*) F_one]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                     - (s IDs_exD_process_i)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDs_exD_process_i)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDs_exD_process_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                   - 
                                                                   (s IDs_exD_process_i))) (F_check_ge (8
                                                                    - (s IDs_exD_process_i)) (0))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                 - (s IDs_exD_process_i))) (F_check_ge (0) (0))]
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
    | 72%positive => [(*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDs_exD_process_status)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDs_exD_process_status)))]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDs_exD_process_status))) (F_check_ge (0) (0));
                      (*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDs_exD_process_status)) (0))) (F_max0_ge_0 ((s IDs_exD_process_status)));
                      (*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDs_exD_process_status))) (F_check_ge (0) (0))]
    | 76%positive => []
    | _ => []
  end.


Theorem s_exD_process_ai_correct:
  forall s p' s', steps (g_start s_exD_process) s (g_edges s_exD_process) p' s' -> s_exD_process_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem s_exD_process_pot_correct:
  forall s p' s',
    steps (g_start s_exD_process) s (g_edges s_exD_process) p' s' ->
    (s_exD_process_pot (g_start s_exD_process) s >= s_exD_process_pot p' s')%Q.
Proof.
  check_lp s_exD_process_ai_correct s_exD_process_hints.
Qed.

