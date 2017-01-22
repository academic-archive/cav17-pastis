Require Import pasta.Pasta.

Notation IDsusan_principle_z := 1%positive.
Notation IDsusan_principle__tmp := 2%positive.
Notation IDsusan_principle__tmp1 := 3%positive.
Notation IDsusan_principle__tmp2 := 4%positive.
Notation IDsusan_principle_i := 5%positive.
Notation IDsusan_principle_j := 6%positive.
Notation IDsusan_principle_n := 7%positive.
Notation IDsusan_principle_bp := 8%positive.
Notation IDsusan_principle_in := 9%positive.
Notation IDsusan_principle_max_no := 10%positive.
Notation IDsusan_principle_r := 11%positive.
Notation IDsusan_principle_x_size := 12%positive.
Notation IDsusan_principle_y_size := 13%positive.
Definition susan_principle : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDsusan_principle_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsusan_principle__tmp2
             (Some (EVar IDsusan_principle_max_no))),3%positive)::
             (3%positive,(AAssign IDsusan_principle__tmp1
             (Some (EVar IDsusan_principle_x_size))),4%positive)::
             (4%positive,(AAssign IDsusan_principle__tmp
             (Some (EVar IDsusan_principle_y_size))),5%positive)::
             (5%positive,(AAssign IDsusan_principle_i (Some (ENum (3)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_i)
             s) < (eval (ESub (EVar IDsusan_principle__tmp) (ENum (3)))
             s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_i)
             s) >= (eval (ESub (EVar IDsusan_principle__tmp) (ENum (3)))
             s))%Z)),9%positive)::(9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDsusan_principle_j (Some (ENum (3)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_j)
             s) < (eval (ESub (EVar IDsusan_principle__tmp1) (ENum (3)))
             s))%Z)),23%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_j)
             s) >= (eval (ESub (EVar IDsusan_principle__tmp1) (ENum (3)))
             s))%Z)),16%positive)::(16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDsusan_principle_i
             (Some (EAdd (EVar IDsusan_principle_i) (ENum (1))))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDsusan_principle_z (Some (EAdd (ENum (1))
             (EVar IDsusan_principle_z)))),22%positive)::
             (22%positive,AWeaken,8%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDsusan_principle_n (Some (ENum (100)))),
             25%positive)::
             (25%positive,(AAssign IDsusan_principle_n None),26%positive)::
             (26%positive,(AAssign IDsusan_principle_n None),27%positive)::
             (27%positive,(AAssign IDsusan_principle_n None),28%positive)::
             (28%positive,(AAssign IDsusan_principle_n None),29%positive)::
             (29%positive,(AAssign IDsusan_principle_n None),30%positive)::
             (30%positive,(AAssign IDsusan_principle_n None),31%positive)::
             (31%positive,(AAssign IDsusan_principle_n None),32%positive)::
             (32%positive,(AAssign IDsusan_principle_n None),33%positive)::
             (33%positive,(AAssign IDsusan_principle_n None),34%positive)::
             (34%positive,(AAssign IDsusan_principle_n None),35%positive)::
             (35%positive,(AAssign IDsusan_principle_n None),36%positive)::
             (36%positive,(AAssign IDsusan_principle_n None),37%positive)::
             (37%positive,(AAssign IDsusan_principle_n None),38%positive)::
             (38%positive,(AAssign IDsusan_principle_n None),39%positive)::
             (39%positive,(AAssign IDsusan_principle_n None),40%positive)::
             (40%positive,(AAssign IDsusan_principle_n None),41%positive)::
             (41%positive,(AAssign IDsusan_principle_n None),42%positive)::
             (42%positive,(AAssign IDsusan_principle_n None),43%positive)::
             (43%positive,(AAssign IDsusan_principle_n None),44%positive)::
             (44%positive,(AAssign IDsusan_principle_n None),45%positive)::
             (45%positive,(AAssign IDsusan_principle_n None),46%positive)::
             (46%positive,(AAssign IDsusan_principle_n None),47%positive)::
             (47%positive,(AAssign IDsusan_principle_n None),48%positive)::
             (48%positive,(AAssign IDsusan_principle_n None),49%positive)::
             (49%positive,(AAssign IDsusan_principle_n None),50%positive)::
             (50%positive,(AAssign IDsusan_principle_n None),51%positive)::
             (51%positive,(AAssign IDsusan_principle_n None),52%positive)::
             (52%positive,(AAssign IDsusan_principle_n None),53%positive)::
             (53%positive,(AAssign IDsusan_principle_n None),54%positive)::
             (54%positive,(AAssign IDsusan_principle_n None),55%positive)::
             (55%positive,(AAssign IDsusan_principle_n None),56%positive)::
             (56%positive,(AAssign IDsusan_principle_n None),57%positive)::
             (57%positive,(AAssign IDsusan_principle_n None),58%positive)::
             (58%positive,(AAssign IDsusan_principle_n None),59%positive)::
             (59%positive,(AAssign IDsusan_principle_n None),60%positive)::
             (60%positive,(AAssign IDsusan_principle_n None),61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_n)
             s) <= (eval (EVar IDsusan_principle__tmp2) s))%Z)),64%positive)::
             (62%positive,(AGuard (fun s => ((eval (EVar IDsusan_principle_n)
             s) > (eval (EVar IDsusan_principle__tmp2) s))%Z)),63%positive)::
             (63%positive,AWeaken,66%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDsusan_principle_j
             (Some (EAdd (EVar IDsusan_principle_j) (ENum (1))))),
             68%positive)::(68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDsusan_principle_z (Some (EAdd (ENum (1))
             (EVar IDsusan_principle_z)))),71%positive)::
             (71%positive,AWeaken,15%positive)::nil
|}.

Definition susan_principle_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle_z) <= 0)%Z
    | 4%positive => (1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle_z) <= 0)%Z
    | 6%positive => (1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle_i) + -3 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ 1 * (s IDsusan_principle_i) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle_z) <= 0)%Z
    | 8%positive => (-1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 9%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle__tmp)+ -1 * (s IDsusan_principle_i) + -3 <= 0)%Z
    | 10%positive => (1 * (s IDsusan_principle__tmp)+ -1 * (s IDsusan_principle_i) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 11%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0)%Z
    | 12%positive => (-1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 13%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ 1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0)%Z
    | 14%positive => (-1 * (s IDsusan_principle_j) + 3 <= 0 /\ 1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 15%positive => (-1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 16%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0)%Z
    | 17%positive => (1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 18%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 20%positive => (-1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ 1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 22%positive => (-1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ 1 * (s IDsusan_principle__tmp1)+ -1 * (s IDsusan_principle_j) + -3 <= 0 /\ -1 * (s IDsusan_principle_z) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 24%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 25%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ 1 * (s IDsusan_principle_n) + -100 <= 0 /\ -1 * (s IDsusan_principle_n) + 100 <= 0)%Z
    | 26%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 27%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 28%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 29%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 30%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 31%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 32%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 33%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 34%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 35%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 36%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 37%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 38%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 39%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 40%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 41%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 42%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 43%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 44%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 45%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 46%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 47%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 48%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 49%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 50%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 51%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 52%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 53%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 54%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 55%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 56%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 57%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 58%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 59%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 60%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 61%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 62%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 63%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ 1 * (s IDsusan_principle__tmp2)+ -1 * (s IDsusan_principle_n) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle__tmp2)+ 1 * (s IDsusan_principle_n) <= 0)%Z
    | 65%positive => (-1 * (s IDsusan_principle__tmp2)+ 1 * (s IDsusan_principle_n) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 66%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 67%positive => (-1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 68%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 69%positive => (-1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0)%Z
    | 70%positive => (-1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_z) <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle_j) + 4 <= 0)%Z
    | 71%positive => (-1 * (s IDsusan_principle_j) + 4 <= 0 /\ -1 * (s IDsusan_principle__tmp1)+ 1 * (s IDsusan_principle_j) + 3 <= 0 /\ -1 * (s IDsusan_principle__tmp)+ 1 * (s IDsusan_principle_i) + 4 <= 0 /\ -1 * (s IDsusan_principle_i) + 3 <= 0 /\ -1 * (s IDsusan_principle_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition susan_principle_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-6 + (s IDsusan_principle_x_size)) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle_y_size))
                     + max0(-6 + (s IDsusan_principle_y_size)))%Q
    | 2%positive => ((s IDsusan_principle_z)
                     + max0(-6 + (s IDsusan_principle_x_size)) * max0(-6
                                                                    + (s IDsusan_principle_y_size))
                     + max0(-6 + (s IDsusan_principle_y_size)))%Q
    | 3%positive => ((s IDsusan_principle_z)
                     + max0(-6 + (s IDsusan_principle_x_size)) * max0(-6
                                                                    + (s IDsusan_principle_y_size))
                     + max0(-6 + (s IDsusan_principle_y_size)))%Q
    | 4%positive => ((s IDsusan_principle_z)
                     + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle_y_size))
                     + max0(-6 + (s IDsusan_principle_y_size)))%Q
    | 5%positive => ((s IDsusan_principle_z)
                     + max0(-6 + (s IDsusan_principle__tmp))
                     + max0(-6 + (s IDsusan_principle__tmp)) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp1)))%Q
    | 6%positive => (-(10 # 27)
                     - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                     + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (10 # 81) * (s IDsusan_principle_i)
                     + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                  + (s IDsusan_principle__tmp))
                     + (s IDsusan_principle_z)
                     + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                     - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (7 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     - (10 # 81) * max0(-3 + (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 7%positive => (-(10 # 27)
                     - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                     + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (10 # 81) * (s IDsusan_principle_i)
                     + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                  + (s IDsusan_principle__tmp))
                     + (s IDsusan_principle_z)
                     + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                     - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (7 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     - (10 # 81) * max0(-3 + (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 8%positive => (-(10 # 27)
                     - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                     + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (10 # 81) * (s IDsusan_principle_i)
                     + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                  + (s IDsusan_principle__tmp))
                     + (s IDsusan_principle_z)
                     + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                     - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (7 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     - (10 # 81) * max0(-3 + (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 9%positive => (-(10 # 27)
                     - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                     + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (10 # 81) * (s IDsusan_principle_i)
                     + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                  + (s IDsusan_principle__tmp))
                     + (s IDsusan_principle_z)
                     + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                     - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                     + (7 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                     - (10 # 81) * max0(-3 + (s IDsusan_principle_i))
                     + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 10%positive => ((s IDsusan_principle_z))%Q
    | 11%positive => (-(10 # 27)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      + (s IDsusan_principle_z)
                      + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (7 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 12%positive => (-(10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (11 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 13%positive => (-(10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (11 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 14%positive => (-(10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (11 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 15%positive => ((1 # 1)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (14 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 16%positive => ((1 # 1)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (14 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 17%positive => ((1 # 1)
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (1 # 9) * max0(-7 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (3 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      - (7 # 27) * max0(-3 + (s IDsusan_principle_i)))%Q
    | 18%positive => ((1 # 1)
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (1 # 9) * max0(-7 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (3 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      - (7 # 27) * max0(-3 + (s IDsusan_principle_i)))%Q
    | 19%positive => ((1 # 1)
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (2 # 27) * max0(-7 + (s IDsusan_principle__tmp))
                      + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (7 # 27) * max0(-4 + (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (49 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      - (2 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j)))%Q
    | 20%positive => ((1 # 1)
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (2 # 27) * max0(-7 + (s IDsusan_principle__tmp))
                      + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (7 # 27) * max0(-4 + (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (49 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      - (2 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j)))%Q
    | 21%positive => ((1 # 1)
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + 
                                                                    (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (2 # 27) * max0(-7 + (s IDsusan_principle__tmp))
                      + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (7 # 27) * max0(-4 + (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (49 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      - (2 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j)))%Q
    | 22%positive => ((1 # 27) * (s IDsusan_principle__tmp) * max0(-7
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (1 # 27) * (s IDsusan_principle_i) * max0(-7
                                                                  + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      - (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (s IDsusan_principle_z)
                      - (2 # 27) * max0(-7 + (s IDsusan_principle__tmp))
                      + (10 # 27) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (7 # 27) * max0(-4 + (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (49 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      - (2 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j)))%Q
    | 23%positive => ((1 # 1)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      + (s IDsusan_principle_z)
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - max0(-4 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (14 # 27) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 24%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 25%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 26%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 27%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 28%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 29%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 30%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 31%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 32%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 33%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 34%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 35%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 36%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 37%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 38%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 39%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 40%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 41%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 42%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 43%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 44%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 45%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 46%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 47%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 48%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 49%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 50%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 51%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 52%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 53%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 54%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 55%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 56%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 57%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 58%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 59%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 60%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 61%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 27) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (s IDsusan_principle__tmp) * max0(-3
                                                          + (s IDsusan_principle__tmp1)
                                                          - (s IDsusan_principle_j))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (s IDsusan_principle_i) * max0(-3
                                                       + (s IDsusan_principle__tmp1)
                                                       - (s IDsusan_principle_j))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (14 # 27) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (19 # 54) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp)
                             - (s IDsusan_principle_i)) * max0(-3
                                                               + (s IDsusan_principle__tmp1)
                                                               - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (4 # 1) * max0(-3 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + (1 # 6) * max0(-3 + (s IDsusan_principle_i)) * max0((s IDsusan_principle_z))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 62%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 77) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 20) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 5) * (s IDsusan_principle__tmp1) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_j))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (5 # 68) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      - (1 # 5) * (s IDsusan_principle_j) * max0(-4
                                                                 + (s IDsusan_principle__tmp1)
                                                                 - (s IDsusan_principle_j))
                      + (1 # 2) * (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (12 # 25) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (4 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      - (1 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))^2
                      - (25 # 62) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 63%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 77) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 20) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 5) * (s IDsusan_principle__tmp1) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_j))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (5 # 68) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      - (1 # 5) * (s IDsusan_principle_j) * max0(-4
                                                                 + (s IDsusan_principle__tmp1)
                                                                 - (s IDsusan_principle_j))
                      + (1 # 2) * (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (12 # 25) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (4 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      - (1 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))^2
                      - (25 # 62) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 64%positive => ((3 # 2) - (1 # 6) * (s IDsusan_principle__tmp)
                      - (10 # 81) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 77) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 20) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (4 # 81) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 5) * (s IDsusan_principle__tmp1) * max0(-4
                                                                    + 
                                                                    (s IDsusan_principle__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_j))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (7 # 81) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (5 # 68) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      - (1 # 5) * (s IDsusan_principle_j) * max0(-4
                                                                 + (s IDsusan_principle__tmp1)
                                                                 - (s IDsusan_principle_j))
                      + (1 # 2) * (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (40 # 81) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 27) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (12 # 25) * max0(-4 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (4 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))
                      - (1 # 5) * max0(-4 + (s IDsusan_principle__tmp1)
                                       - (s IDsusan_principle_j))^2
                      - (25 # 62) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))%Q
    | 65%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + max0(-4 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 66%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + max0(-4 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 67%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + max0(-4 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 68%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 69%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 70%positive => ((3 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0((s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0((s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (24 # 85) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (3 # 52) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0((s IDsusan_principle_z)))%Q
    | 71%positive => ((1 # 2)
                      - (1 # 154) * (s IDsusan_principle__tmp) * max0(-7
                                                                    + (s IDsusan_principle__tmp))
                      - (11 # 94) * (s IDsusan_principle__tmp) * max0(-6
                                                                    + (s IDsusan_principle__tmp))
                      + (10 # 81) * (s IDsusan_principle__tmp) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 23) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_i))
                      - (3 # 82) * (s IDsusan_principle__tmp) * max0(-3
                                                                    + 
                                                                    (s IDsusan_principle_i))
                      + (1 # 6) * (s IDsusan_principle__tmp) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_z))
                      + (1 # 6) * (s IDsusan_principle_i)
                      + (10 # 81) * (s IDsusan_principle_i) * max0(-6
                                                                   + 
                                                                   (s IDsusan_principle__tmp))
                      - (2 # 25) * (s IDsusan_principle_i) * max0(-4
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (7 # 81) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle__tmp)
                                                                  - (s IDsusan_principle_i))
                      + (2 # 25) * (s IDsusan_principle_i) * max0(-3
                                                                  + (s IDsusan_principle_i))
                      - (1 # 6) * (s IDsusan_principle_i) * max0(-1
                                                                 + (s IDsusan_principle_z))
                      + (s IDsusan_principle_z)
                      - (1 # 6) * (s IDsusan_principle_z) * max0(-6
                                                                 + (s IDsusan_principle__tmp))
                      + (1 # 6) * (s IDsusan_principle_z) * max0(-3
                                                                 + (s IDsusan_principle_i))
                      + (1 # 26) * max0(-7 + (s IDsusan_principle__tmp))
                      + (22 # 49) * max0(-6 + (s IDsusan_principle__tmp))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      - (1 # 23) * max0(-6 + (s IDsusan_principle__tmp)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (10 # 81) * max0(-6 + (s IDsusan_principle__tmp)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-6 + (s IDsusan_principle__tmp1)) * max0(-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))
                      + (1 # 2) * max0(-4 + (s IDsusan_principle__tmp)
                                       - (s IDsusan_principle_i))
                      + (7 # 81) * max0(-4 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      - (48 # 121) * max0(-3 + (s IDsusan_principle__tmp)
                                          - (s IDsusan_principle_i))
                      - (1 # 78) * max0(-3 + (s IDsusan_principle__tmp)
                                        - (s IDsusan_principle_i)) * max0(-3
                                                                    + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle__tmp)
                                         - (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      + max0(-3 + (s IDsusan_principle__tmp1)
                             - (s IDsusan_principle_j))
                      - (24 # 107) * max0(-3 + (s IDsusan_principle_i))
                      + (10 # 81) * max0(-3 + (s IDsusan_principle_i)) * max0(3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))
                      - (1 # 2) * max0(-1 + (s IDsusan_principle_z)))%Q
    | _ => (0 # 1)%Q
  end.

Definition susan_principle_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-3
                                                            + (s IDsusan_principle__tmp)
                                                            - (s IDsusan_principle_i)) (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)));
                     (*-1 0*) F_max0_ge_0 (-4 + (s IDsusan_principle__tmp)
                                           - (s IDsusan_principle_i));
                     (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp1))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                     (*-0.123457 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))]
    | 10%positive => []
    | 11%positive => [(*0 0.037037*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*0 0.0864198*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*0 0.123457*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0));
                      (*0 0.123457*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement (-3
                                                     + (s IDsusan_principle__tmp)
                                                     - (s IDsusan_principle_i)) (1);
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)))]
    | 15%positive => []
    | 16%positive => [(*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - (s IDsusan_principle__tmp)
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.123457 0*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                 + (s IDsusan_principle__tmp1)
                                                                 - (s IDsusan_principle_j))) (F_check_ge (0) (0))]
    | 23%positive => [(*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))]
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
    | 61%positive => [(*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_z))) (F_check_ge ((s IDsusan_principle_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_z)))]
    | 62%positive => []
    | 63%positive => [(*-1 0*) F_max0_pre_decrement (-3
                                                     + (s IDsusan_principle__tmp1)
                                                     - (s IDsusan_principle_j)) (1);
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_z))) (F_check_ge ((s IDsusan_principle_z)) (0));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))]
    | 64%positive => [(*0 1*) F_max0_pre_decrement (-3
                                                    + (s IDsusan_principle__tmp1)
                                                    - (s IDsusan_principle_j)) (1);
                      (*0 0.00641026*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*0 0.0128205*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_z))) (F_check_ge ((s IDsusan_principle_z)) (0));
                      (*-0.0128205 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)));
                      (*-0.00641026 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)));
                      (*0 0.00641026*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)));
                      (*-0.173077 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0));
                      (*-0.00641026 0*) F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))]
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + (s IDsusan_principle__tmp)) (0))) (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp1)
                                                                    - (s IDsusan_principle_j))) (F_check_ge (0) (0)));
                      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_z))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_z))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle__tmp)
                                                                    - (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_z))) (F_check_ge ((s IDsusan_principle_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + (s IDsusan_principle__tmp))) (F_check_ge (0) (0)));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDsusan_principle_i)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_i)))]
    | _ => []
  end.


Theorem susan_principle_ai_correct:
  forall s p' s', steps (g_start susan_principle) s (g_edges susan_principle) p' s' -> susan_principle_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem susan_principle_pot_correct:
  forall s p' s',
    steps (g_start susan_principle) s (g_edges susan_principle) p' s' ->
    (susan_principle_pot (g_start susan_principle) s >= susan_principle_pot p' s')%Q.
Proof.
  check_lp susan_principle_ai_correct susan_principle_hints.
Qed.

