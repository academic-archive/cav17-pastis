Require Import pasta.Pasta.

Notation IDzfdct_byte_params_z := 1%positive.
Notation IDzfdct_byte_params__tmp := 2%positive.
Notation IDzfdct_byte_params__tmp1 := 3%positive.
Notation IDzfdct_byte_params__tmp2 := 4%positive.
Notation IDzfdct_byte_params_i := 5%positive.
Notation IDzfdct_byte_params_count := 6%positive.
Notation IDzfdct_byte_params_op := 7%positive.
Notation IDzfdct_byte_params_pvals := 8%positive.
Notation IDzfdct_byte_params_start := 9%positive.
Definition zfdct_byte_params : graph := {|
  g_start := 1%positive;
  g_end := 96%positive;
  g_edges := (1%positive,(AAssign IDzfdct_byte_params_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDzfdct_byte_params__tmp2
             (Some (EVar IDzfdct_byte_params_start))),3%positive)::
             (3%positive,(AAssign IDzfdct_byte_params__tmp1
             (Some (EVar IDzfdct_byte_params_count))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,93%positive)::(5%positive,ANone,73%positive)::
             (5%positive,ANone,28%positive)::(5%positive,ANone,7%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,8%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,12%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-7)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,96%positive)::
             (12%positive,(AAssign IDzfdct_byte_params_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) <
             (eval (EVar IDzfdct_byte_params__tmp2) s))%Z)),18%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) >=
             (eval (EVar IDzfdct_byte_params__tmp2) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,34%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,21%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDzfdct_byte_params_i
             (Some (EAdd (EVar IDzfdct_byte_params_i) (ENum (1))))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDzfdct_byte_params_z
             (Some (EAdd (ENum (1)) (EVar IDzfdct_byte_params_z)))),
             27%positive)::(27%positive,AWeaken,15%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,33%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-7)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,96%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDzfdct_byte_params_i (Some (ENum (0)))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) <
             (eval (EVar IDzfdct_byte_params__tmp1) s))%Z)),42%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) >=
             (eval (EVar IDzfdct_byte_params__tmp1) s))%Z)),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (0)))),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,96%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,70%positive)::
             (43%positive,ANone,52%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,49%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,49%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,57%positive)::
             (49%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-15)))),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,96%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,67%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,67%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,61%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,62%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,(AAssign IDzfdct_byte_params_i
             (Some (EAdd (EVar IDzfdct_byte_params_i) (ENum (1))))),
             63%positive)::(63%positive,ANone,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDzfdct_byte_params_z
             (Some (EAdd (ENum (1)) (EVar IDzfdct_byte_params_z)))),
             66%positive)::(66%positive,AWeaken,37%positive)::
             (67%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-15)))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,AWeaken,96%positive)::
             (70%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-20)))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,96%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,ANone,78%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-7)))),76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,AWeaken,96%positive)::
             (78%positive,(AAssign IDzfdct_byte_params_i (Some (ENum (0)))),
             79%positive)::(79%positive,ANone,80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) <
             (eval (EVar IDzfdct_byte_params__tmp1) s))%Z)),86%positive)::
             (81%positive,(AGuard
             (fun s => ((eval (EVar IDzfdct_byte_params_i) s) >=
             (eval (EVar IDzfdct_byte_params__tmp1) s))%Z)),82%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (0)))),84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,AWeaken,96%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,(AAssign IDzfdct_byte_params_i
             (Some (EAdd (EVar IDzfdct_byte_params_i) (ENum (1))))),
             89%positive)::(89%positive,ANone,90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,(AAssign IDzfdct_byte_params_z
             (Some (EAdd (ENum (1)) (EVar IDzfdct_byte_params_z)))),
             92%positive)::(92%positive,AWeaken,81%positive)::
             (93%positive,(AAssign IDzfdct_byte_params__tmp
             (Some (ENum (-20)))),94%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,AWeaken,96%positive)::nil
|}.

Definition zfdct_byte_params_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 4%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 6%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 7%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 8%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 9%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 10%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -7 <= 0)%Z
    | 11%positive => (-1 * (s IDzfdct_byte_params__tmp) + -7 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 12%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 13%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 14%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 15%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 16%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp2)+ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 17%positive => (1 * (s IDzfdct_byte_params__tmp2)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 18%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 20%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 23%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 25%positive => (-1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 26%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 27%positive => (-1 * (s IDzfdct_byte_params__tmp2)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 29%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 30%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 31%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -7 <= 0)%Z
    | 32%positive => (-1 * (s IDzfdct_byte_params__tmp) + -7 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 33%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 34%positive => (-1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 35%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 36%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 37%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 38%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 39%positive => (1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 40%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDzfdct_byte_params__tmp) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 42%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 44%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 46%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 48%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 15 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -15 <= 0)%Z
    | 51%positive => (-1 * (s IDzfdct_byte_params__tmp) + -15 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 15 <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 54%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 56%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 58%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 60%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 63%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 65%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 15 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -15 <= 0)%Z
    | 69%positive => (-1 * (s IDzfdct_byte_params__tmp) + -15 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 15 <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 70%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 20 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -20 <= 0)%Z
    | 72%positive => (-1 * (s IDzfdct_byte_params__tmp) + -20 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 20 <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 73%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 74%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 75%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 76%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -7 <= 0)%Z
    | 77%positive => (-1 * (s IDzfdct_byte_params__tmp) + -7 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 7 <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 78%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 79%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 80%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 81%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 82%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 83%positive => (1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 84%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) <= 0)%Z
    | 85%positive => (-1 * (s IDzfdct_byte_params__tmp) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp1)+ -1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 86%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 87%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 88%positive => (-1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) + 1 <= 0)%Z
    | 89%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 90%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 91%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0)%Z
    | 92%positive => (-1 * (s IDzfdct_byte_params__tmp1)+ 1 * (s IDzfdct_byte_params_i) <= 0 /\ -1 * (s IDzfdct_byte_params_i) + 1 <= 0 /\ -1 * (s IDzfdct_byte_params_z) + 1 <= 0)%Z
    | 93%positive => (1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 94%positive => (-1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 20 <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -20 <= 0)%Z
    | 95%positive => (-1 * (s IDzfdct_byte_params__tmp) + -20 <= 0 /\ 1 * (s IDzfdct_byte_params__tmp) + 20 <= 0 /\ 1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0)%Z
    | 96%positive => (1 * (s IDzfdct_byte_params__tmp) <= 0 /\ -1 * (s IDzfdct_byte_params_z) <= 0 /\ -1 * (s IDzfdct_byte_params__tmp) + -20 <= 0)%Z
    | _ => False
  end.

Definition zfdct_byte_params_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDzfdct_byte_params_count))
                     + max0((s IDzfdct_byte_params_start)))%Q
    | 2%positive => (max0((s IDzfdct_byte_params_count))
                     + max0((s IDzfdct_byte_params_start))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 3%positive => (max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_count))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 4%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 5%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 6%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 7%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 8%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 9%positive => (max0((s IDzfdct_byte_params__tmp1))
                     + max0((s IDzfdct_byte_params__tmp2))
                     + max0((s IDzfdct_byte_params_z)))%Q
    | 10%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 11%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 12%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 13%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 14%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 15%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 16%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 17%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 18%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 19%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 20%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 21%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 22%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 23%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 24%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 25%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 26%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 27%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 28%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 29%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 30%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 31%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 32%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 33%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 34%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)))%Q
    | 35%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 36%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 37%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 38%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 39%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 40%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 41%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 42%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 43%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 44%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 45%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 46%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 47%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 48%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 49%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 50%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 8) * max0(-7 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 51%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 8) * max0(-7 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 52%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 53%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 54%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 55%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 56%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 57%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 58%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 59%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 60%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 61%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 62%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 63%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 64%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 65%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 66%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 67%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 68%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 8) * max0(-7 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 69%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 8) * max0(-7 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 70%positive => ((1 # 1) - (s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 71%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 5) * max0(-15 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 72%positive => (-(s IDzfdct_byte_params__tmp1)
                      + (s IDzfdct_byte_params_i) + (s IDzfdct_byte_params_z)
                      + (1 # 5) * max0(-15 - (s IDzfdct_byte_params__tmp))
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 73%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 74%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)))%Q
    | 75%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)))%Q
    | 76%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)))%Q
    | 77%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)))%Q
    | 78%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2)))%Q
    | 79%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 80%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 81%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 82%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 83%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 84%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 85%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 86%positive => ((s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i)))%Q
    | 87%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 88%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0(-1 + (s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0(-1 + (s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 89%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 90%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 91%positive => ((1 # 1) + (s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 92%positive => ((s IDzfdct_byte_params_z)
                      + max0((s IDzfdct_byte_params__tmp1)
                             - (s IDzfdct_byte_params_i))
                      + max0((s IDzfdct_byte_params__tmp2)
                             - (s IDzfdct_byte_params_i)))%Q
    | 93%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 94%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 95%positive => (max0((s IDzfdct_byte_params__tmp1))
                      + max0((s IDzfdct_byte_params__tmp2))
                      + max0((s IDzfdct_byte_params_z)))%Q
    | 96%positive => ((s IDzfdct_byte_params_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zfdct_byte_params_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp1));
                      (*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp2));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params_z))) (F_check_ge ((s IDzfdct_byte_params_z)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzfdct_byte_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzfdct_byte_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzfdct_byte_params_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzfdct_byte_params_z)))]
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzfdct_byte_params__tmp2)
                                                             - (s IDzfdct_byte_params_i)) (-1
                                                                    + (s IDzfdct_byte_params__tmp2)
                                                                    - (s IDzfdct_byte_params_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDzfdct_byte_params__tmp2)
                                            - (s IDzfdct_byte_params_i))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_pre_decrement ((s IDzfdct_byte_params__tmp2)
                                                     - (s IDzfdct_byte_params_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp2));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params_z))) (F_check_ge ((s IDzfdct_byte_params_z)) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp1))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzfdct_byte_params__tmp1)
                                                             - (s IDzfdct_byte_params_i)) (-1
                                                                    + (s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDzfdct_byte_params__tmp1)
                                            - (s IDzfdct_byte_params_i))]
    | 42%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)))]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params__tmp1)
                                                                   - 
                                                                   (s IDzfdct_byte_params_i))) (F_check_ge ((s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDzfdct_byte_params__tmp1)
                                                                - (s IDzfdct_byte_params_i))) (F_check_ge (0) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - 
                                                                    (s IDzfdct_byte_params__tmp))) (F_check_ge (0) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params__tmp1)
                                                                   - 
                                                                   (s IDzfdct_byte_params_i))) (F_check_ge ((s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)) (0))]
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
    | 69%positive => [(*-1 0*) F_max0_pre_decrement ((s IDzfdct_byte_params__tmp1)
                                                     - (s IDzfdct_byte_params_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDzfdct_byte_params__tmp1)
                                            - (s IDzfdct_byte_params_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDzfdct_byte_params__tmp1)
                                                                  - (s IDzfdct_byte_params_i))) (F_check_ge (-1
                                                                    + (s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - 
                                                                    (s IDzfdct_byte_params__tmp))) (F_check_ge (0) (0))]
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*0 1*) F_max0_ge_0 (-1 + (s IDzfdct_byte_params__tmp1)
                                           - (s IDzfdct_byte_params_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params__tmp1)
                                                                  - (s IDzfdct_byte_params_i))) (F_check_ge ((s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)) (0));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15
                                                                   - 
                                                                   (s IDzfdct_byte_params__tmp))) (F_check_ge (0) (0))]
    | 73%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzfdct_byte_params_z))) (F_check_ge ((s IDzfdct_byte_params_z)) (0))]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => [(*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp1));
                      (*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp2))]
    | 78%positive => []
    | 79%positive => []
    | 80%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzfdct_byte_params__tmp2)
                                                             - (s IDzfdct_byte_params_i)) (-1
                                                                    + (s IDzfdct_byte_params__tmp2)
                                                                    - (s IDzfdct_byte_params_i)))]
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzfdct_byte_params__tmp1)
                                                             - (s IDzfdct_byte_params_i)) (-1
                                                                    + (s IDzfdct_byte_params__tmp1)
                                                                    - (s IDzfdct_byte_params_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDzfdct_byte_params__tmp1)
                                            - (s IDzfdct_byte_params_i));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDzfdct_byte_params__tmp2)
                                            - (s IDzfdct_byte_params_i))]
    | 86%positive => [(*-1 0*) F_max0_pre_decrement ((s IDzfdct_byte_params__tmp1)
                                                     - (s IDzfdct_byte_params_i)) (1)]
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzfdct_byte_params__tmp2)
                                                             - (s IDzfdct_byte_params_i)) (-1
                                                                    + (s IDzfdct_byte_params__tmp2)
                                                                    - (s IDzfdct_byte_params_i)))]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => [(*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp1));
                      (*-1 0*) F_max0_ge_0 ((s IDzfdct_byte_params__tmp2));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzfdct_byte_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzfdct_byte_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzfdct_byte_params_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzfdct_byte_params_z)))]
    | 96%positive => []
    | _ => []
  end.


Theorem zfdct_byte_params_ai_correct:
  forall s p' s', steps (g_start zfdct_byte_params) s (g_edges zfdct_byte_params) p' s' -> zfdct_byte_params_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zfdct_byte_params_pot_correct:
  forall s p' s',
    steps (g_start zfdct_byte_params) s (g_edges zfdct_byte_params) p' s' ->
    (zfdct_byte_params_pot (g_start zfdct_byte_params) s >= zfdct_byte_params_pot p' s')%Q.
Proof.
  check_lp zfdct_byte_params_ai_correct zfdct_byte_params_hints.
Qed.

