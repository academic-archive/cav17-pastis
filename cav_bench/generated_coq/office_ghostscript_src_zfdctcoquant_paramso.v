Require Import pasta.Pasta.

Notation IDquant_params_z := 1%positive.
Notation IDquant_params__tmp := 2%positive.
Notation IDquant_params__tmp1 := 3%positive.
Notation IDquant_params_i := 4%positive.
Notation IDquant_params_QFactor := 5%positive.
Notation IDquant_params_count := 6%positive.
Notation IDquant_params_op := 7%positive.
Notation IDquant_params_pvals := 8%positive.
Definition quant_params : graph := {|
  g_start := 1%positive;
  g_end := 80%positive;
  g_edges := (1%positive,(AAssign IDquant_params_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDquant_params__tmp1
             (Some (EVar IDquant_params_count))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,ANone,77%positive)::(4%positive,ANone,51%positive)::
             (4%positive,ANone,12%positive)::(4%positive,ANone,6%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,7%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,11%positive)::(7%positive,ANone,8%positive)::
             (8%positive,(AAssign IDquant_params__tmp (Some (ENum (-7)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,80%positive)::
             (11%positive,ANone,18%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,17%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDquant_params__tmp (Some (ENum (-7)))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,80%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDquant_params_i (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDquant_params_i)
             s) < (eval (EVar IDquant_params__tmp1) s))%Z)),26%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDquant_params_i)
             s) >= (eval (EVar IDquant_params__tmp1) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDquant_params__tmp (Some (ENum (0)))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,AWeaken,80%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,48%positive)::
             (27%positive,ANone,30%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,32%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,34%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,36%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (36%positive,ANone,38%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,42%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,43%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDquant_params_i
             (Some (EAdd (EVar IDquant_params_i) (ENum (1))))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDquant_params_z (Some (EAdd (ENum (1))
             (EVar IDquant_params_z)))),47%positive)::
             (47%positive,AWeaken,21%positive)::
             (48%positive,(AAssign IDquant_params__tmp (Some (ENum (-20)))),
             49%positive)::(49%positive,ANone,50%positive)::
             (50%positive,AWeaken,80%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,56%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDquant_params__tmp (Some (ENum (-7)))),
             54%positive)::(54%positive,ANone,55%positive)::
             (55%positive,AWeaken,80%positive)::
             (56%positive,(AAssign IDquant_params_i (Some (ENum (0)))),
             57%positive)::(57%positive,ANone,58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AGuard (fun s => ((eval (EVar IDquant_params_i)
             s) < (eval (EVar IDquant_params__tmp1) s))%Z)),64%positive)::
             (59%positive,(AGuard (fun s => ((eval (EVar IDquant_params_i)
             s) >= (eval (EVar IDquant_params__tmp1) s))%Z)),60%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AAssign IDquant_params__tmp (Some (ENum (0)))),
             62%positive)::(62%positive,ANone,63%positive)::
             (63%positive,AWeaken,80%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,67%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,69%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (69%positive,ANone,71%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,(AAssign IDquant_params_i
             (Some (EAdd (EVar IDquant_params_i) (ENum (1))))),73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDquant_params_z (Some (EAdd (ENum (1))
             (EVar IDquant_params_z)))),76%positive)::
             (76%positive,AWeaken,59%positive)::
             (77%positive,(AAssign IDquant_params__tmp (Some (ENum (-20)))),
             78%positive)::(78%positive,ANone,79%positive)::
             (79%positive,AWeaken,80%positive)::nil
|}.

Definition quant_params_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 3%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 4%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 5%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 6%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 7%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 8%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 9%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params__tmp) + -7 <= 0)%Z
    | 10%positive => (-1 * (s IDquant_params__tmp) + -7 <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 11%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 12%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 13%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 14%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 15%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params__tmp) + -7 <= 0)%Z
    | 16%positive => (-1 * (s IDquant_params__tmp) + -7 <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 17%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 18%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 19%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 20%positive => (-1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 21%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 22%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0)%Z
    | 23%positive => (1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 24%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params__tmp) <= 0 /\ -1 * (s IDquant_params__tmp) <= 0)%Z
    | 25%positive => (-1 * (s IDquant_params__tmp) <= 0 /\ 1 * (s IDquant_params__tmp) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 26%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 28%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 30%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 32%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 34%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 35%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 37%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 39%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 41%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 44%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 46%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params__tmp) + 20 <= 0 /\ -1 * (s IDquant_params__tmp) + -20 <= 0)%Z
    | 50%positive => (-1 * (s IDquant_params__tmp) + -20 <= 0 /\ 1 * (s IDquant_params__tmp) + 20 <= 0 /\ -1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 52%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 53%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 54%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params__tmp) + -7 <= 0)%Z
    | 55%positive => (-1 * (s IDquant_params__tmp) + -7 <= 0 /\ 1 * (s IDquant_params__tmp) + 7 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 56%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 57%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 58%positive => (-1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 59%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 60%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0)%Z
    | 61%positive => (1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 62%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ 1 * (s IDquant_params__tmp) <= 0 /\ -1 * (s IDquant_params__tmp) <= 0)%Z
    | 63%positive => (-1 * (s IDquant_params__tmp) <= 0 /\ 1 * (s IDquant_params__tmp) <= 0 /\ 1 * (s IDquant_params__tmp1)+ -1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 64%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 66%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 69%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 70%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 71%positive => (-1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_i) <= 0)%Z
    | 73%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) <= 0)%Z
    | 75%positive => (-1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_i) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDquant_params_i) + 1 <= 0 /\ -1 * (s IDquant_params__tmp1)+ 1 * (s IDquant_params_i) <= 0 /\ -1 * (s IDquant_params_z) + 1 <= 0)%Z
    | 77%positive => (-1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 78%positive => (1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params__tmp) + 20 <= 0 /\ -1 * (s IDquant_params__tmp) + -20 <= 0)%Z
    | 79%positive => (-1 * (s IDquant_params__tmp) + -20 <= 0 /\ 1 * (s IDquant_params__tmp) + 20 <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ 1 * (s IDquant_params_z) <= 0)%Z
    | 80%positive => (1 * (s IDquant_params__tmp) <= 0 /\ -1 * (s IDquant_params_z) <= 0 /\ -1 * (s IDquant_params__tmp) + -20 <= 0)%Z
    | _ => False
  end.

Definition quant_params_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDquant_params_count)))%Q
    | 2%positive => (max0((s IDquant_params_count))
                     + max0((s IDquant_params_z)))%Q
    | 3%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 4%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 5%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 6%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 7%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 8%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 9%positive => (max0((s IDquant_params__tmp1))
                     + max0((s IDquant_params_z)))%Q
    | 10%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 11%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 12%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 13%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 14%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 15%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 16%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 17%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 18%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 19%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 20%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 21%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 22%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 23%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 24%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 25%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 26%positive => (max0((s IDquant_params__tmp1) - (s IDquant_params_i))
                      + max0((s IDquant_params_z)))%Q
    | 27%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 28%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 29%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 30%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 31%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 32%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 33%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 34%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 35%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 36%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 37%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 38%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 39%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 40%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 41%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 42%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 43%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i)))%Q
    | 44%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 45%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 46%positive => ((1 # 1) + (s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 47%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 48%positive => ((1 # 1) - (s IDquant_params__tmp1)
                      + (s IDquant_params_i) + (s IDquant_params_z)
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 49%positive => (-(s IDquant_params__tmp1) + (s IDquant_params_i)
                      + (s IDquant_params_z)
                      + (1 # 13) * max0(-7 - (s IDquant_params__tmp))
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 50%positive => (-(s IDquant_params__tmp1) + (s IDquant_params_i)
                      + (s IDquant_params_z)
                      + (1 # 13) * max0(-7 - (s IDquant_params__tmp))
                      + max0(-1 + (s IDquant_params__tmp1)
                             - (s IDquant_params_i))
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 51%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 52%positive => ((s IDquant_params_z) + max0((s IDquant_params__tmp1)))%Q
    | 53%positive => ((s IDquant_params_z) + max0((s IDquant_params__tmp1)))%Q
    | 54%positive => ((s IDquant_params_z) + max0((s IDquant_params__tmp1)))%Q
    | 55%positive => ((s IDquant_params_z) + max0((s IDquant_params__tmp1)))%Q
    | 56%positive => ((s IDquant_params_z) + max0((s IDquant_params__tmp1)))%Q
    | 57%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 58%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 59%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 60%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 61%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 62%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 63%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 64%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 65%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 66%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 67%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 68%positive => ((s IDquant_params_z)
                      + max0((s IDquant_params__tmp1) - (s IDquant_params_i)))%Q
    | 69%positive => ((s IDquant_params__tmp1) - (s IDquant_params_i)
                      + (s IDquant_params_z))%Q
    | 70%positive => ((s IDquant_params__tmp1) - (s IDquant_params_i)
                      + (s IDquant_params_z))%Q
    | 71%positive => ((s IDquant_params__tmp1) - (s IDquant_params_i)
                      + (s IDquant_params_z))%Q
    | 72%positive => ((s IDquant_params__tmp1) - (s IDquant_params_i)
                      + (s IDquant_params_z))%Q
    | 73%positive => ((1 # 1) + (s IDquant_params__tmp1)
                      - (s IDquant_params_i) + (s IDquant_params_z))%Q
    | 74%positive => ((1 # 1) + (s IDquant_params__tmp1)
                      - (s IDquant_params_i) + (s IDquant_params_z))%Q
    | 75%positive => ((1 # 1) + (s IDquant_params__tmp1)
                      - (s IDquant_params_i) + (s IDquant_params_z))%Q
    | 76%positive => ((s IDquant_params__tmp1) - (s IDquant_params_i)
                      + (s IDquant_params_z))%Q
    | 77%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 78%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 79%positive => (max0((s IDquant_params__tmp1))
                      + max0((s IDquant_params_z)))%Q
    | 80%positive => ((s IDquant_params_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition quant_params_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_ge_0 ((s IDquant_params__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params_z))) (F_check_ge ((s IDquant_params_z)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_ge_0 ((s IDquant_params__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params_z))) (F_check_ge ((s IDquant_params_z)) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDquant_params__tmp1)
                                                             - (s IDquant_params_i)) (-1
                                                                    + (s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDquant_params__tmp1)
                                            - (s IDquant_params_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params_z))) (F_check_ge ((s IDquant_params_z)) (0))]
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params_z))) (F_check_ge ((s IDquant_params_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params__tmp1)
                                                                   - 
                                                                   (s IDquant_params_i))) (F_check_ge ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))]
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_pre_decrement ((s IDquant_params__tmp1)
                                                     - (s IDquant_params_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDquant_params__tmp1)
                                                                  - (s IDquant_params_i))) (F_check_ge (-1
                                                                    + (s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))]
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
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_params_z)) (0))) (F_max0_ge_0 ((s IDquant_params_z)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDquant_params__tmp1)
                                            - (s IDquant_params_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params__tmp1)
                                                                  - (s IDquant_params_i))) (F_check_ge ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0));
                      (*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - (s IDquant_params__tmp))) (F_check_ge (0) (0))]
    | 51%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params_z))) (F_check_ge ((s IDquant_params_z)) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-1 0*) F_max0_ge_0 ((s IDquant_params__tmp1))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDquant_params__tmp1)
                                                             - (s IDquant_params_i)) (-1
                                                                    + (s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDquant_params__tmp1)
                                            - (s IDquant_params_i))]
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params__tmp1)
                                                                   - 
                                                                   (s IDquant_params_i))) (F_check_ge ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))]
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_params__tmp1)
                                                                   - 
                                                                   (s IDquant_params_i))) (F_check_ge ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)) (0))) (F_max0_ge_0 ((s IDquant_params__tmp1)
                                                                    - (s IDquant_params_i)))]
    | 77%positive => []
    | 78%positive => []
    | 79%positive => [(*-1 0*) F_max0_ge_0 ((s IDquant_params__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDquant_params_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_params_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_params_z)))]
    | 80%positive => []
    | _ => []
  end.


Theorem quant_params_ai_correct:
  forall s p' s', steps (g_start quant_params) s (g_edges quant_params) p' s' -> quant_params_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem quant_params_pot_correct:
  forall s p' s',
    steps (g_start quant_params) s (g_edges quant_params) p' s' ->
    (quant_params_pot (g_start quant_params) s >= quant_params_pot p' s')%Q.
Proof.
  check_lp quant_params_ai_correct quant_params_hints.
Qed.

