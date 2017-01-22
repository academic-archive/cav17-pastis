Require Import pasta.Pasta.

Notation IDgdev_prn_render_pages_z := 1%positive.
Notation IDgdev_prn_render_pages__tmp := 2%positive.
Notation IDgdev_prn_render_pages__tmp1 := 3%positive.
Notation IDgdev_prn_render_pages_code := 4%positive.
Notation IDgdev_prn_render_pages_i := 5%positive.
Notation IDgdev_prn_render_pages_i1 := 6%positive.
Notation IDgdev_prn_render_pages_count := 7%positive.
Notation IDgdev_prn_render_pages_pdev := 8%positive.
Notation IDgdev_prn_render_pages_ppages := 9%positive.
Definition gdev_prn_render_pages : graph := {|
  g_start := 1%positive;
  g_end := 59%positive;
  g_edges := (1%positive,(AAssign IDgdev_prn_render_pages_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgdev_prn_render_pages__tmp
             (Some (EVar IDgdev_prn_render_pages_count))),3%positive)::
             (3%positive,(AAssign IDgdev_prn_render_pages_i
             (Some (ENum (0)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i) s) <
             (eval (EVar IDgdev_prn_render_pages__tmp) s))%Z)),24%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i) s) >=
             (eval (EVar IDgdev_prn_render_pages__tmp) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDgdev_prn_render_pages_code None),
             9%positive)::
             (9%positive,(AAssign IDgdev_prn_render_pages_i1
             (Some (ENum (0)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i1) s) <
             (eval (EVar IDgdev_prn_render_pages__tmp) s))%Z)),17%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i1) s) >=
             (eval (EVar IDgdev_prn_render_pages__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDgdev_prn_render_pages__tmp1
             (Some (EVar IDgdev_prn_render_pages_code))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,59%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDgdev_prn_render_pages_i1
             (Some (EAdd (EVar IDgdev_prn_render_pages_i1) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDgdev_prn_render_pages_z
             (Some (EAdd (ENum (1)) (EVar IDgdev_prn_render_pages_z)))),
             23%positive)::(23%positive,AWeaken,12%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,56%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,56%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,53%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,50%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,50%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i) s) =
             (eval (ENum (0)) s))%Z)),42%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_prn_render_pages_i) s) <>
             (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,39%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,44%positive)::
             (39%positive,(AAssign IDgdev_prn_render_pages__tmp1
             (Some (ENum (-15)))),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,59%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDgdev_prn_render_pages_i
             (Some (EAdd (EVar IDgdev_prn_render_pages_i) (ENum (1))))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDgdev_prn_render_pages_z
             (Some (EAdd (ENum (1)) (EVar IDgdev_prn_render_pages_z)))),
             49%positive)::(49%positive,AWeaken,6%positive)::
             (50%positive,(AAssign IDgdev_prn_render_pages__tmp1
             (Some (ENum (-15)))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,59%positive)::
             (53%positive,(AAssign IDgdev_prn_render_pages__tmp1
             (Some (ENum (-15)))),54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,59%positive)::
             (56%positive,(AAssign IDgdev_prn_render_pages__tmp1
             (Some (ENum (-15)))),57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,AWeaken,59%positive)::nil
|}.

Definition gdev_prn_render_pages_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 4%positive => (1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 5%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 6%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 7%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 8%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 9%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 10%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0)%Z
    | 11%positive => (-1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ 1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 12%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 13%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i1) <= 0)%Z
    | 14%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i1) <= 0)%Z
    | 16%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 17%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 19%positive => (1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) <= 0)%Z
    | 21%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 22%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) <= 0)%Z
    | 23%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i1) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i1) + 1 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp)+ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 26%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 28%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 30%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 32%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 34%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 36%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 38%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0)%Z
    | 41%positive => (-1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ 1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 43%positive => (1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 44%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | 46%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0)%Z
    | 48%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0)%Z
    | 52%positive => (-1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0)%Z
    | 55%positive => (-1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0)%Z
    | 58%positive => (-1 * (s IDgdev_prn_render_pages__tmp1) + -15 <= 0 /\ 1 * (s IDgdev_prn_render_pages__tmp1) + 15 <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0 /\ -1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages__tmp)+ 1 * (s IDgdev_prn_render_pages_i) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDgdev_prn_render_pages_z) <= 0 /\ -1 * (s IDgdev_prn_render_pages_i) <= 0)%Z
    | _ => False
  end.

Definition gdev_prn_render_pages_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1) * max0((s IDgdev_prn_render_pages_count)))%Q
    | 2%positive => ((s IDgdev_prn_render_pages_z)
                     + (2 # 1) * max0((s IDgdev_prn_render_pages_count)))%Q
    | 3%positive => ((s IDgdev_prn_render_pages_z)
                     + (2 # 1) * max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 4%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 5%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 6%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 7%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 8%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 9%positive => ((s IDgdev_prn_render_pages_z)
                     + max0((s IDgdev_prn_render_pages__tmp))
                     + max0((s IDgdev_prn_render_pages__tmp)
                            - (s IDgdev_prn_render_pages_i)))%Q
    | 10%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 11%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 12%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 13%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 14%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 15%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 16%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 17%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 18%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 19%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 20%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 21%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 22%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 23%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i1)))%Q
    | 24%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 25%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 26%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 27%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 28%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 29%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 30%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 31%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 32%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 33%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 34%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 35%positive => ((1 # 1) + (s IDgdev_prn_render_pages__tmp)
                      + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 36%positive => ((1 # 1) + (s IDgdev_prn_render_pages__tmp)
                      + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 37%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 38%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 39%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 40%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 41%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 42%positive => ((1 # 1) + (s IDgdev_prn_render_pages__tmp)
                      + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 43%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 44%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 45%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 46%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 47%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 48%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 49%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 50%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 51%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 52%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 53%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 54%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 55%positive => ((1 # 1) + (s IDgdev_prn_render_pages_z)
                      + max0(-1 + (s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i))
                      + max0((s IDgdev_prn_render_pages__tmp)))%Q
    | 56%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 57%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 58%positive => ((s IDgdev_prn_render_pages_z)
                      + max0((s IDgdev_prn_render_pages__tmp))
                      + max0((s IDgdev_prn_render_pages__tmp)
                             - (s IDgdev_prn_render_pages_i)))%Q
    | 59%positive => ((s IDgdev_prn_render_pages_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gdev_prn_render_pages_hints (p : node) (s : state) := 
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
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgdev_prn_render_pages__tmp)
                                                             - (s IDgdev_prn_render_pages_i)) (-1
                                                                    + (s IDgdev_prn_render_pages__tmp)
                                                                    - (s IDgdev_prn_render_pages_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_prn_render_pages__tmp)
                                            - (s IDgdev_prn_render_pages_i));
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDgdev_prn_render_pages__tmp)
                                                             - (s IDgdev_prn_render_pages_i1)) (-1
                                                                    + (s IDgdev_prn_render_pages__tmp)
                                                                    - (s IDgdev_prn_render_pages_i1)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_prn_render_pages__tmp)
                                            - (s IDgdev_prn_render_pages_i1))]
    | 17%positive => [(*-1 0*) F_max0_pre_decrement ((s IDgdev_prn_render_pages__tmp)
                                                     - (s IDgdev_prn_render_pages_i1)) (1)]
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
    | 28%positive => [(*0 1*) F_max0_pre_decrement ((s IDgdev_prn_render_pages__tmp)
                                                    - (s IDgdev_prn_render_pages_i)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_prn_render_pages__tmp))) (F_check_ge ((s IDgdev_prn_render_pages__tmp)) (0))]
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_prn_render_pages__tmp)) (0))) (F_max0_ge_0 ((s IDgdev_prn_render_pages__tmp)))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_prn_render_pages__tmp)
                                            - (s IDgdev_prn_render_pages_i));
                      (*-1 0*) F_max0_ge_0 ((s IDgdev_prn_render_pages__tmp))]
    | 42%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_prn_render_pages__tmp)) (0))) (F_max0_ge_0 ((s IDgdev_prn_render_pages__tmp)))]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_prn_render_pages_z))) (F_check_ge ((s IDgdev_prn_render_pages_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_prn_render_pages_z)) (0))) (F_max0_ge_0 ((s IDgdev_prn_render_pages_z)))]
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 ((s IDgdev_prn_render_pages__tmp));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDgdev_prn_render_pages__tmp)
                                                                - (s IDgdev_prn_render_pages_i))) (F_check_ge (0) (0))]
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_prn_render_pages__tmp)
                                            - (s IDgdev_prn_render_pages_i));
                      (*-1 0*) F_max0_ge_0 ((s IDgdev_prn_render_pages__tmp))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => [(*-2 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDgdev_prn_render_pages__tmp)
                                                     - (s IDgdev_prn_render_pages_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_prn_render_pages__tmp)
                                            - (s IDgdev_prn_render_pages_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_prn_render_pages__tmp))) (F_check_ge ((s IDgdev_prn_render_pages__tmp)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgdev_prn_render_pages__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgdev_prn_render_pages__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgdev_prn_render_pages__tmp)))]
    | 59%positive => []
    | _ => []
  end.


Theorem gdev_prn_render_pages_ai_correct:
  forall s p' s', steps (g_start gdev_prn_render_pages) s (g_edges gdev_prn_render_pages) p' s' -> gdev_prn_render_pages_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gdev_prn_render_pages_pot_correct:
  forall s p' s',
    steps (g_start gdev_prn_render_pages) s (g_edges gdev_prn_render_pages) p' s' ->
    (gdev_prn_render_pages_pot (g_start gdev_prn_render_pages) s >= gdev_prn_render_pages_pot p' s')%Q.
Proof.
  check_lp gdev_prn_render_pages_ai_correct gdev_prn_render_pages_hints.
Qed.

