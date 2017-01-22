Require Import pasta.Pasta.

Notation IDgdev_vector_write_polygon_z := 1%positive.
Notation IDgdev_vector_write_polygon__tmp := 2%positive.
Notation IDgdev_vector_write_polygon__tmp1 := 3%positive.
Notation IDgdev_vector_write_polygon__tmp2 := 4%positive.
Notation IDgdev_vector_write_polygon__tmp3 := 5%positive.
Notation IDgdev_vector_write_polygon_code := 6%positive.
Notation IDgdev_vector_write_polygon_i := 7%positive.
Notation IDgdev_vector_write_polygon_close := 8%positive.
Notation IDgdev_vector_write_polygon_count := 9%positive.
Notation IDgdev_vector_write_polygon_points := 10%positive.
Notation IDgdev_vector_write_polygon_type := 11%positive.
Notation IDgdev_vector_write_polygon_vdev := 12%positive.
Definition gdev_vector_write_polygon : graph := {|
  g_start := 1%positive;
  g_end := 69%positive;
  g_edges := (1%positive,(AAssign IDgdev_vector_write_polygon_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp2) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDgdev_vector_write_polygon__tmp2
             (Some (EVar IDgdev_vector_write_polygon_count))),6%positive)::
             (6%positive,(AAssign IDgdev_vector_write_polygon__tmp3
             (Some (EVar IDgdev_vector_write_polygon_close))),7%positive)::
             (7%positive,(AAssign IDgdev_vector_write_polygon__tmp
             (Some (EVar IDgdev_vector_write_polygon_type))),8%positive)::
             (8%positive,(AAssign IDgdev_vector_write_polygon_code
             (Some (ENum (0)))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp) s) <>
             (eval (ENum (0)) s))%Z)),12%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp) s) =
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,17%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDgdev_vector_write_polygon_code None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,ANone,66%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp2) s) >
             (eval (ENum (0)) s))%Z)),19%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp2) s) <=
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,47%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDgdev_vector_write_polygon_code None),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) >=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) <
             (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,37%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDgdev_vector_write_polygon_i
             (Some (ENum (1)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_i) s) <
             (eval (EVar IDgdev_vector_write_polygon__tmp2) s))%Z)),
             30%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_i) s) >=
             (eval (EVar IDgdev_vector_write_polygon__tmp2) s))%Z)),
             29%positive)::(29%positive,AWeaken,35%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) >=
             (eval (ENum (0)) s))%Z)),58%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) <
             (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) >=
             (eval (ENum (0)) s))%Z)),39%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) <
             (eval (ENum (0)) s))%Z)),38%positive)::
             (38%positive,AWeaken,45%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp3) s) <>
             (eval (ENum (0)) s))%Z)),42%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp3) s) =
             (eval (ENum (0)) s))%Z)),41%positive)::
             (41%positive,AWeaken,45%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDgdev_vector_write_polygon_code None),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) >=
             (eval (ENum (0)) s))%Z)),49%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon_code) s) <
             (eval (ENum (0)) s))%Z)),48%positive)::
             (48%positive,AWeaken,52%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp) s) <>
             (eval (ENum (0)) s))%Z)),53%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_vector_write_polygon__tmp) s) =
             (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,55%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDgdev_vector_write_polygon__tmp1 None),
             56%positive)::(56%positive,ANone,57%positive)::
             (57%positive,AWeaken,69%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AAssign IDgdev_vector_write_polygon_code None),
             60%positive)::(60%positive,ANone,61%positive)::
             (61%positive,(AAssign IDgdev_vector_write_polygon_i
             (Some (EAdd (EVar IDgdev_vector_write_polygon_i) (ENum (1))))),
             62%positive)::(62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDgdev_vector_write_polygon_z
             (Some (EAdd (ENum (1)) (EVar IDgdev_vector_write_polygon_z)))),
             65%positive)::(65%positive,AWeaken,28%positive)::
             (66%positive,(AAssign IDgdev_vector_write_polygon__tmp1
             (Some (EVar IDgdev_vector_write_polygon_code))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::nil
|}.

Definition gdev_vector_write_polygon_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) <= 0)%Z
    | 5%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 6%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 8%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 10%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 11%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon__tmp) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 13%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 14%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 16%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 17%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 18%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon__tmp2) <= 0)%Z
    | 19%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 21%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 25%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 26%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_i) + -1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ 1 * (s IDgdev_vector_write_polygon_i) + -1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 28%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 29%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon__tmp2)+ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 30%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 37%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 38%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 40%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 41%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon__tmp3) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp3) <= 0)%Z
    | 42%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 43%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 44%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 46%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 48%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_code) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 50%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 51%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ 1 * (s IDgdev_vector_write_polygon__tmp) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp) <= 0)%Z
    | 52%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 53%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 54%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 55%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 56%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 57%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 58%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_code) <= 0)%Z
    | 59%positive => (-1 * (s IDgdev_vector_write_polygon_code) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 2 <= 0)%Z
    | 63%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 2 <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 64%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) + 2 <= 0)%Z
    | 65%positive => (-1 * (s IDgdev_vector_write_polygon_i) + 2 <= 0 /\ -1 * (s IDgdev_vector_write_polygon__tmp2)+ 1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 67%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | 68%positive => (-1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ 1 * (s IDgdev_vector_write_polygon_z) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_i) <= 0)%Z
    | 69%positive => (-1 * (s IDgdev_vector_write_polygon_i) <= 0 /\ -1 * (s IDgdev_vector_write_polygon_z) <= 0)%Z
    | _ => False
  end.

Definition gdev_vector_write_polygon_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgdev_vector_write_polygon_count)))%Q
    | 2%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon_count)))%Q
    | 3%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon_count)))%Q
    | 4%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon_count)))%Q
    | 5%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon_count)))%Q
    | 6%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 7%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 8%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 9%positive => ((s IDgdev_vector_write_polygon_z)
                     + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 10%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 11%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 12%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 13%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 14%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 15%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 16%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 17%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 18%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 19%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 20%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 21%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 22%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z))%Q
    | 23%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z))%Q
    | 24%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z))%Q
    | 25%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z))%Q
    | 26%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 27%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 28%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 29%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 30%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 31%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 32%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 33%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 34%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 35%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 36%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 37%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 38%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 39%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 40%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 41%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 42%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 43%positive => (-(1 # 1) + (s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 44%positive => (-(1 # 1) + (s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 45%positive => (-(1 # 1) + (s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 46%positive => (-(1 # 1) + (s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 47%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 48%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 49%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 50%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 51%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 52%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 53%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 54%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 55%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 56%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 57%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | 58%positive => ((s IDgdev_vector_write_polygon__tmp2)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2))
                      + max0((s IDgdev_vector_write_polygon__tmp2)
                             - (s IDgdev_vector_write_polygon_i)))%Q
    | 59%positive => ((2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 60%positive => ((2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 61%positive => ((2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 62%positive => ((1 # 1)
                      + (2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 63%positive => ((1 # 1)
                      + (2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 64%positive => ((1 # 1)
                      + (2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 65%positive => ((2 # 1) * (s IDgdev_vector_write_polygon__tmp2)
                      - (s IDgdev_vector_write_polygon_i)
                      + (s IDgdev_vector_write_polygon_z)
                      - max0(-1 + (s IDgdev_vector_write_polygon__tmp2)))%Q
    | 66%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 67%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 68%positive => ((s IDgdev_vector_write_polygon_z)
                      + max0((s IDgdev_vector_write_polygon__tmp2)))%Q
    | 69%positive => ((s IDgdev_vector_write_polygon_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gdev_vector_write_polygon_hints (p : node) (s : state) := 
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
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgdev_vector_write_polygon__tmp2))) (F_check_ge (0) (0))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_vector_write_polygon__tmp2))) (F_check_ge ((s IDgdev_vector_write_polygon__tmp2)) (0))]
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_vector_write_polygon__tmp2))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDgdev_vector_write_polygon__tmp2)
                                                            - (s IDgdev_vector_write_polygon_i)) (-1
                                                                    + (s IDgdev_vector_write_polygon__tmp2)
                                                                    - (s IDgdev_vector_write_polygon_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgdev_vector_write_polygon__tmp2)
                                                                 - (s IDgdev_vector_write_polygon_i))) (F_check_ge (0) (0))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDgdev_vector_write_polygon__tmp2)
                                                     - (s IDgdev_vector_write_polygon_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgdev_vector_write_polygon__tmp2)
                                            - (s IDgdev_vector_write_polygon_i))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_one]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_one]
    | 42%positive => [(*-1 0*) F_one]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgdev_vector_write_polygon__tmp2)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgdev_vector_write_polygon__tmp2)))]
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
    | 58%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_vector_write_polygon__tmp2)
                                                                   - 
                                                                   (s IDgdev_vector_write_polygon_i))) (F_check_ge ((s IDgdev_vector_write_polygon__tmp2)
                                                                    - (s IDgdev_vector_write_polygon_i)) (0))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_vector_write_polygon__tmp2)
                                                                    - (s IDgdev_vector_write_polygon_i)) (0))) (F_max0_ge_0 ((s IDgdev_vector_write_polygon__tmp2)
                                                                    - (s IDgdev_vector_write_polygon_i)))]
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgdev_vector_write_polygon__tmp2))) (F_check_ge (0) (0))]
    | 69%positive => []
    | _ => []
  end.


Theorem gdev_vector_write_polygon_ai_correct:
  forall s p' s', steps (g_start gdev_vector_write_polygon) s (g_edges gdev_vector_write_polygon) p' s' -> gdev_vector_write_polygon_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gdev_vector_write_polygon_pot_correct:
  forall s p' s',
    steps (g_start gdev_vector_write_polygon) s (g_edges gdev_vector_write_polygon) p' s' ->
    (gdev_vector_write_polygon_pot (g_start gdev_vector_write_polygon) s >= gdev_vector_write_polygon_pot p' s')%Q.
Proof.
  check_lp gdev_vector_write_polygon_ai_correct gdev_vector_write_polygon_hints.
Qed.

