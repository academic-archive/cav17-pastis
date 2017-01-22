Require Import pasta.Pasta.

Notation IDgs_imagepath_z := 1%positive.
Notation IDgs_imagepath__tmp := 2%positive.
Notation IDgs_imagepath__tmp1 := 3%positive.
Notation IDgs_imagepath__tmp2 := 4%positive.
Notation IDgs_imagepath_code := 5%positive.
Notation IDgs_imagepath_x := 6%positive.
Notation IDgs_imagepath_y := 7%positive.
Notation IDgs_imagepath_data := 8%positive.
Notation IDgs_imagepath_height := 9%positive.
Notation IDgs_imagepath_pgs := 10%positive.
Notation IDgs_imagepath_width := 11%positive.
Definition gs_imagepath : graph := {|
  g_start := 1%positive;
  g_end := 54%positive;
  g_edges := (1%positive,(AAssign IDgs_imagepath_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgs_imagepath__tmp1
             (Some (EVar IDgs_imagepath_width))),3%positive)::
             (3%positive,(AAssign IDgs_imagepath__tmp2
             (Some (EVar IDgs_imagepath_height))),4%positive)::
             (4%positive,(AAssign IDgs_imagepath_y
             (Some (ESub (EVar IDgs_imagepath__tmp2) (ENum (1))))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgs_imagepath_y)
             s) >= (eval (ENum (0)) s))%Z)),12%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgs_imagepath_y)
             s) < (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDgs_imagepath__tmp (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,54%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDgs_imagepath_x
             (Some (ESub (EVar IDgs_imagepath__tmp1) (ENum (1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDgs_imagepath_x)
             s) >= (eval (ENum (0)) s))%Z)),24%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDgs_imagepath_x)
             s) < (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDgs_imagepath_y
             (Some (EAdd (EVar IDgs_imagepath_y) (ENum (-1))))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDgs_imagepath_z (Some (EAdd (ENum (1))
             (EVar IDgs_imagepath_z)))),23%positive)::
             (23%positive,AWeaken,7%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (25%positive,ANone,55%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,55%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,31%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,34%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (32%positive,ANone,55%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,55%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDgs_imagepath_code None),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,51%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDgs_imagepath_code None),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,48%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDgs_imagepath_code None),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,45%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,55%positive)::
             (45%positive,(AAssign IDgs_imagepath__tmp
             (Some (EVar IDgs_imagepath_code))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,54%positive)::
             (48%positive,(AAssign IDgs_imagepath__tmp
             (Some (EVar IDgs_imagepath_code))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,54%positive)::
             (51%positive,(AAssign IDgs_imagepath__tmp
             (Some (EVar IDgs_imagepath_code))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDgs_imagepath_x
             (Some (EAdd (EVar IDgs_imagepath_x) (ENum (-1))))),57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,(AAssign IDgs_imagepath_z (Some (EAdd (ENum (1))
             (EVar IDgs_imagepath_z)))),60%positive)::
             (60%positive,AWeaken,16%positive)::nil
|}.

Definition gs_imagepath_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_z) <= 0)%Z
    | 4%positive => (1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_z) <= 0)%Z
    | 6%positive => (1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgs_imagepath_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_y) + 1 <= 0)%Z
    | 9%positive => (1 * (s IDgs_imagepath_y) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 10%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_y) + 1 <= 0 /\ 1 * (s IDgs_imagepath__tmp) <= 0 /\ -1 * (s IDgs_imagepath__tmp) <= 0)%Z
    | 11%positive => (-1 * (s IDgs_imagepath__tmp) <= 0 /\ 1 * (s IDgs_imagepath__tmp) <= 0 /\ 1 * (s IDgs_imagepath_y) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 12%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 13%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 14%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 15%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0)%Z
    | 16%positive => (-1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 17%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_x) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDgs_imagepath_x) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 19%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_x) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDgs_imagepath_x) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) + -1 <= 0)%Z
    | 21%positive => (-1 * (s IDgs_imagepath_y) + -1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ 1 * (s IDgs_imagepath_x) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDgs_imagepath_x) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) + -1 <= 0)%Z
    | 23%positive => (-1 * (s IDgs_imagepath_y) + -1 <= 0 /\ 1 * (s IDgs_imagepath_x) + 1 <= 0 /\ -1 * (s IDgs_imagepath_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 25%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 26%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 27%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 28%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 29%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 30%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 31%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 32%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 33%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 34%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 35%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 36%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 37%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 38%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 39%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 40%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 41%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 42%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 43%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 44%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 45%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 46%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 47%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 48%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 49%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 50%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 51%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 52%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 53%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 54%positive => (-1 * (s IDgs_imagepath_z) <= 0)%Z
    | 55%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) <= 0)%Z
    | 56%positive => (-1 * (s IDgs_imagepath_x) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 57%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) + -1 <= 0)%Z
    | 58%positive => (-1 * (s IDgs_imagepath_x) + -1 <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0)%Z
    | 59%positive => (-1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) <= 0 /\ -1 * (s IDgs_imagepath_x) + -1 <= 0)%Z
    | 60%positive => (-1 * (s IDgs_imagepath_x) + -1 <= 0 /\ -1 * (s IDgs_imagepath_y) <= 0 /\ -1 * (s IDgs_imagepath_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gs_imagepath_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgs_imagepath_height))
                     + max0((s IDgs_imagepath_height)) * max0((s IDgs_imagepath_width)))%Q
    | 2%positive => ((s IDgs_imagepath_z) + max0((s IDgs_imagepath_height))
                     + max0((s IDgs_imagepath_height)) * max0((s IDgs_imagepath_width)))%Q
    | 3%positive => ((s IDgs_imagepath_z)
                     + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_height))
                     + max0((s IDgs_imagepath_height)))%Q
    | 4%positive => ((s IDgs_imagepath_z)
                     + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath__tmp2))
                     + max0((s IDgs_imagepath__tmp2)))%Q
    | 5%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                     + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 6%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                     + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 7%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                     + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 8%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                     + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 9%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                     + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 10%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                      + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 11%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                      + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 12%positive => ((s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_y))
                      + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1)))%Q
    | 13%positive => ((1 # 1) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 2) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath__tmp1))
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + (1 # 2) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0((s IDgs_imagepath_y))^2)%Q
    | 14%positive => ((1 # 1) + (1 # 2) * (s IDgs_imagepath_y)
                      - (s IDgs_imagepath_y) * max0(1 + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      - (1 # 2) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      + max0(1 + (s IDgs_imagepath_x)) * max0(1
                                                              + (s IDgs_imagepath_y))
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + (1 # 2) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0((s IDgs_imagepath_y))^2)%Q
    | 15%positive => ((1 # 1) + (1 # 2) * (s IDgs_imagepath_y)
                      - (s IDgs_imagepath_y) * max0(1 + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      - (1 # 2) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      + max0(1 + (s IDgs_imagepath_x)) * max0(1
                                                              + (s IDgs_imagepath_y))
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + (1 # 2) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0((s IDgs_imagepath_y))^2)%Q
    | 16%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath_y)))%Q
    | 17%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath_y)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0((s IDgs_imagepath_y)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0((s IDgs_imagepath_y)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)))%Q
    | 23%positive => ((s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)))%Q
    | 24%positive => ((1 # 1)
                      + (s IDgs_imagepath_y) * max0((s IDgs_imagepath__tmp1))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath_y)))%Q
    | 25%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 26%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 27%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 28%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 29%positive => ((3 # 2) + (1 # 3) * (s IDgs_imagepath_x)
                      - (1 # 3) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      - (1 # 3) * (s IDgs_imagepath_x)^2
                      - (1 # 4) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      - (3 # 4) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      - (1 # 3) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x)) * max0((s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 3) * max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 30%positive => ((3 # 2) + (1 # 3) * (s IDgs_imagepath_x)
                      - (1 # 3) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      - (1 # 3) * (s IDgs_imagepath_x)^2
                      - (1 # 4) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      - (3 # 4) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      - (1 # 3) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x)) * max0((s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 3) * max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 31%positive => ((3 # 2) + (1 # 3) * (s IDgs_imagepath_x)
                      - (1 # 3) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      - (1 # 3) * (s IDgs_imagepath_x)^2
                      - (1 # 4) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      - (3 # 4) * (s IDgs_imagepath_y)^2
                      + (s IDgs_imagepath_z)
                      - (1 # 3) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x)) * max0((s IDgs_imagepath_x))
                      + (1 # 3) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 3) * max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 32%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 33%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 34%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 35%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 36%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 37%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 38%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 39%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 40%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 41%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 42%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 43%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 44%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 45%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 46%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 47%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 48%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 49%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 50%positive => ((3 # 2)
                      - (1 # 2) * (s IDgs_imagepath_x) * max0(1
                                                              + (s IDgs_imagepath_x))
                      + (s IDgs_imagepath_z)
                      - (1 # 2) * max0(1 + (s IDgs_imagepath_x))
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_x))^2
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 51%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 52%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 53%positive => ((3 # 2) + (s IDgs_imagepath_z)
                      + (1 # 2) * max0(1 + (s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 2) * max0((s IDgs_imagepath_y)))%Q
    | 54%positive => ((s IDgs_imagepath_z))%Q
    | 55%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 56%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z)
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + max0((s IDgs_imagepath_x))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 57%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 58%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 59%positive => ((3 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | 60%positive => ((1 # 2) + (1 # 2) * (s IDgs_imagepath_y)
                      - (1 # 4) * (s IDgs_imagepath_y) * max0((s IDgs_imagepath_y))
                      + (s IDgs_imagepath_z) + max0(1 + (s IDgs_imagepath_x))
                      + (3 # 4) * max0(1 + (s IDgs_imagepath_y))
                      + (1 # 4) * max0(1 + (s IDgs_imagepath_y)) * max0((s IDgs_imagepath_y))
                      - (1 # 4) * max0(1 + (s IDgs_imagepath_y))^2
                      + max0((s IDgs_imagepath__tmp1)) * max0((s IDgs_imagepath_y))
                      + (1 # 4) * max0((s IDgs_imagepath_y))^2)%Q
    | _ => (0 # 1)%Q
  end.

Definition gs_imagepath_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDgs_imagepath_y)) ((s IDgs_imagepath_y)));
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_y));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0)))]
    | 12%positive => [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                  + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)))]
    | 16%positive => []
    | 17%positive => [(*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 + (s IDgs_imagepath_x))) (F_check_ge (0) (0))]
    | 24%positive => [(*-1 0*) F_max0_pre_decrement (1 + (s IDgs_imagepath_x)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*0 0.333333*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x)));
                      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_x)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*0 0.666667*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_x))) (F_check_ge ((s IDgs_imagepath_x)) (0));
                      (*0 0.75*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))]
    | 29%positive => []
    | 30%positive => [(*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0));
                      (*-0.75 0*) F_binom_monotonic 2 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_x))) (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_x)))]
    | 31%positive => [(*-0.75 0*) F_binom_monotonic 2 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0));
                      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDgs_imagepath_x))) (F_check_ge ((s IDgs_imagepath_x)) (0));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_x)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_x))) (F_check_ge (0) (0)))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x)));
                      (*0 0.5*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDgs_imagepath_x))) (F_check_ge ((s IDgs_imagepath_x)) (0));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_x)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_x)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_x)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_x))) (F_check_ge ((s IDgs_imagepath_x)) (0))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0))]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-2 0*) F_one;
                      (*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDgs_imagepath_y)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_x));
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_y));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-2 0*) F_one;
                      (*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDgs_imagepath_y)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_x));
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_y));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_x))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)))]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-2 0*) F_one;
                      (*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDgs_imagepath_y)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_x));
                      (*-1 0*) F_max0_ge_0 ((s IDgs_imagepath_y));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDgs_imagepath_y)) (1);
                      (*0.25 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y)));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (1
                                                                    + (s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-0.5 -0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_max0_ge_0 ((s IDgs_imagepath_y)))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDgs_imagepath_y))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDgs_imagepath_y))) (F_check_ge ((s IDgs_imagepath_y)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDgs_imagepath__tmp1))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem gs_imagepath_ai_correct:
  forall s p' s', steps (g_start gs_imagepath) s (g_edges gs_imagepath) p' s' -> gs_imagepath_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gs_imagepath_pot_correct:
  forall s p' s',
    steps (g_start gs_imagepath) s (g_edges gs_imagepath) p' s' ->
    (gs_imagepath_pot (g_start gs_imagepath) s >= gs_imagepath_pot p' s')%Q.
Proof.
  check_lp gs_imagepath_ai_correct gs_imagepath_hints.
Qed.

