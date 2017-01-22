Require Import pasta.Pasta.

Notation IDgdev_pdf_copy_color_z := 1%positive.
Notation IDgdev_pdf_copy_color__tmp := 2%positive.
Notation IDgdev_pdf_copy_color__tmp1 := 3%positive.
Notation IDgdev_pdf_copy_color__tmp2 := 4%positive.
Notation IDgdev_pdf_copy_color__tmp3 := 5%positive.
Notation IDgdev_pdf_copy_color__tmp4 := 6%positive.
Notation IDgdev_pdf_copy_color__tmp5 := 7%positive.
Notation IDgdev_pdf_copy_color__tmp6 := 8%positive.
Notation IDgdev_pdf_copy_color__tmp7 := 9%positive.
Notation IDgdev_pdf_copy_color_bytes_per_pixel := 10%positive.
Notation IDgdev_pdf_copy_color_code := 11%positive.
Notation IDgdev_pdf_copy_color_depth := 12%positive.
Notation IDgdev_pdf_copy_color_nbytes := 13%positive.
Notation IDgdev_pdf_copy_color_yi := 14%positive.
Notation IDgdev_pdf_copy_color_base := 15%positive.
Notation IDgdev_pdf_copy_color_dev := 16%positive.
Notation IDgdev_pdf_copy_color_h := 17%positive.
Notation IDgdev_pdf_copy_color_id := 18%positive.
Notation IDgdev_pdf_copy_color_raster := 19%positive.
Notation IDgdev_pdf_copy_color_sourcex := 20%positive.
Notation IDgdev_pdf_copy_color_w := 21%positive.
Notation IDgdev_pdf_copy_color_x := 22%positive.
Notation IDgdev_pdf_copy_color_y := 23%positive.
Definition gdev_pdf_copy_color : graph := {|
  g_start := 1%positive;
  g_end := 75%positive;
  g_edges := (1%positive,(AAssign IDgdev_pdf_copy_color_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgdev_pdf_copy_color__tmp7
             (Some (EVar IDgdev_pdf_copy_color_sourcex))),3%positive)::
             (3%positive,(AAssign IDgdev_pdf_copy_color__tmp6
             (Some (EVar IDgdev_pdf_copy_color_raster))),4%positive)::
             (4%positive,(AAssign IDgdev_pdf_copy_color__tmp5
             (Some (EVar IDgdev_pdf_copy_color_id))),5%positive)::
             (5%positive,(AAssign IDgdev_pdf_copy_color__tmp4
             (Some (EVar IDgdev_pdf_copy_color_x))),6%positive)::
             (6%positive,(AAssign IDgdev_pdf_copy_color__tmp3
             (Some (EVar IDgdev_pdf_copy_color_y))),7%positive)::
             (7%positive,(AAssign IDgdev_pdf_copy_color__tmp1
             (Some (EVar IDgdev_pdf_copy_color_w))),8%positive)::
             (8%positive,(AAssign IDgdev_pdf_copy_color__tmp2
             (Some (EVar IDgdev_pdf_copy_color_h))),9%positive)::
             (9%positive,(AAssign IDgdev_pdf_copy_color_depth None),
             10%positive)::
             (10%positive,(AAssign IDgdev_pdf_copy_color_bytes_per_pixel
             None),11%positive)::
             (11%positive,(AAssign IDgdev_pdf_copy_color_code None),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) <
             (eval (ENum (0)) s))%Z)),71%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) >=
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color__tmp1) s) <=
             (eval (ENum (0)) s))%Z)),67%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color__tmp1) s) >
             (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color__tmp2) s) <=
             (eval (ENum (0)) s))%Z)),66%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color__tmp2) s) >
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_bytes_per_pixel)
             s) = (eval (ENum (3)) s))%Z)),22%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_bytes_per_pixel)
             s) <> (eval (ENum (3)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,24%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDgdev_pdf_copy_color_nbytes
             (Some (EMul (EMul (EVar IDgdev_pdf_copy_color__tmp1)
             (EVar IDgdev_pdf_copy_color_bytes_per_pixel))
             (EVar IDgdev_pdf_copy_color__tmp2)))),25%positive)::
             (25%positive,(AAssign IDgdev_pdf_copy_color_code None),
             26%positive)::(26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) <
             (eval (ENum (0)) s))%Z)),62%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) >=
             (eval (ENum (0)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDgdev_pdf_copy_color_code None),
             30%positive)::(30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) <
             (eval (ENum (0)) s))%Z)),58%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_code) s) >=
             (eval (ENum (0)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDgdev_pdf_copy_color_yi
             (Some (ENum (0)))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_yi) s) <
             (eval (EVar IDgdev_pdf_copy_color__tmp2) s))%Z)),51%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDgdev_pdf_copy_color_yi) s) >=
             (eval (EVar IDgdev_pdf_copy_color__tmp2) s))%Z)),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDgdev_pdf_copy_color_code None),
             39%positive)::(39%positive,AWeaken,40%positive)::
             (40%positive,ANone,48%positive)::
             (40%positive,ANone,45%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDgdev_pdf_copy_color__tmp None),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,AWeaken,75%positive)::
             (45%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (ENum (0)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,75%positive)::
             (48%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (EVar IDgdev_pdf_copy_color_code))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,75%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDgdev_pdf_copy_color_yi
             (Some (EAdd (EVar IDgdev_pdf_copy_color_yi) (ENum (1))))),
             54%positive)::(54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDgdev_pdf_copy_color_z
             (Some (EAdd (ENum (1)) (EVar IDgdev_pdf_copy_color_z)))),
             57%positive)::(57%positive,AWeaken,36%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (EVar IDgdev_pdf_copy_color_code))),60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,75%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (EVar IDgdev_pdf_copy_color_code))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,AWeaken,75%positive)::
             (66%positive,AWeaken,68%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (ENum (0)))),69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,AWeaken,75%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,(AAssign IDgdev_pdf_copy_color__tmp
             (Some (EVar IDgdev_pdf_copy_color_code))),73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,AWeaken,75%positive)::nil
|}.

Definition gdev_pdf_copy_color_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 4%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 6%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 8%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 10%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 11%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 12%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 14%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0)%Z
    | 15%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 16%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 18%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 20%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 22%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_bytes_per_pixel) + -3 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_bytes_per_pixel) + 3 <= 0)%Z
    | 23%positive => (-1 * (s IDgdev_pdf_copy_color_bytes_per_pixel) + 3 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_bytes_per_pixel) + -3 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 24%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 26%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 28%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0)%Z
    | 29%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 30%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 32%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0)%Z
    | 33%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 34%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 35%positive => (-1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 36%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 37%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 38%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 39%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 40%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 41%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 42%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 43%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 44%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 45%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 46%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp) <= 0)%Z
    | 47%positive => (-1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 48%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 49%positive => (1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 50%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2)+ -1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 51%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0)%Z
    | 53%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 55%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 56%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0)%Z
    | 57%positive => (-1 * (s IDgdev_pdf_copy_color__tmp2)+ 1 * (s IDgdev_pdf_copy_color_yi) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_yi) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) + 1 <= 0)%Z
    | 58%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0)%Z
    | 59%positive => (1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 60%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0)%Z
    | 61%positive => (1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 62%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0)%Z
    | 63%positive => (1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 64%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0)%Z
    | 65%positive => (1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp2) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 66%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp1) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp2) <= 0)%Z
    | 67%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp1) <= 0)%Z
    | 68%positive => (-1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 69%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ -1 * (s IDgdev_pdf_copy_color__tmp) <= 0)%Z
    | 70%positive => (-1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_code) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 71%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0)%Z
    | 72%positive => (1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 73%positive => (1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0)%Z
    | 74%positive => (1 * (s IDgdev_pdf_copy_color__tmp) + 1 <= 0 /\ 1 * (s IDgdev_pdf_copy_color_code) + 1 <= 0 /\ -1 * (s IDgdev_pdf_copy_color_z) <= 0 /\ 1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | 75%positive => (-1 * (s IDgdev_pdf_copy_color_z) <= 0)%Z
    | _ => False
  end.

Definition gdev_pdf_copy_color_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgdev_pdf_copy_color_h)))%Q
    | 2%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 3%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 4%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 5%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 6%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 7%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 8%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color_h)))%Q
    | 9%positive => ((s IDgdev_pdf_copy_color_z)
                     + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 10%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 11%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 12%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 13%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 14%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 15%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 16%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 17%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 18%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 19%positive => (max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 20%positive => (max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 21%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 22%positive => (max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 23%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 24%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 25%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 26%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 27%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 28%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 29%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 30%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 31%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 32%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 33%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 34%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 35%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 36%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 37%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 38%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 39%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 40%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 41%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 42%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 43%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 44%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 45%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 46%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 47%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 48%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 49%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 50%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 51%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 52%positive => ((1 # 1) + (s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      + max0(-1 + (s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi))
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 53%positive => ((1 # 1) + (s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      + max0(-1 + (s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi))
                      - max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 54%positive => ((1 # 1) + (s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 55%positive => ((1 # 1) + (s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 56%positive => ((1 # 1) + (s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 57%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      - max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color__tmp2)
                             - (s IDgdev_pdf_copy_color_yi)))%Q
    | 58%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 59%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      + max0(-(s IDgdev_pdf_copy_color_z)))%Q
    | 60%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      + max0(-(s IDgdev_pdf_copy_color_z)))%Q
    | 61%positive => ((s IDgdev_pdf_copy_color__tmp2)
                      + (s IDgdev_pdf_copy_color_z)
                      + max0(-(s IDgdev_pdf_copy_color_z)))%Q
    | 62%positive => ((s IDgdev_pdf_copy_color__tmp2))%Q
    | 63%positive => ((1 # 1) + max0(-1 + (s IDgdev_pdf_copy_color__tmp2)))%Q
    | 64%positive => ((1 # 1) + max0(-1 + (s IDgdev_pdf_copy_color__tmp2)))%Q
    | 65%positive => ((1 # 1) + max0(-1 + (s IDgdev_pdf_copy_color__tmp2)))%Q
    | 66%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 67%positive => (max0((s IDgdev_pdf_copy_color__tmp2))
                      + max0((s IDgdev_pdf_copy_color_z)))%Q
    | 68%positive => (max0((s IDgdev_pdf_copy_color_z)))%Q
    | 69%positive => (max0((s IDgdev_pdf_copy_color_z)))%Q
    | 70%positive => (max0((s IDgdev_pdf_copy_color_z)))%Q
    | 71%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 72%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 73%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 74%positive => ((s IDgdev_pdf_copy_color_z)
                      + max0((s IDgdev_pdf_copy_color__tmp2)))%Q
    | 75%positive => ((s IDgdev_pdf_copy_color_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gdev_pdf_copy_color_hints (p : node) (s : state) := 
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
    | 14%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_pdf_copy_color_z)) (0))) (F_max0_ge_0 ((s IDgdev_pdf_copy_color_z)))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgdev_pdf_copy_color_z))) (F_check_ge (0) (0))]
    | 19%positive => []
    | 20%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_pdf_copy_color__tmp2))) (F_check_ge ((s IDgdev_pdf_copy_color__tmp2)) (0))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_pdf_copy_color__tmp2))) (F_check_ge ((s IDgdev_pdf_copy_color__tmp2)) (0))]
    | 23%positive => []
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
    | 35%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgdev_pdf_copy_color_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgdev_pdf_copy_color_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgdev_pdf_copy_color_z)))]
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2)
                                            - (s IDgdev_pdf_copy_color_yi))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_pdf_copy_color__tmp2)) (0))) (F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2)))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_pdf_copy_color__tmp2)) (0))) (F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgdev_pdf_copy_color__tmp2)) (0))) (F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2)))]
    | 51%positive => [(*0 1*) F_max0_pre_decrement ((s IDgdev_pdf_copy_color__tmp2)
                                                    - (s IDgdev_pdf_copy_color_yi)) (1)]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgdev_pdf_copy_color_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgdev_pdf_copy_color_z)))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgdev_pdf_copy_color_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgdev_pdf_copy_color__tmp2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgdev_pdf_copy_color__tmp2)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgdev_pdf_copy_color__tmp2)))]
    | 62%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgdev_pdf_copy_color__tmp2)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgdev_pdf_copy_color__tmp2)))]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgdev_pdf_copy_color_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgdev_pdf_copy_color_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgdev_pdf_copy_color_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgdev_pdf_copy_color__tmp2))) (F_check_ge (0) (0))]
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2))) (F_check_ge (0) (0))]
    | 67%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2))) (F_check_ge (0) (0))]
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgdev_pdf_copy_color_z))) (F_check_ge ((s IDgdev_pdf_copy_color_z)) (0))]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*-1 0*) F_max0_ge_0 ((s IDgdev_pdf_copy_color__tmp2))]
    | 75%positive => []
    | _ => []
  end.


Theorem gdev_pdf_copy_color_ai_correct:
  forall s p' s', steps (g_start gdev_pdf_copy_color) s (g_edges gdev_pdf_copy_color) p' s' -> gdev_pdf_copy_color_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gdev_pdf_copy_color_pot_correct:
  forall s p' s',
    steps (g_start gdev_pdf_copy_color) s (g_edges gdev_pdf_copy_color) p' s' ->
    (gdev_pdf_copy_color_pot (g_start gdev_pdf_copy_color) s >= gdev_pdf_copy_color_pot p' s')%Q.
Proof.
  check_lp gdev_pdf_copy_color_ai_correct gdev_pdf_copy_color_hints.
Qed.

