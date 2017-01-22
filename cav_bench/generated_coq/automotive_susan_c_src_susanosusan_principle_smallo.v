Require Import pasta.Pasta.

Notation IDsusan_principle_small_z := 1%positive.
Notation IDsusan_principle_small__tmp := 2%positive.
Notation IDsusan_principle_small__tmp1 := 3%positive.
Notation IDsusan_principle_small__tmp2 := 4%positive.
Notation IDsusan_principle_small_i := 5%positive.
Notation IDsusan_principle_small_j := 6%positive.
Notation IDsusan_principle_small_n := 7%positive.
Notation IDsusan_principle_small_bp := 8%positive.
Notation IDsusan_principle_small_in := 9%positive.
Notation IDsusan_principle_small_max_no := 10%positive.
Notation IDsusan_principle_small_r := 11%positive.
Notation IDsusan_principle_small_x_size := 12%positive.
Notation IDsusan_principle_small_y_size := 13%positive.
Definition susan_principle_small : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDsusan_principle_small_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDsusan_principle_small__tmp2
             (Some (EVar IDsusan_principle_small_max_no))),3%positive)::
             (3%positive,(AAssign IDsusan_principle_small__tmp1
             (Some (EVar IDsusan_principle_small_x_size))),4%positive)::
             (4%positive,(AAssign IDsusan_principle_small__tmp
             (Some (EVar IDsusan_principle_small_y_size))),5%positive)::
             (5%positive,(AAssign IDsusan_principle_small__tmp2
             (Some (ENum (730)))),6%positive)::
             (6%positive,(AAssign IDsusan_principle_small_i
             (Some (ENum (1)))),7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_i) s) <
             (eval (ESub (EVar IDsusan_principle_small__tmp) (ENum (1)))
             s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_i) s) >=
             (eval (ESub (EVar IDsusan_principle_small__tmp) (ENum (1)))
             s))%Z)),10%positive)::(10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDsusan_principle_small_j
             (Some (ENum (1)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_j) s) <
             (eval (ESub (EVar IDsusan_principle_small__tmp1) (ENum (1)))
             s))%Z)),24%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_j) s) >=
             (eval (ESub (EVar IDsusan_principle_small__tmp1) (ENum (1)))
             s))%Z)),17%positive)::(17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDsusan_principle_small_i
             (Some (EAdd (EVar IDsusan_principle_small_i) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDsusan_principle_small_z
             (Some (EAdd (ENum (1)) (EVar IDsusan_principle_small_z)))),
             23%positive)::(23%positive,AWeaken,9%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDsusan_principle_small_n
             (Some (ENum (100)))),26%positive)::
             (26%positive,(AAssign IDsusan_principle_small_n None),
             27%positive)::
             (27%positive,(AAssign IDsusan_principle_small_n None),
             28%positive)::
             (28%positive,(AAssign IDsusan_principle_small_n None),
             29%positive)::
             (29%positive,(AAssign IDsusan_principle_small_n None),
             30%positive)::
             (30%positive,(AAssign IDsusan_principle_small_n None),
             31%positive)::
             (31%positive,(AAssign IDsusan_principle_small_n None),
             32%positive)::
             (32%positive,(AAssign IDsusan_principle_small_n None),
             33%positive)::
             (33%positive,(AAssign IDsusan_principle_small_n None),
             34%positive)::(34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_n) s) <=
             (eval (EVar IDsusan_principle_small__tmp2) s))%Z)),37%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDsusan_principle_small_n) s) >
             (eval (EVar IDsusan_principle_small__tmp2) s))%Z)),36%positive)::
             (36%positive,AWeaken,39%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDsusan_principle_small_j
             (Some (EAdd (EVar IDsusan_principle_small_j) (ENum (1))))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDsusan_principle_small_z
             (Some (EAdd (ENum (1)) (EVar IDsusan_principle_small_z)))),
             44%positive)::(44%positive,AWeaken,16%positive)::nil
|}.

Definition susan_principle_small_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small_z) <= 0)%Z
    | 4%positive => (1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small_z) <= 0)%Z
    | 6%positive => (1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 7%positive => (-1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small_i) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small_i) + -1 <= 0 /\ 1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 9%positive => (-1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 10%positive => (-1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp)+ -1 * (s IDsusan_principle_small_i) + -1 <= 0)%Z
    | 11%positive => (1 * (s IDsusan_principle_small__tmp)+ -1 * (s IDsusan_principle_small_i) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 12%positive => (-1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0)%Z
    | 13%positive => (-1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 14%positive => (-1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ 1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ 1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0)%Z
    | 16%positive => (-1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0)%Z
    | 18%positive => (1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ 1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0)%Z
    | 22%positive => (1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp1)+ -1 * (s IDsusan_principle_small_j) + -1 <= 0 /\ -1 * (s IDsusan_principle_small_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 25%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ 1 * (s IDsusan_principle_small_n) + -100 <= 0 /\ -1 * (s IDsusan_principle_small_n) + 100 <= 0)%Z
    | 27%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 29%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 31%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 33%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 35%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2)+ -1 * (s IDsusan_principle_small_n) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2)+ 1 * (s IDsusan_principle_small_n) <= 0)%Z
    | 38%positive => (-1 * (s IDsusan_principle_small__tmp2)+ 1 * (s IDsusan_principle_small_n) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 40%positive => (-1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 42%positive => (-1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small_z) <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_j) + 2 <= 0)%Z
    | 44%positive => (-1 * (s IDsusan_principle_small_j) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp1)+ 1 * (s IDsusan_principle_small_j) + 1 <= 0 /\ -1 * (s IDsusan_principle_small__tmp)+ 1 * (s IDsusan_principle_small_i) + 2 <= 0 /\ -1 * (s IDsusan_principle_small__tmp2) + 730 <= 0 /\ 1 * (s IDsusan_principle_small__tmp2) + -730 <= 0 /\ -1 * (s IDsusan_principle_small_i) + 1 <= 0 /\ -1 * (s IDsusan_principle_small_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition susan_principle_small_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2 + (s IDsusan_principle_small_x_size)) * max0(-2
                                                                    + (s IDsusan_principle_small_y_size))
                     + max0(-2 + (s IDsusan_principle_small_y_size)))%Q
    | 2%positive => ((s IDsusan_principle_small_z)
                     + max0(-2 + (s IDsusan_principle_small_x_size)) * max0(-2
                                                                    + (s IDsusan_principle_small_y_size))
                     + max0(-2 + (s IDsusan_principle_small_y_size)))%Q
    | 3%positive => ((s IDsusan_principle_small_z)
                     + max0(-2 + (s IDsusan_principle_small_x_size)) * max0(-2
                                                                    + (s IDsusan_principle_small_y_size))
                     + max0(-2 + (s IDsusan_principle_small_y_size)))%Q
    | 4%positive => ((s IDsusan_principle_small_z)
                     + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-2
                                                                    + (s IDsusan_principle_small_y_size))
                     + max0(-2 + (s IDsusan_principle_small_y_size)))%Q
    | 5%positive => ((s IDsusan_principle_small_z)
                     + max0(-2 + (s IDsusan_principle_small__tmp))
                     + max0(-2 + (s IDsusan_principle_small__tmp)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)))%Q
    | 6%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     + (s IDsusan_principle_small_z)
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + max0(-2 + (s IDsusan_principle_small__tmp)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                     - (5 # 78) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (5 # 78) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                      - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 7%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     + (s IDsusan_principle_small_z)
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (5 # 78) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (5 # 78) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                      - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 8%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     + (s IDsusan_principle_small_z)
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (5 # 78) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (5 # 78) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                      - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 9%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     + (s IDsusan_principle_small_z)
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-730
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                     - (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                     + (0 # 1) * max0(-100
                                      + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                     - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                     + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                     - (5 # 78) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (5 # 78) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j))
                     + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                      - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 10%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 11%positive => ((s IDsusan_principle_small_z))%Q
    | 12%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 13%positive => ((0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)))%Q
    | 14%positive => (-(0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j)))%Q
    | 15%positive => (-(0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j)))%Q
    | 16%positive => (-(0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j)))%Q
    | 17%positive => (-(0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j)))%Q
    | 18%positive => ((10 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + (10 # 73) * max0(-2
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0(-1 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 19%positive => ((10 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + (10 # 73) * max0(-2
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0(-1 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 20%positive => ((10 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (10 # 73) * max0(-1
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      - max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 21%positive => ((10 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (10 # 73) * max0(-1
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      - max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 22%positive => ((10 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (10 # 73) * max0(-1
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      - max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 23%positive => (-(63 # 73)
                      + (0 # 1) * (s IDsusan_principle_small__tmp) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (s IDsusan_principle_small__tmp) * max0(-1
                                                                + (s IDsusan_principle_small__tmp1)
                                                                - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small_i) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (s IDsusan_principle_small_i) * max0(-1
                                                             + (s IDsusan_principle_small__tmp1)
                                                             - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (10 # 73) * max0(-1
                                         + (s IDsusan_principle_small__tmp)
                                         - (s IDsusan_principle_small_i))
                      - max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2)))%Q
    | 24%positive => (-(0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j)))%Q
    | 25%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 26%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 27%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 28%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 29%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 30%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 31%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 32%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 33%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 34%positive => ((5 # 26)
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (s IDsusan_principle_small_z)
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small_z))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 39) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      - max0((s IDsusan_principle_small_z)))%Q
    | 35%positive => ((5 # 26)
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      - (0 # 1) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (73 # 78) * max0(-1
                                         + (s IDsusan_principle_small__tmp1)
                                         - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2)))%Q
    | 36%positive => ((5 # 26)
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      - (0 # 1) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (73 # 78) * max0(-1
                                         + (s IDsusan_principle_small__tmp1)
                                         - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2)))%Q
    | 37%positive => ((5 # 26)
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      - (0 # 1) * max0(-2 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (73 # 78) * max0(-1
                                         + (s IDsusan_principle_small__tmp1)
                                         - (s IDsusan_principle_small_j))
                      - (0 # 1) * max0(-1 + (s IDsusan_principle_small__tmp1)
                                       - (s IDsusan_principle_small_j)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2)))%Q
    | 38%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 39%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 40%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + max0(-2 + (s IDsusan_principle_small__tmp1)
                             - (s IDsusan_principle_small_j)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 41%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 42%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 43%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | 44%positive => (-(3 # 98)
                      - (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp1) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2)
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      + (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      - (0 # 1) * (s IDsusan_principle_small__tmp2) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_j) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_j) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * (s IDsusan_principle_small_z) * max0(-730
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * (s IDsusan_principle_small_z) * max0(-100
                                                                    + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-100
                                                                    + (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-730
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-3
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-2
                                                                    + (s IDsusan_principle_small__tmp1))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      - (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0(730
                                                                    - (s IDsusan_principle_small_n))
                      + (0 # 1) * max0(-100
                                       + (s IDsusan_principle_small__tmp2)) * max0((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0(-100 + (s IDsusan_principle_small_n)) * max0(730
                                                                    - (s IDsusan_principle_small__tmp2))
                      + (5 # 78) * max0(-3
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-2
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1))
                      - max0(-2 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      - (5 # 78) * max0(-2
                                        + (s IDsusan_principle_small__tmp1))
                      + max0(-1 + (s IDsusan_principle_small__tmp)
                             - (s IDsusan_principle_small_i)) * max0(-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp1)
                                                                    - 
                                                                    (s IDsusan_principle_small_j))
                      + (5 # 78) * max0(-1
                                        + (s IDsusan_principle_small__tmp1)
                                        - (s IDsusan_principle_small_j))
                      + (0 # 1) * max0(730
                                       - (s IDsusan_principle_small__tmp2))
                      + (0 # 1) * max0(730 - (s IDsusan_principle_small_n))
                      - (0 # 1) * max0((s IDsusan_principle_small__tmp2)
                                       - (s IDsusan_principle_small_n)))%Q
    | _ => (0 # 1)%Q
  end.

Definition susan_principle_small_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-0.136986 0*) F_max0_monotonic (F_check_ge (-1
                                                                    + 
                                                                    (s IDsusan_principle_small__tmp)
                                                                    - 
                                                                    (s IDsusan_principle_small_i)) (-2
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)));
                      (*-0.136986 0*) F_max0_ge_0 (-2
                                                   + (s IDsusan_principle_small__tmp)
                                                   - (s IDsusan_principle_small_i));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (0) (0)));
                      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-8.78117e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => [(*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-8.78117e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)))]
    | 16%positive => []
    | 17%positive => [(*0 0.136986*) F_max0_pre_decrement (-1
                                                           + (s IDsusan_principle_small__tmp)
                                                           - (s IDsusan_principle_small_i)) (1);
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (0) (0)));
                      (*0 0.000946276*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*0 0.000217439*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1))) (F_check_ge (0) (0)));
                      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + (s IDsusan_principle_small__tmp1))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.00136986 0*) F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0));
                      (*-0.00136986 0*) F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))]
    | 24%positive => [(*-0.192308 0*) F_max0_pre_decrement (-1
                                                            + (s IDsusan_principle_small__tmp1)
                                                            - (s IDsusan_principle_small_j)) (1);
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small_z))) (F_check_ge (0) (0)));
                      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small_z))) (F_check_ge (0) (0)));
                      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_small_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_small_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (-2
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp)
                                                                    - (s IDsusan_principle_small_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*0 0.00128205*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_small_z))) (F_check_ge ((s IDsusan_principle_small_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_small_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_small_z)))]
    | 35%positive => []
    | 36%positive => [(*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-731
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-731
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1.5448e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-1.5448e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84523e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-2.02933e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.02933e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (-1
                                                                    - (s IDsusan_principle_small__tmp2)
                                                                    + (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)));
                      (*-1.5448e-06 0*) F_binom_monotonic 2 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))]
    | 37%positive => [(*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1.54557e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (730
                                                                    - (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n)) (0));
                      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small_n)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n)))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small_z))) (F_check_ge (0) (0)));
                      (*-2.51953e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-730
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.51953e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-3.0525e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small__tmp2)
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (0) (0)));
                      (*-3.0525e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small_n))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDsusan_principle_small_z))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j))) (F_check_ge (-1
                                                                    + (s IDsusan_principle_small__tmp1)
                                                                    - (s IDsusan_principle_small_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (730
                                                                    - (s IDsusan_principle_small__tmp2)))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - (s IDsusan_principle_small__tmp2))) (F_check_ge (730
                                                                    - (s IDsusan_principle_small__tmp2)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsusan_principle_small_z)) (0))) (F_max0_ge_0 ((s IDsusan_principle_small_z)))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_small_z))) (F_check_ge ((s IDsusan_principle_small_z)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + (s IDsusan_principle_small__tmp2))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsusan_principle_small_z))) (F_check_ge ((s IDsusan_principle_small_z)) (0));
                      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + (s IDsusan_principle_small__tmp2)) (0))) (F_max0_ge_0 (-100
                                                                    + (s IDsusan_principle_small__tmp2)))]
    | _ => []
  end.


Theorem susan_principle_small_ai_correct:
  forall s p' s', steps (g_start susan_principle_small) s (g_edges susan_principle_small) p' s' -> susan_principle_small_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem susan_principle_small_pot_correct:
  forall s p' s',
    steps (g_start susan_principle_small) s (g_edges susan_principle_small) p' s' ->
    (susan_principle_small_pot (g_start susan_principle_small) s >= susan_principle_small_pot p' s')%Q.
Proof.
  check_lp susan_principle_small_ai_correct susan_principle_small_hints.
Qed.

