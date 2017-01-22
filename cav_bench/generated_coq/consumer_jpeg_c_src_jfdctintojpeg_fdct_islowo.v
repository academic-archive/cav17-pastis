Require Import pasta.Pasta.

Notation IDjpeg_fdct_islow_z := 1%positive.
Notation IDjpeg_fdct_islow_ctr := 2%positive.
Notation IDjpeg_fdct_islow_tmp0 := 3%positive.
Notation IDjpeg_fdct_islow_tmp1 := 4%positive.
Notation IDjpeg_fdct_islow_tmp10 := 5%positive.
Notation IDjpeg_fdct_islow_tmp11 := 6%positive.
Notation IDjpeg_fdct_islow_tmp12 := 7%positive.
Notation IDjpeg_fdct_islow_tmp13 := 8%positive.
Notation IDjpeg_fdct_islow_tmp2 := 9%positive.
Notation IDjpeg_fdct_islow_tmp3 := 10%positive.
Notation IDjpeg_fdct_islow_tmp4 := 11%positive.
Notation IDjpeg_fdct_islow_tmp5 := 12%positive.
Notation IDjpeg_fdct_islow_tmp6 := 13%positive.
Notation IDjpeg_fdct_islow_tmp7 := 14%positive.
Notation IDjpeg_fdct_islow_z1 := 15%positive.
Notation IDjpeg_fdct_islow_z2 := 16%positive.
Notation IDjpeg_fdct_islow_z3 := 17%positive.
Notation IDjpeg_fdct_islow_z4 := 18%positive.
Notation IDjpeg_fdct_islow_z5 := 19%positive.
Notation IDjpeg_fdct_islow_data := 20%positive.
Definition jpeg_fdct_islow : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDjpeg_fdct_islow_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_fdct_islow_ctr (Some (ENum (7)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_islow_ctr) s) >=
             (eval (ENum (0)) s))%Z)),48%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_islow_ctr) s) <
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDjpeg_fdct_islow_ctr (Some (ENum (7)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_islow_ctr) s) >=
             (eval (ENum (0)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_islow_ctr) s) <
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDjpeg_fdct_islow_tmp0 None),15%positive)::
             (15%positive,(AAssign IDjpeg_fdct_islow_tmp7 None),16%positive)::
             (16%positive,(AAssign IDjpeg_fdct_islow_tmp1 None),17%positive)::
             (17%positive,(AAssign IDjpeg_fdct_islow_tmp6 None),18%positive)::
             (18%positive,(AAssign IDjpeg_fdct_islow_tmp2 None),19%positive)::
             (19%positive,(AAssign IDjpeg_fdct_islow_tmp5 None),20%positive)::
             (20%positive,(AAssign IDjpeg_fdct_islow_tmp3 None),21%positive)::
             (21%positive,(AAssign IDjpeg_fdct_islow_tmp4 None),22%positive)::
             (22%positive,(AAssign IDjpeg_fdct_islow_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp0)
             (EVar IDjpeg_fdct_islow_tmp3)))),23%positive)::
             (23%positive,(AAssign IDjpeg_fdct_islow_tmp13
             (Some (ESub (EVar IDjpeg_fdct_islow_tmp0)
             (EVar IDjpeg_fdct_islow_tmp3)))),24%positive)::
             (24%positive,(AAssign IDjpeg_fdct_islow_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp1)
             (EVar IDjpeg_fdct_islow_tmp2)))),25%positive)::
             (25%positive,(AAssign IDjpeg_fdct_islow_tmp12
             (Some (ESub (EVar IDjpeg_fdct_islow_tmp1)
             (EVar IDjpeg_fdct_islow_tmp2)))),26%positive)::
             (26%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EMul (EAdd (EVar IDjpeg_fdct_islow_tmp12)
             (EVar IDjpeg_fdct_islow_tmp13)) (ENum (4433))))),27%positive)::
             (27%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp4)
             (EVar IDjpeg_fdct_islow_tmp7)))),28%positive)::
             (28%positive,(AAssign IDjpeg_fdct_islow_z2
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp5)
             (EVar IDjpeg_fdct_islow_tmp6)))),29%positive)::
             (29%positive,(AAssign IDjpeg_fdct_islow_z3
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp4)
             (EVar IDjpeg_fdct_islow_tmp6)))),30%positive)::
             (30%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp5)
             (EVar IDjpeg_fdct_islow_tmp7)))),31%positive)::
             (31%positive,(AAssign IDjpeg_fdct_islow_z5
             (Some (EMul (EAdd (EVar IDjpeg_fdct_islow_z3)
             (EVar IDjpeg_fdct_islow_z4)) (ENum (9633))))),32%positive)::
             (32%positive,(AAssign IDjpeg_fdct_islow_tmp4
             (Some (EMul (EVar IDjpeg_fdct_islow_tmp4) (ENum (2446))))),
             33%positive)::
             (33%positive,(AAssign IDjpeg_fdct_islow_tmp5 None),34%positive)::
             (34%positive,(AAssign IDjpeg_fdct_islow_tmp6 None),35%positive)::
             (35%positive,(AAssign IDjpeg_fdct_islow_tmp7 None),36%positive)::
             (36%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EMul (EVar IDjpeg_fdct_islow_z1) (ENum (-7373))))),
             37%positive)::
             (37%positive,(AAssign IDjpeg_fdct_islow_z2 None),38%positive)::
             (38%positive,(AAssign IDjpeg_fdct_islow_z3 None),39%positive)::
             (39%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EMul (EVar IDjpeg_fdct_islow_z4) (ENum (-3196))))),
             40%positive)::
             (40%positive,(AAssign IDjpeg_fdct_islow_z3
             (Some (EAdd (EVar IDjpeg_fdct_islow_z3)
             (EVar IDjpeg_fdct_islow_z5)))),41%positive)::
             (41%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EAdd (EVar IDjpeg_fdct_islow_z4)
             (EVar IDjpeg_fdct_islow_z5)))),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDjpeg_fdct_islow_ctr
             (Some (EAdd (EVar IDjpeg_fdct_islow_ctr) (ENum (-1))))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDjpeg_fdct_islow_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_islow_z)))),47%positive)::
             (47%positive,AWeaken,10%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDjpeg_fdct_islow_tmp0 None),50%positive)::
             (50%positive,(AAssign IDjpeg_fdct_islow_tmp7 None),51%positive)::
             (51%positive,(AAssign IDjpeg_fdct_islow_tmp1 None),52%positive)::
             (52%positive,(AAssign IDjpeg_fdct_islow_tmp6 None),53%positive)::
             (53%positive,(AAssign IDjpeg_fdct_islow_tmp2 None),54%positive)::
             (54%positive,(AAssign IDjpeg_fdct_islow_tmp5 None),55%positive)::
             (55%positive,(AAssign IDjpeg_fdct_islow_tmp3 None),56%positive)::
             (56%positive,(AAssign IDjpeg_fdct_islow_tmp4 None),57%positive)::
             (57%positive,(AAssign IDjpeg_fdct_islow_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp0)
             (EVar IDjpeg_fdct_islow_tmp3)))),58%positive)::
             (58%positive,(AAssign IDjpeg_fdct_islow_tmp13
             (Some (ESub (EVar IDjpeg_fdct_islow_tmp0)
             (EVar IDjpeg_fdct_islow_tmp3)))),59%positive)::
             (59%positive,(AAssign IDjpeg_fdct_islow_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp1)
             (EVar IDjpeg_fdct_islow_tmp2)))),60%positive)::
             (60%positive,(AAssign IDjpeg_fdct_islow_tmp12
             (Some (ESub (EVar IDjpeg_fdct_islow_tmp1)
             (EVar IDjpeg_fdct_islow_tmp2)))),61%positive)::
             (61%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EMul (EAdd (EVar IDjpeg_fdct_islow_tmp12)
             (EVar IDjpeg_fdct_islow_tmp13)) (ENum (4433))))),62%positive)::
             (62%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp4)
             (EVar IDjpeg_fdct_islow_tmp7)))),63%positive)::
             (63%positive,(AAssign IDjpeg_fdct_islow_z2
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp5)
             (EVar IDjpeg_fdct_islow_tmp6)))),64%positive)::
             (64%positive,(AAssign IDjpeg_fdct_islow_z3
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp4)
             (EVar IDjpeg_fdct_islow_tmp6)))),65%positive)::
             (65%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EAdd (EVar IDjpeg_fdct_islow_tmp5)
             (EVar IDjpeg_fdct_islow_tmp7)))),66%positive)::
             (66%positive,(AAssign IDjpeg_fdct_islow_z5
             (Some (EMul (EAdd (EVar IDjpeg_fdct_islow_z3)
             (EVar IDjpeg_fdct_islow_z4)) (ENum (9633))))),67%positive)::
             (67%positive,(AAssign IDjpeg_fdct_islow_tmp4
             (Some (EMul (EVar IDjpeg_fdct_islow_tmp4) (ENum (2446))))),
             68%positive)::
             (68%positive,(AAssign IDjpeg_fdct_islow_tmp5 None),69%positive)::
             (69%positive,(AAssign IDjpeg_fdct_islow_tmp6 None),70%positive)::
             (70%positive,(AAssign IDjpeg_fdct_islow_tmp7 None),71%positive)::
             (71%positive,(AAssign IDjpeg_fdct_islow_z1
             (Some (EMul (EVar IDjpeg_fdct_islow_z1) (ENum (-7373))))),
             72%positive)::
             (72%positive,(AAssign IDjpeg_fdct_islow_z2 None),73%positive)::
             (73%positive,(AAssign IDjpeg_fdct_islow_z3 None),74%positive)::
             (74%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EMul (EVar IDjpeg_fdct_islow_z4) (ENum (-3196))))),
             75%positive)::
             (75%positive,(AAssign IDjpeg_fdct_islow_z3
             (Some (EAdd (EVar IDjpeg_fdct_islow_z3)
             (EVar IDjpeg_fdct_islow_z5)))),76%positive)::
             (76%positive,(AAssign IDjpeg_fdct_islow_z4
             (Some (EAdd (EVar IDjpeg_fdct_islow_z4)
             (EVar IDjpeg_fdct_islow_z5)))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDjpeg_fdct_islow_ctr
             (Some (EAdd (EVar IDjpeg_fdct_islow_ctr) (ENum (-1))))),
             79%positive)::(79%positive,ANone,80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDjpeg_fdct_islow_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_islow_z)))),82%positive)::
             (82%positive,AWeaken,5%positive)::nil
|}.

Definition jpeg_fdct_islow_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + 7 <= 0)%Z
    | 4%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ 1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_fdct_islow_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + 7 <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_fdct_islow_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 17%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 19%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 20%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 21%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 25%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 29%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 34%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 35%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 36%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 37%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 38%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 39%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 40%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 41%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 42%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 43%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 44%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 45%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0)%Z
    | 46%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 47%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) + 1 <= 0)%Z
    | 48%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 49%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 50%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 51%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 52%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 53%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 54%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 55%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 56%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 57%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 58%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 59%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 60%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 61%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 62%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 63%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 64%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 65%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 66%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 67%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 68%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 69%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 70%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 71%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 72%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 73%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 74%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 75%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 76%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 77%positive => (-1 * (s IDjpeg_fdct_islow_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0)%Z
    | 78%positive => (1 * (s IDjpeg_fdct_islow_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) <= 0)%Z
    | 79%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 80%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) <= 0)%Z
    | 81%positive => (-1 * (s IDjpeg_fdct_islow_z) <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0)%Z
    | 82%positive => (-1 * (s IDjpeg_fdct_islow_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_islow_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_islow_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_fdct_islow_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjpeg_fdct_islow_z))%Q
    | 3%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 4%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 5%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 6%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 7%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z))%Q
    | 8%positive => ((s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 9%positive => ((s IDjpeg_fdct_islow_z)
                     + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 10%positive => ((s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 11%positive => ((s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 12%positive => ((s IDjpeg_fdct_islow_z))%Q
    | 13%positive => ((s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 14%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 19%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 20%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 21%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 22%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 23%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 24%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 25%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 26%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 27%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 28%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 29%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 30%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 31%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 32%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 33%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 34%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 35%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 36%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 37%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 38%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 39%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 40%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 41%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 42%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 43%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 44%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 45%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 46%positive => ((1 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 47%positive => ((s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 48%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 49%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 50%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 51%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 52%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 53%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 54%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 55%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 56%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 57%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 58%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 59%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 60%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 61%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 62%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 63%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 64%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 65%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 66%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 67%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 68%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 69%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 70%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 71%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 72%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 73%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 74%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 75%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 76%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 77%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 78%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0((s IDjpeg_fdct_islow_ctr)))%Q
    | 79%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 80%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 81%positive => ((9 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | 82%positive => ((8 # 1) + (s IDjpeg_fdct_islow_z)
                      + max0(1 + (s IDjpeg_fdct_islow_ctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_fdct_islow_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDjpeg_fdct_islow_ctr)) ((s IDjpeg_fdct_islow_ctr)));
                     (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_islow_ctr))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDjpeg_fdct_islow_ctr)) ((s IDjpeg_fdct_islow_ctr)));
                      (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_islow_ctr))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_islow_ctr)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
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
    | 48%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_islow_ctr)) (1)]
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
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | _ => []
  end.


Theorem jpeg_fdct_islow_ai_correct:
  forall s p' s', steps (g_start jpeg_fdct_islow) s (g_edges jpeg_fdct_islow) p' s' -> jpeg_fdct_islow_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_fdct_islow_pot_correct:
  forall s p' s',
    steps (g_start jpeg_fdct_islow) s (g_edges jpeg_fdct_islow) p' s' ->
    (jpeg_fdct_islow_pot (g_start jpeg_fdct_islow) s >= jpeg_fdct_islow_pot p' s')%Q.
Proof.
  check_lp jpeg_fdct_islow_ai_correct jpeg_fdct_islow_hints.
Qed.

