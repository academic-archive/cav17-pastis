Require Import pasta.Pasta.

Notation IDjpeg_fdct_ifast_z := 1%positive.
Notation IDjpeg_fdct_ifast_ctr := 2%positive.
Notation IDjpeg_fdct_ifast_tmp0 := 3%positive.
Notation IDjpeg_fdct_ifast_tmp1 := 4%positive.
Notation IDjpeg_fdct_ifast_tmp10 := 5%positive.
Notation IDjpeg_fdct_ifast_tmp11 := 6%positive.
Notation IDjpeg_fdct_ifast_tmp12 := 7%positive.
Notation IDjpeg_fdct_ifast_tmp13 := 8%positive.
Notation IDjpeg_fdct_ifast_tmp2 := 9%positive.
Notation IDjpeg_fdct_ifast_tmp3 := 10%positive.
Notation IDjpeg_fdct_ifast_tmp4 := 11%positive.
Notation IDjpeg_fdct_ifast_tmp5 := 12%positive.
Notation IDjpeg_fdct_ifast_tmp6 := 13%positive.
Notation IDjpeg_fdct_ifast_tmp7 := 14%positive.
Notation IDjpeg_fdct_ifast_z1 := 15%positive.
Notation IDjpeg_fdct_ifast_z11 := 16%positive.
Notation IDjpeg_fdct_ifast_z13 := 17%positive.
Notation IDjpeg_fdct_ifast_z2 := 18%positive.
Notation IDjpeg_fdct_ifast_z3 := 19%positive.
Notation IDjpeg_fdct_ifast_z4 := 20%positive.
Notation IDjpeg_fdct_ifast_z5 := 21%positive.
Notation IDjpeg_fdct_ifast_data := 22%positive.
Definition jpeg_fdct_ifast : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDjpeg_fdct_ifast_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_fdct_ifast_ctr (Some (ENum (7)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_ifast_ctr) s) >=
             (eval (ENum (0)) s))%Z)),42%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_ifast_ctr) s) <
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDjpeg_fdct_ifast_ctr (Some (ENum (7)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_ifast_ctr) s) >=
             (eval (ENum (0)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_ifast_ctr) s) <
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDjpeg_fdct_ifast_tmp0 None),15%positive)::
             (15%positive,(AAssign IDjpeg_fdct_ifast_tmp7 None),16%positive)::
             (16%positive,(AAssign IDjpeg_fdct_ifast_tmp1 None),17%positive)::
             (17%positive,(AAssign IDjpeg_fdct_ifast_tmp6 None),18%positive)::
             (18%positive,(AAssign IDjpeg_fdct_ifast_tmp2 None),19%positive)::
             (19%positive,(AAssign IDjpeg_fdct_ifast_tmp5 None),20%positive)::
             (20%positive,(AAssign IDjpeg_fdct_ifast_tmp3 None),21%positive)::
             (21%positive,(AAssign IDjpeg_fdct_ifast_tmp4 None),22%positive)::
             (22%positive,(AAssign IDjpeg_fdct_ifast_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp0)
             (EVar IDjpeg_fdct_ifast_tmp3)))),23%positive)::
             (23%positive,(AAssign IDjpeg_fdct_ifast_tmp13
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp0)
             (EVar IDjpeg_fdct_ifast_tmp3)))),24%positive)::
             (24%positive,(AAssign IDjpeg_fdct_ifast_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp1)
             (EVar IDjpeg_fdct_ifast_tmp2)))),25%positive)::
             (25%positive,(AAssign IDjpeg_fdct_ifast_tmp12
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp1)
             (EVar IDjpeg_fdct_ifast_tmp2)))),26%positive)::
             (26%positive,(AAssign IDjpeg_fdct_ifast_z1 None),27%positive)::
             (27%positive,(AAssign IDjpeg_fdct_ifast_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp4)
             (EVar IDjpeg_fdct_ifast_tmp5)))),28%positive)::
             (28%positive,(AAssign IDjpeg_fdct_ifast_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp5)
             (EVar IDjpeg_fdct_ifast_tmp6)))),29%positive)::
             (29%positive,(AAssign IDjpeg_fdct_ifast_tmp12
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp6)
             (EVar IDjpeg_fdct_ifast_tmp7)))),30%positive)::
             (30%positive,(AAssign IDjpeg_fdct_ifast_z5 None),31%positive)::
             (31%positive,(AAssign IDjpeg_fdct_ifast_z2 None),32%positive)::
             (32%positive,(AAssign IDjpeg_fdct_ifast_z4 None),33%positive)::
             (33%positive,(AAssign IDjpeg_fdct_ifast_z3 None),34%positive)::
             (34%positive,(AAssign IDjpeg_fdct_ifast_z11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp7)
             (EVar IDjpeg_fdct_ifast_z3)))),35%positive)::
             (35%positive,(AAssign IDjpeg_fdct_ifast_z13
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp7)
             (EVar IDjpeg_fdct_ifast_z3)))),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDjpeg_fdct_ifast_ctr
             (Some (EAdd (EVar IDjpeg_fdct_ifast_ctr) (ENum (-1))))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDjpeg_fdct_ifast_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_ifast_z)))),41%positive)::
             (41%positive,AWeaken,10%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDjpeg_fdct_ifast_tmp0 None),44%positive)::
             (44%positive,(AAssign IDjpeg_fdct_ifast_tmp7 None),45%positive)::
             (45%positive,(AAssign IDjpeg_fdct_ifast_tmp1 None),46%positive)::
             (46%positive,(AAssign IDjpeg_fdct_ifast_tmp6 None),47%positive)::
             (47%positive,(AAssign IDjpeg_fdct_ifast_tmp2 None),48%positive)::
             (48%positive,(AAssign IDjpeg_fdct_ifast_tmp5 None),49%positive)::
             (49%positive,(AAssign IDjpeg_fdct_ifast_tmp3 None),50%positive)::
             (50%positive,(AAssign IDjpeg_fdct_ifast_tmp4 None),51%positive)::
             (51%positive,(AAssign IDjpeg_fdct_ifast_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp0)
             (EVar IDjpeg_fdct_ifast_tmp3)))),52%positive)::
             (52%positive,(AAssign IDjpeg_fdct_ifast_tmp13
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp0)
             (EVar IDjpeg_fdct_ifast_tmp3)))),53%positive)::
             (53%positive,(AAssign IDjpeg_fdct_ifast_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp1)
             (EVar IDjpeg_fdct_ifast_tmp2)))),54%positive)::
             (54%positive,(AAssign IDjpeg_fdct_ifast_tmp12
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp1)
             (EVar IDjpeg_fdct_ifast_tmp2)))),55%positive)::
             (55%positive,(AAssign IDjpeg_fdct_ifast_z1 None),56%positive)::
             (56%positive,(AAssign IDjpeg_fdct_ifast_tmp10
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp4)
             (EVar IDjpeg_fdct_ifast_tmp5)))),57%positive)::
             (57%positive,(AAssign IDjpeg_fdct_ifast_tmp11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp5)
             (EVar IDjpeg_fdct_ifast_tmp6)))),58%positive)::
             (58%positive,(AAssign IDjpeg_fdct_ifast_tmp12
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp6)
             (EVar IDjpeg_fdct_ifast_tmp7)))),59%positive)::
             (59%positive,(AAssign IDjpeg_fdct_ifast_z5 None),60%positive)::
             (60%positive,(AAssign IDjpeg_fdct_ifast_z2 None),61%positive)::
             (61%positive,(AAssign IDjpeg_fdct_ifast_z4 None),62%positive)::
             (62%positive,(AAssign IDjpeg_fdct_ifast_z3 None),63%positive)::
             (63%positive,(AAssign IDjpeg_fdct_ifast_z11
             (Some (EAdd (EVar IDjpeg_fdct_ifast_tmp7)
             (EVar IDjpeg_fdct_ifast_z3)))),64%positive)::
             (64%positive,(AAssign IDjpeg_fdct_ifast_z13
             (Some (ESub (EVar IDjpeg_fdct_ifast_tmp7)
             (EVar IDjpeg_fdct_ifast_z3)))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDjpeg_fdct_ifast_ctr
             (Some (EAdd (EVar IDjpeg_fdct_ifast_ctr) (ENum (-1))))),
             67%positive)::(67%positive,ANone,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDjpeg_fdct_ifast_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_ifast_z)))),70%positive)::
             (70%positive,AWeaken,5%positive)::nil
|}.

Definition jpeg_fdct_ifast_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + 7 <= 0)%Z
    | 4%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + 7 <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 17%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 19%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 20%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 21%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 25%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 29%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 34%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 35%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 36%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 37%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 38%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 39%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0)%Z
    | 40%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 41%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 43%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 44%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 45%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 46%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 47%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 48%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 49%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 50%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 51%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 52%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 53%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 54%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 55%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 56%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 57%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 58%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 59%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 60%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 61%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 62%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 63%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 64%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 65%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0)%Z
    | 66%positive => (1 * (s IDjpeg_fdct_ifast_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) <= 0)%Z
    | 67%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 68%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) <= 0)%Z
    | 69%positive => (-1 * (s IDjpeg_fdct_ifast_z) <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0)%Z
    | 70%positive => (-1 * (s IDjpeg_fdct_ifast_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_ifast_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_ifast_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_fdct_ifast_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjpeg_fdct_ifast_z))%Q
    | 3%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 4%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 5%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 6%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 7%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z))%Q
    | 8%positive => ((s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 9%positive => ((s IDjpeg_fdct_ifast_z)
                     + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 10%positive => ((s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 11%positive => ((s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 12%positive => ((s IDjpeg_fdct_ifast_z))%Q
    | 13%positive => ((s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 14%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 19%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 20%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 21%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 22%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 23%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 24%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 25%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 26%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 27%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 28%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 29%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 30%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 31%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 32%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 33%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 34%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 35%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 36%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 37%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 38%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 39%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 40%positive => ((1 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 41%positive => ((s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 42%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 43%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 44%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 45%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 46%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 47%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 48%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 49%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 50%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 51%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 52%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 53%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 54%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 55%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 56%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 57%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 58%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 59%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 60%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 61%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 62%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 63%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 64%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 65%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 66%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0((s IDjpeg_fdct_ifast_ctr)))%Q
    | 67%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 68%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 69%positive => ((9 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | 70%positive => ((8 # 1) + (s IDjpeg_fdct_ifast_z)
                      + max0(1 + (s IDjpeg_fdct_ifast_ctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_fdct_ifast_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDjpeg_fdct_ifast_ctr)) ((s IDjpeg_fdct_ifast_ctr)));
                     (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_ifast_ctr))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDjpeg_fdct_ifast_ctr)) ((s IDjpeg_fdct_ifast_ctr)));
                      (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_ifast_ctr))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_ifast_ctr)) (1)]
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
    | 42%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_ifast_ctr)) (1)]
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
    | _ => []
  end.


Theorem jpeg_fdct_ifast_ai_correct:
  forall s p' s', steps (g_start jpeg_fdct_ifast) s (g_edges jpeg_fdct_ifast) p' s' -> jpeg_fdct_ifast_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_fdct_ifast_pot_correct:
  forall s p' s',
    steps (g_start jpeg_fdct_ifast) s (g_edges jpeg_fdct_ifast) p' s' ->
    (jpeg_fdct_ifast_pot (g_start jpeg_fdct_ifast) s >= jpeg_fdct_ifast_pot p' s')%Q.
Proof.
  check_lp jpeg_fdct_ifast_ai_correct jpeg_fdct_ifast_hints.
Qed.

