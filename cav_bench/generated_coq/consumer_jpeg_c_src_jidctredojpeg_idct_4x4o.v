Require Import pasta.Pasta.

Notation IDjpeg_idct_4x4_z := 1%positive.
Notation IDjpeg_idct_4x4__tmp := 2%positive.
Notation IDjpeg_idct_4x4_ctr := 3%positive.
Notation IDjpeg_idct_4x4_dcval := 4%positive.
Notation IDjpeg_idct_4x4_dcval1 := 5%positive.
Notation IDjpeg_idct_4x4_tmp0 := 6%positive.
Notation IDjpeg_idct_4x4_tmp10 := 7%positive.
Notation IDjpeg_idct_4x4_tmp12 := 8%positive.
Notation IDjpeg_idct_4x4_tmp2 := 9%positive.
Notation IDjpeg_idct_4x4_z1 := 10%positive.
Notation IDjpeg_idct_4x4_z2 := 11%positive.
Notation IDjpeg_idct_4x4_z3 := 12%positive.
Notation IDjpeg_idct_4x4_z4 := 13%positive.
Notation IDjpeg_idct_4x4_cinfo := 14%positive.
Notation IDjpeg_idct_4x4_coef_block := 15%positive.
Notation IDjpeg_idct_4x4_compptr := 16%positive.
Notation IDjpeg_idct_4x4_output_buf := 17%positive.
Notation IDjpeg_idct_4x4_output_col := 18%positive.
Definition jpeg_idct_4x4 : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDjpeg_idct_4x4_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_idct_4x4__tmp
             (Some (EVar IDjpeg_idct_4x4_output_col))),3%positive)::
             (3%positive,(AAssign IDjpeg_idct_4x4_ctr (Some (ENum (8)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) > (eval (ENum (0)) s))%Z)),34%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDjpeg_idct_4x4_ctr (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) < (eval (ENum (4)) s))%Z)),14%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) >= (eval (ENum (4)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,27%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDjpeg_idct_4x4_tmp0 None),17%positive)::
             (17%positive,(AAssign IDjpeg_idct_4x4_tmp2 None),18%positive)::
             (18%positive,(AAssign IDjpeg_idct_4x4_tmp10
             (Some (EAdd (EVar IDjpeg_idct_4x4_tmp0)
             (EVar IDjpeg_idct_4x4_tmp2)))),19%positive)::
             (19%positive,(AAssign IDjpeg_idct_4x4_tmp12
             (Some (ESub (EVar IDjpeg_idct_4x4_tmp0)
             (EVar IDjpeg_idct_4x4_tmp2)))),20%positive)::
             (20%positive,(AAssign IDjpeg_idct_4x4_z1 None),21%positive)::
             (21%positive,(AAssign IDjpeg_idct_4x4_z2 None),22%positive)::
             (22%positive,(AAssign IDjpeg_idct_4x4_z3 None),23%positive)::
             (23%positive,(AAssign IDjpeg_idct_4x4_z4 None),24%positive)::
             (24%positive,(AAssign IDjpeg_idct_4x4_tmp0 None),25%positive)::
             (25%positive,(AAssign IDjpeg_idct_4x4_tmp2 None),26%positive)::
             (26%positive,ANone,29%positive)::
             (27%positive,(AAssign IDjpeg_idct_4x4_dcval1 None),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDjpeg_idct_4x4_ctr
             (Some (EAdd (EVar IDjpeg_idct_4x4_ctr) (ENum (1))))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDjpeg_idct_4x4_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_4x4_z)))),33%positive)::
             (33%positive,AWeaken,11%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) = (eval (ENum (4)) s))%Z)),54%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_4x4_ctr)
             s) <> (eval (ENum (4)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,52%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDjpeg_idct_4x4_tmp0 None),39%positive)::
             (39%positive,(AAssign IDjpeg_idct_4x4_tmp0 None),40%positive)::
             (40%positive,(AAssign IDjpeg_idct_4x4_z2 None),41%positive)::
             (41%positive,(AAssign IDjpeg_idct_4x4_z3 None),42%positive)::
             (42%positive,(AAssign IDjpeg_idct_4x4_tmp2 None),43%positive)::
             (43%positive,(AAssign IDjpeg_idct_4x4_tmp10
             (Some (EAdd (EVar IDjpeg_idct_4x4_tmp0)
             (EVar IDjpeg_idct_4x4_tmp2)))),44%positive)::
             (44%positive,(AAssign IDjpeg_idct_4x4_tmp12
             (Some (ESub (EVar IDjpeg_idct_4x4_tmp0)
             (EVar IDjpeg_idct_4x4_tmp2)))),45%positive)::
             (45%positive,(AAssign IDjpeg_idct_4x4_z1 None),46%positive)::
             (46%positive,(AAssign IDjpeg_idct_4x4_z2 None),47%positive)::
             (47%positive,(AAssign IDjpeg_idct_4x4_z3 None),48%positive)::
             (48%positive,(AAssign IDjpeg_idct_4x4_z4 None),49%positive)::
             (49%positive,(AAssign IDjpeg_idct_4x4_tmp0 None),50%positive)::
             (50%positive,(AAssign IDjpeg_idct_4x4_tmp2 None),51%positive)::
             (51%positive,ANone,56%positive)::
             (52%positive,(AAssign IDjpeg_idct_4x4_dcval None),53%positive)::
             (53%positive,ANone,56%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDjpeg_idct_4x4_ctr
             (Some (EAdd (EVar IDjpeg_idct_4x4_ctr) (ENum (-1))))),
             57%positive)::(57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,(AAssign IDjpeg_idct_4x4_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_4x4_z)))),60%positive)::
             (60%positive,AWeaken,6%positive)::nil
|}.

Definition jpeg_idct_4x4_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 8 <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_idct_4x4_ctr) + 8 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 7%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 8%positive => (1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_idct_4x4_ctr) + 4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 17%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 20%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 25%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 27%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0)%Z
    | 29%positive => (-1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -3 <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 36%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 37%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 38%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 39%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 40%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 41%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 42%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 44%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 45%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 46%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 47%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 48%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 49%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 50%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 51%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 52%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0)%Z
    | 53%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 54%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) + 4 <= 0)%Z
    | 55%positive => (-1 * (s IDjpeg_idct_4x4_ctr) + 4 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 56%positive => (-1 * (s IDjpeg_idct_4x4_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 57%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -7 <= 0)%Z
    | 58%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) <= 0)%Z
    | 59%positive => (-1 * (s IDjpeg_idct_4x4_z) <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ 1 * (s IDjpeg_idct_4x4_ctr) + -7 <= 0)%Z
    | 60%positive => (1 * (s IDjpeg_idct_4x4_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_4x4_ctr) <= 0 /\ -1 * (s IDjpeg_idct_4x4_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_idct_4x4_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((12 # 1))%Q
    | 2%positive => ((12 # 1) + (s IDjpeg_idct_4x4_z))%Q
    | 3%positive => ((12 # 1) + (s IDjpeg_idct_4x4_z))%Q
    | 4%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                     + (s IDjpeg_idct_4x4_z))%Q
    | 5%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                     + (s IDjpeg_idct_4x4_z))%Q
    | 6%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                     + (s IDjpeg_idct_4x4_z))%Q
    | 7%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                     + (s IDjpeg_idct_4x4_z))%Q
    | 8%positive => ((4 # 1) + (s IDjpeg_idct_4x4_z))%Q
    | 9%positive => ((s IDjpeg_idct_4x4_z)
                     + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 10%positive => ((s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 11%positive => ((s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 12%positive => ((s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 13%positive => ((s IDjpeg_idct_4x4_z))%Q
    | 14%positive => ((s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 19%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 20%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 21%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 22%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 23%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 24%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 25%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 26%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 27%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 28%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 29%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(3 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 30%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 31%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 32%positive => ((1 # 1) + (s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 33%positive => ((s IDjpeg_idct_4x4_z)
                      + max0(4 - (s IDjpeg_idct_4x4_ctr)))%Q
    | 34%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 35%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 36%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 37%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 38%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 39%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 40%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 41%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 42%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 43%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 44%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 45%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 46%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 47%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 48%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 49%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 50%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 51%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 52%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 53%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 54%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 55%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 56%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 57%positive => ((5 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 58%positive => ((5 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 59%positive => ((5 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | 60%positive => ((4 # 1) + (s IDjpeg_idct_4x4_ctr)
                      + (s IDjpeg_idct_4x4_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_idct_4x4_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDjpeg_idct_4x4_ctr)) (-1
                                                                    + (s IDjpeg_idct_4x4_ctr)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDjpeg_idct_4x4_ctr));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDjpeg_idct_4x4_ctr)) (0))) (F_max0_ge_0 ((s IDjpeg_idct_4x4_ctr)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_ge_0 (4 - (s IDjpeg_idct_4x4_ctr))]
    | 13%positive => []
    | 14%positive => [(*0 1*) F_max0_pre_decrement (4
                                                    - (s IDjpeg_idct_4x4_ctr)) (1)]
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
    | _ => []
  end.


Theorem jpeg_idct_4x4_ai_correct:
  forall s p' s', steps (g_start jpeg_idct_4x4) s (g_edges jpeg_idct_4x4) p' s' -> jpeg_idct_4x4_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_idct_4x4_pot_correct:
  forall s p' s',
    steps (g_start jpeg_idct_4x4) s (g_edges jpeg_idct_4x4) p' s' ->
    (jpeg_idct_4x4_pot (g_start jpeg_idct_4x4) s >= jpeg_idct_4x4_pot p' s')%Q.
Proof.
  check_lp jpeg_idct_4x4_ai_correct jpeg_idct_4x4_hints.
Qed.

