Require Import pasta.Pasta.

Notation IDjpeg_idct_2x2_z := 1%positive.
Notation IDjpeg_idct_2x2__tmp := 2%positive.
Notation IDjpeg_idct_2x2_ctr := 3%positive.
Notation IDjpeg_idct_2x2_dcval := 4%positive.
Notation IDjpeg_idct_2x2_dcval1 := 5%positive.
Notation IDjpeg_idct_2x2_tmp0 := 6%positive.
Notation IDjpeg_idct_2x2_tmp10 := 7%positive.
Notation IDjpeg_idct_2x2_z1 := 8%positive.
Notation IDjpeg_idct_2x2_cinfo := 9%positive.
Notation IDjpeg_idct_2x2_coef_block := 10%positive.
Notation IDjpeg_idct_2x2_compptr := 11%positive.
Notation IDjpeg_idct_2x2_output_buf := 12%positive.
Notation IDjpeg_idct_2x2_output_col := 13%positive.
Definition jpeg_idct_2x2 : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDjpeg_idct_2x2_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_idct_2x2__tmp
             (Some (EVar IDjpeg_idct_2x2_output_col))),3%positive)::
             (3%positive,(AAssign IDjpeg_idct_2x2_ctr (Some (ENum (8)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) > (eval (ENum (0)) s))%Z)),26%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDjpeg_idct_2x2_ctr (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) < (eval (ENum (2)) s))%Z)),14%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) >= (eval (ENum (2)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,19%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDjpeg_idct_2x2_tmp10 None),17%positive)::
             (17%positive,(AAssign IDjpeg_idct_2x2_tmp0 None),18%positive)::
             (18%positive,ANone,21%positive)::
             (19%positive,(AAssign IDjpeg_idct_2x2_dcval1 None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDjpeg_idct_2x2_ctr
             (Some (EAdd (EVar IDjpeg_idct_2x2_ctr) (ENum (1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDjpeg_idct_2x2_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_2x2_z)))),25%positive)::
             (25%positive,AWeaken,11%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) = (eval (ENum (6)) s))%Z)),49%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) <> (eval (ENum (6)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) = (eval (ENum (4)) s))%Z)),48%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) <> (eval (ENum (4)) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) = (eval (ENum (2)) s))%Z)),47%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDjpeg_idct_2x2_ctr)
             s) <> (eval (ENum (2)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,45%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDjpeg_idct_2x2_z1 None),35%positive)::
             (35%positive,(AAssign IDjpeg_idct_2x2_tmp10 None),36%positive)::
             (36%positive,(AAssign IDjpeg_idct_2x2_z1 None),37%positive)::
             (37%positive,(AAssign IDjpeg_idct_2x2_tmp0
             (Some (EMul (EVar IDjpeg_idct_2x2_z1) (ENum (-5906))))),
             38%positive)::
             (38%positive,(AAssign IDjpeg_idct_2x2_z1 None),39%positive)::
             (39%positive,(AAssign IDjpeg_idct_2x2_tmp0
             (Some (EAdd (EVar IDjpeg_idct_2x2_tmp0)
             (EMul (EVar IDjpeg_idct_2x2_z1) (ENum (6967)))))),40%positive)::
             (40%positive,(AAssign IDjpeg_idct_2x2_z1 None),41%positive)::
             (41%positive,(AAssign IDjpeg_idct_2x2_tmp0 None),42%positive)::
             (42%positive,(AAssign IDjpeg_idct_2x2_z1 None),43%positive)::
             (43%positive,(AAssign IDjpeg_idct_2x2_tmp0 None),44%positive)::
             (44%positive,ANone,51%positive)::
             (45%positive,(AAssign IDjpeg_idct_2x2_dcval None),46%positive)::
             (46%positive,ANone,51%positive)::
             (47%positive,AWeaken,50%positive)::
             (48%positive,AWeaken,50%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDjpeg_idct_2x2_ctr
             (Some (EAdd (EVar IDjpeg_idct_2x2_ctr) (ENum (-1))))),
             52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDjpeg_idct_2x2_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_2x2_z)))),55%positive)::
             (55%positive,AWeaken,6%positive)::nil
|}.

Definition jpeg_idct_2x2_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 8 <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_idct_2x2_ctr) + 8 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 7%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 8%positive => (1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 2 <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_idct_2x2_ctr) + 2 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0)%Z
    | 17%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0)%Z
    | 21%positive => (-1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0)%Z
    | 25%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 29%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 34%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 35%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 36%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 37%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 38%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 39%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 40%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 41%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 42%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 44%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 45%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0)%Z
    | 46%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 47%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -2 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 2 <= 0)%Z
    | 48%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -4 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 4 <= 0)%Z
    | 49%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) + 6 <= 0)%Z
    | 50%positive => (-1 * (s IDjpeg_idct_2x2_ctr) + 2 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 51%positive => (-1 * (s IDjpeg_idct_2x2_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 52%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -7 <= 0)%Z
    | 53%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) <= 0)%Z
    | 54%positive => (-1 * (s IDjpeg_idct_2x2_z) <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ 1 * (s IDjpeg_idct_2x2_ctr) + -7 <= 0)%Z
    | 55%positive => (1 * (s IDjpeg_idct_2x2_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_2x2_ctr) <= 0 /\ -1 * (s IDjpeg_idct_2x2_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_idct_2x2_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((10 # 1))%Q
    | 2%positive => ((10 # 1) + (s IDjpeg_idct_2x2_z))%Q
    | 3%positive => ((10 # 1) + (s IDjpeg_idct_2x2_z))%Q
    | 4%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                     + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 5%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                     + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 6%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                     + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 7%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                     + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 8%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z))%Q
    | 9%positive => ((s IDjpeg_idct_2x2_z)
                     + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 10%positive => ((s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 11%positive => ((s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 12%positive => ((s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 13%positive => ((s IDjpeg_idct_2x2_z))%Q
    | 14%positive => ((s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 19%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 20%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 21%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(1 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 22%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 23%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 24%positive => ((1 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 25%positive => ((s IDjpeg_idct_2x2_z)
                      + max0(2 - (s IDjpeg_idct_2x2_ctr)))%Q
    | 26%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 27%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 28%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 29%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 30%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 31%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 32%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 33%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 34%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 35%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 36%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 37%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 38%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 39%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 40%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 41%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 42%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 43%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 44%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 45%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 46%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 47%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 48%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 49%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 50%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 51%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0(-1 + (s IDjpeg_idct_2x2_ctr)))%Q
    | 52%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 53%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 54%positive => ((3 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | 55%positive => ((2 # 1) + (s IDjpeg_idct_2x2_z)
                      + max0((s IDjpeg_idct_2x2_ctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_idct_2x2_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_ge_0 ((s IDjpeg_idct_2x2_ctr))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (2
                                                             - (s IDjpeg_idct_2x2_ctr)) (1
                                                                    - (s IDjpeg_idct_2x2_ctr)));
                      (*-1 0*) F_max0_ge_0 (1 - (s IDjpeg_idct_2x2_ctr))]
    | 13%positive => []
    | 14%positive => [(*0 1*) F_max0_pre_decrement (2
                                                    - (s IDjpeg_idct_2x2_ctr)) (1)]
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
    | 26%positive => [(*0 1*) F_max0_pre_decrement ((s IDjpeg_idct_2x2_ctr)) (1)]
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
    | _ => []
  end.


Theorem jpeg_idct_2x2_ai_correct:
  forall s p' s', steps (g_start jpeg_idct_2x2) s (g_edges jpeg_idct_2x2) p' s' -> jpeg_idct_2x2_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_idct_2x2_pot_correct:
  forall s p' s',
    steps (g_start jpeg_idct_2x2) s (g_edges jpeg_idct_2x2) p' s' ->
    (jpeg_idct_2x2_pot (g_start jpeg_idct_2x2) s >= jpeg_idct_2x2_pot p' s')%Q.
Proof.
  check_lp jpeg_idct_2x2_ai_correct jpeg_idct_2x2_hints.
Qed.

