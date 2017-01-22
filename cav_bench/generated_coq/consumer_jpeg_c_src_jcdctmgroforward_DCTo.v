Require Import pasta.Pasta.

Notation IDforward_DCT_z := 1%positive.
Notation IDforward_DCT__tmp := 2%positive.
Notation IDforward_DCT__tmp1 := 3%positive.
Notation IDforward_DCT__tmp2 := 4%positive.
Notation IDforward_DCT_bi := 5%positive.
Notation IDforward_DCT_elemr := 6%positive.
Notation IDforward_DCT_i := 7%positive.
Notation IDforward_DCT_qval := 8%positive.
Notation IDforward_DCT_temp := 9%positive.
Notation IDforward_DCT_cinfo := 10%positive.
Notation IDforward_DCT_coef_blocks := 11%positive.
Notation IDforward_DCT_compptr := 12%positive.
Notation IDforward_DCT_num_blocks := 13%positive.
Notation IDforward_DCT_sample_data := 14%positive.
Notation IDforward_DCT_start_col := 15%positive.
Notation IDforward_DCT_start_row := 16%positive.
Definition forward_DCT : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDforward_DCT_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_bi)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDforward_DCT__tmp2
             (Some (EVar IDforward_DCT_start_row))),6%positive)::
             (6%positive,(AAssign IDforward_DCT__tmp1
             (Some (EVar IDforward_DCT_start_col))),7%positive)::
             (7%positive,(AAssign IDforward_DCT__tmp
             (Some (EVar IDforward_DCT_num_blocks))),8%positive)::
             (8%positive,(AAssign IDforward_DCT_bi (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_bi)
             s) < (eval (EVar IDforward_DCT__tmp) s))%Z)),14%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_bi)
             s) >= (eval (EVar IDforward_DCT__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDforward_DCT_elemr (Some (ENum (0)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_elemr)
             s) < (eval (ENum (8)) s))%Z)),67%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_elemr)
             s) >= (eval (ENum (8)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDforward_DCT_i (Some (ENum (0)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_i)
             s) < (eval (ENum (64)) s))%Z)),32%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_i)
             s) >= (eval (ENum (64)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDforward_DCT_bi
             (Some (EAdd (EVar IDforward_DCT_bi) (ENum (1))))),27%positive)::
             (27%positive,(AAssign IDforward_DCT__tmp1
             (Some (EAdd (EVar IDforward_DCT__tmp1) (ENum (8))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDforward_DCT_z (Some (EAdd (ENum (1))
             (EVar IDforward_DCT_z)))),31%positive)::
             (31%positive,AWeaken,11%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDforward_DCT_qval None),34%positive)::
             (34%positive,(AAssign IDforward_DCT_temp None),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) < (eval (ENum (0)) s))%Z)),48%positive)::
             (36%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) >= (eval (ENum (0)) s))%Z)),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDforward_DCT_temp None),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) >= (eval (EVar IDforward_DCT_qval) s))%Z)),44%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) < (eval (EVar IDforward_DCT_qval) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AAssign IDforward_DCT_temp (Some (ENum (0)))),
             43%positive)::(43%positive,ANone,47%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDforward_DCT_temp None),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,61%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDforward_DCT_temp (Some (ESub (ENum (0))
             (EVar IDforward_DCT_temp)))),50%positive)::
             (50%positive,(AAssign IDforward_DCT_temp None),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) >= (eval (EVar IDforward_DCT_qval) s))%Z)),56%positive)::
             (52%positive,(AGuard (fun s => ((eval (EVar IDforward_DCT_temp)
             s) < (eval (EVar IDforward_DCT_qval) s))%Z)),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AAssign IDforward_DCT_temp (Some (ENum (0)))),
             55%positive)::(55%positive,ANone,59%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AAssign IDforward_DCT_temp None),58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,(AAssign IDforward_DCT_temp (Some (ESub (ENum (0))
             (EVar IDforward_DCT_temp)))),60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,(AAssign IDforward_DCT_i
             (Some (EAdd (EVar IDforward_DCT_i) (ENum (1))))),63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDforward_DCT_z (Some (EAdd (ENum (1))
             (EVar IDforward_DCT_z)))),66%positive)::
             (66%positive,AWeaken,23%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDforward_DCT_elemr
             (Some (EAdd (EVar IDforward_DCT_elemr) (ENum (1))))),
             70%positive)::(70%positive,ANone,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,(AAssign IDforward_DCT_z (Some (EAdd (ENum (1))
             (EVar IDforward_DCT_z)))),73%positive)::
             (73%positive,AWeaken,18%positive)::nil
|}.

Definition forward_DCT_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0)%Z
    | 3%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 4%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDforward_DCT__tmp) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 6%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDforward_DCT__tmp) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 8%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0)%Z
    | 9%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 10%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0)%Z
    | 11%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 12%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT__tmp)+ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 13%positive => (1 * (s IDforward_DCT__tmp)+ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 14%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 16%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT_elemr) <= 0)%Z
    | 17%positive => (-1 * (s IDforward_DCT_elemr) <= 0 /\ 1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 18%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_elemr) + -8 <= 0)%Z
    | 19%positive => (1 * (s IDforward_DCT_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 20%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_elemr) + -8 <= 0)%Z
    | 21%positive => (1 * (s IDforward_DCT_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0)%Z
    | 22%positive => (-1 * (s IDforward_DCT_i) <= 0 /\ 1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ 1 * (s IDforward_DCT_elemr) + -8 <= 0)%Z
    | 23%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0)%Z
    | 24%positive => (1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) + 64 <= 0)%Z
    | 25%positive => (-1 * (s IDforward_DCT_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0)%Z
    | 26%positive => (1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) + 64 <= 0)%Z
    | 27%positive => (-1 * (s IDforward_DCT_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) <= 0)%Z
    | 28%positive => (-1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) + 64 <= 0)%Z
    | 29%positive => (-1 * (s IDforward_DCT_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) <= 0)%Z
    | 30%positive => (-1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) + 64 <= 0)%Z
    | 31%positive => (-1 * (s IDforward_DCT_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 33%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 34%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 35%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 36%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 37%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_temp) <= 0)%Z
    | 38%positive => (-1 * (s IDforward_DCT_temp) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 39%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 40%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 41%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_qval)+ 1 * (s IDforward_DCT_temp) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDforward_DCT_qval)+ 1 * (s IDforward_DCT_temp) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 43%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_temp) <= 0 /\ -1 * (s IDforward_DCT_temp) <= 0)%Z
    | 44%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_qval)+ -1 * (s IDforward_DCT_temp) <= 0)%Z
    | 45%positive => (1 * (s IDforward_DCT_qval)+ -1 * (s IDforward_DCT_temp) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 46%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 47%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 48%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_temp) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDforward_DCT_temp) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 50%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_temp) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 52%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 53%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_qval)+ 1 * (s IDforward_DCT_temp) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDforward_DCT_qval)+ 1 * (s IDforward_DCT_temp) + 1 <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 55%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0 /\ 1 * (s IDforward_DCT_temp) <= 0 /\ -1 * (s IDforward_DCT_temp) <= 0)%Z
    | 56%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0 /\ 1 * (s IDforward_DCT_qval)+ -1 * (s IDforward_DCT_temp) <= 0)%Z
    | 57%positive => (1 * (s IDforward_DCT_qval)+ -1 * (s IDforward_DCT_temp) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 58%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 59%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 60%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 61%positive => (1 * (s IDforward_DCT_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0)%Z
    | 62%positive => (-1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_i) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_i) + -63 <= 0)%Z
    | 63%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_i) + 1 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0)%Z
    | 64%positive => (1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_i) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0)%Z
    | 65%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_i) + 1 <= 0 /\ 1 * (s IDforward_DCT_i) + -64 <= 0)%Z
    | 66%positive => (1 * (s IDforward_DCT_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_i) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_elemr) + -7 <= 0)%Z
    | 68%positive => (1 * (s IDforward_DCT_elemr) + -7 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0)%Z
    | 69%positive => (-1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_elemr) <= 0 /\ -1 * (s IDforward_DCT_z) <= 0 /\ 1 * (s IDforward_DCT_elemr) + -7 <= 0)%Z
    | 70%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 1 <= 0 /\ 1 * (s IDforward_DCT_elemr) + -8 <= 0)%Z
    | 71%positive => (1 * (s IDforward_DCT_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) <= 0)%Z
    | 72%positive => (-1 * (s IDforward_DCT_z) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT_elemr) + 1 <= 0 /\ 1 * (s IDforward_DCT_elemr) + -8 <= 0)%Z
    | 73%positive => (1 * (s IDforward_DCT_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_elemr) + 1 <= 0 /\ -1 * (s IDforward_DCT_bi) <= 0 /\ -1 * (s IDforward_DCT__tmp)+ 1 * (s IDforward_DCT_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition forward_DCT_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 2%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 3%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 4%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 5%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 6%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 7%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT_num_blocks)))%Q
    | 8%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT__tmp)))%Q
    | 9%positive => ((s IDforward_DCT_z)
                     + (73 # 1) * max0((s IDforward_DCT__tmp)
                                       - (s IDforward_DCT_bi)))%Q
    | 10%positive => ((s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 11%positive => ((s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 12%positive => ((s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 13%positive => ((s IDforward_DCT_z))%Q
    | 14%positive => ((s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 15%positive => ((s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 16%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 17%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 18%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 19%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 20%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 21%positive => (-(64 # 1) - (s IDforward_DCT_elemr)
                      + (s IDforward_DCT_z) + max0(64 - (s IDforward_DCT_i))
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 22%positive => (-(64 # 1) - (s IDforward_DCT_elemr)
                      + (s IDforward_DCT_z) + max0(64 - (s IDforward_DCT_i))
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 23%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 24%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 25%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 26%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 27%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 28%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 29%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 30%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 31%positive => ((73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 32%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 33%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 34%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 35%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 36%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 37%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 38%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 39%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 40%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi)
                      - (9 # 8) * (s IDforward_DCT_i) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i))
                      - (9 # 8) * max0(64 - (s IDforward_DCT_i)))%Q
    | 41%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi)
                      - (9 # 8) * (s IDforward_DCT_i) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i))
                      - (9 # 8) * max0(64 - (s IDforward_DCT_i)))%Q
    | 42%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 43%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 44%positive => ((1 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi)
                      - (9 # 8) * (s IDforward_DCT_i) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i))
                      - (9 # 8) * max0(64 - (s IDforward_DCT_i)))%Q
    | 45%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 46%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 47%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 48%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 49%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 50%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 51%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 52%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 53%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 54%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 55%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 56%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 57%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 58%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 59%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 60%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 61%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 62%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(63 - (s IDforward_DCT_i)))%Q
    | 63%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 64%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 65%positive => (-(71 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 66%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT__tmp)
                      - (73 # 1) * (s IDforward_DCT_bi) + (s IDforward_DCT_z)
                      + max0(8 - (s IDforward_DCT_elemr))
                      + max0(64 - (s IDforward_DCT_i)))%Q
    | 67%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 68%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 69%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 70%positive => ((1 # 1) - (s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 71%positive => ((1 # 1) - (s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 72%positive => ((1 # 1) - (s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | 73%positive => (-(s IDforward_DCT_elemr) + (s IDforward_DCT_z)
                      + (73 # 1) * max0((s IDforward_DCT__tmp)
                                        - (s IDforward_DCT_bi)))%Q
    | _ => (0 # 1)%Q
  end.

Definition forward_DCT_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-73 0*) F_max0_monotonic (F_check_ge ((s IDforward_DCT__tmp)
                                                              - (s IDforward_DCT_bi)) (-1
                                                                    + (s IDforward_DCT__tmp)
                                                                    - (s IDforward_DCT_bi)));
                      (*-73 0*) F_max0_ge_0 (-1 + (s IDforward_DCT__tmp)
                                             - (s IDforward_DCT_bi))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-73 0*) F_max0_pre_decrement ((s IDforward_DCT__tmp)
                                                      - (s IDforward_DCT_bi)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDforward_DCT_elemr)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDforward_DCT_elemr)));
                      (*-73 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDforward_DCT__tmp)
                                                                    - 
                                                                    (s IDforward_DCT_bi))) (F_check_ge (-1
                                                                    + (s IDforward_DCT__tmp)
                                                                    - (s IDforward_DCT_bi)) (0))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-73 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDforward_DCT__tmp)
                                                                    - (s IDforward_DCT_bi)) (0))) (F_max0_ge_0 ((s IDforward_DCT__tmp)
                                                                    - (s IDforward_DCT_bi)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64
                                                                 - (s IDforward_DCT_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                 - (s IDforward_DCT_elemr))) (F_check_ge (0) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDforward_DCT_i)) (1)]
    | 38%positive => []
    | 39%positive => [(*0 1.125*) F_binom_monotonic 1 (F_max0_ge_arg (64
                                                                    - (s IDforward_DCT_i))) (F_check_ge (64
                                                                    - (s IDforward_DCT_i)) (0))]
    | 40%positive => []
    | 41%positive => [(*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDforward_DCT_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDforward_DCT_i)))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDforward_DCT_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDforward_DCT_i)))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDforward_DCT_i)) (1)]
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
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDforward_DCT_z))) (F_check_ge (-1
                                                                    + (s IDforward_DCT_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDforward_DCT_z)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDforward_DCT_z)))]
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | _ => []
  end.


Theorem forward_DCT_ai_correct:
  forall s p' s', steps (g_start forward_DCT) s (g_edges forward_DCT) p' s' -> forward_DCT_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem forward_DCT_pot_correct:
  forall s p' s',
    steps (g_start forward_DCT) s (g_edges forward_DCT) p' s' ->
    (forward_DCT_pot (g_start forward_DCT) s >= forward_DCT_pot p' s')%Q.
Proof.
  check_lp forward_DCT_ai_correct forward_DCT_hints.
Qed.

