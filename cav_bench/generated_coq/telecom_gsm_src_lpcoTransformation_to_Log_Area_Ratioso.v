Require Import pasta.Pasta.

Notation IDTransformation_to_Log_Area_Ratios_z := 1%positive.
Notation IDTransformation_to_Log_Area_Ratios_i := 2%positive.
Notation IDTransformation_to_Log_Area_Ratios_temp := 3%positive.
Notation IDTransformation_to_Log_Area_Ratios_r := 4%positive.
Definition Transformation_to_Log_Area_Ratios : graph := {|
  g_start := 1%positive;
  g_end := 61%positive;
  g_edges := (1%positive,(AAssign IDTransformation_to_Log_Area_Ratios_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDTransformation_to_Log_Area_Ratios_i
             (Some (ENum (1)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_i)
             s) <= (eval (ENum (8)) s))%Z)),9%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_i)
             s) > (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,61%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_temp)
             s) < (eval (ENum (0)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_temp)
             s) >= (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,22%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => True)),20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AGuard (fun s => True)),19%positive)::
             (19%positive,AWeaken,22%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_temp)
             s) >= (eval (ENum (0)) s))%Z)),28%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDTransformation_to_Log_Area_Ratios_temp)
             s) < (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,61%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,51%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,42%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,38%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,61%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),40%positive)::
             (40%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),41%positive)::(41%positive,ANone,49%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,46%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,61%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),48%positive)::(48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,54%positive)::
             (51%positive,(AAssign IDTransformation_to_Log_Area_Ratios_temp
             None),52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,57%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,59%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,62%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,AWeaken,61%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDTransformation_to_Log_Area_Ratios_i
             (Some (EAdd (EVar IDTransformation_to_Log_Area_Ratios_i)
             (ENum (1))))),65%positive)::(65%positive,ANone,66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDTransformation_to_Log_Area_Ratios_z
             (Some (EAdd (ENum (1))
             (EVar IDTransformation_to_Log_Area_Ratios_z)))),68%positive)::
             (68%positive,AWeaken,5%positive)::nil
|}.

Definition Transformation_to_Log_Area_Ratios_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0)%Z
    | 6%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 9 <= 0)%Z
    | 7%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0)%Z
    | 8%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 9 <= 0)%Z
    | 9%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 10%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 12%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 14%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 16%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 23%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 25%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 27%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_temp) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 29%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 30%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 31%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 32%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 33%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 34%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 35%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 36%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 37%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 38%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 39%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 40%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 42%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 43%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 44%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 45%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 46%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 47%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 48%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 50%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_temp) <= 0)%Z
    | 52%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 53%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 55%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 57%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 59%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 61%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 63%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_i) + 1 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -8 <= 0)%Z
    | 65%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 2 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0)%Z
    | 66%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 2 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0)%Z
    | 67%positive => (-1 * (s IDTransformation_to_Log_Area_Ratios_z) <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 2 <= 0 /\ 1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0)%Z
    | 68%positive => (1 * (s IDTransformation_to_Log_Area_Ratios_i) + -9 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_i) + 2 <= 0 /\ -1 * (s IDTransformation_to_Log_Area_Ratios_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Transformation_to_Log_Area_Ratios_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1)
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 3%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 4%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 5%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 6%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 7%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 8%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 9%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                     + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 10%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 11%positive => (max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 12%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 13%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 14%positive => ((10 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i))
                      - max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 15%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 16%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 17%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 18%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 19%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 20%positive => ((9 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 21%positive => ((10 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i))
                      - max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 22%positive => ((10 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i))
                      - max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 23%positive => ((10 # 1) - (s IDTransformation_to_Log_Area_Ratios_i)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i))
                      - max0(9 - (s IDTransformation_to_Log_Area_Ratios_i))
                      + max0((s IDTransformation_to_Log_Area_Ratios_z)))%Q
    | 24%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 25%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 26%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 27%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 28%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 29%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 30%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 31%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 32%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 33%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 34%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 35%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 36%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 37%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 38%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 39%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 40%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 41%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 42%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 43%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 44%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 45%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 46%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 47%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 48%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 49%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 50%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 51%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 52%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 53%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 54%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 55%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 56%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 57%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 58%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 59%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 60%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 61%positive => ((s IDTransformation_to_Log_Area_Ratios_z))%Q
    | 62%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 63%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 64%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(8 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 65%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(9 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 66%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(9 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 67%positive => ((1 # 1) + (s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(9 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | 68%positive => ((s IDTransformation_to_Log_Area_Ratios_z)
                      + max0(9 - (s IDTransformation_to_Log_Area_Ratios_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Transformation_to_Log_Area_Ratios_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (9
                                                            - (s IDTransformation_to_Log_Area_Ratios_i)) (8
                                                                    - (s IDTransformation_to_Log_Area_Ratios_i)));
                     (*-1 0*) F_max0_ge_0 (8
                                           - (s IDTransformation_to_Log_Area_Ratios_i));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTransformation_to_Log_Area_Ratios_z))) (F_check_ge ((s IDTransformation_to_Log_Area_Ratios_z)) (0))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                  - (s IDTransformation_to_Log_Area_Ratios_i))) (F_check_ge (9
                                                                    - (s IDTransformation_to_Log_Area_Ratios_i)) (0))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                     - (s IDTransformation_to_Log_Area_Ratios_i)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                     - (s IDTransformation_to_Log_Area_Ratios_i)) (1)]
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                     - (s IDTransformation_to_Log_Area_Ratios_i)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTransformation_to_Log_Area_Ratios_z))) (F_check_ge ((s IDTransformation_to_Log_Area_Ratios_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDTransformation_to_Log_Area_Ratios_i)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDTransformation_to_Log_Area_Ratios_i)))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (8
                                            - (s IDTransformation_to_Log_Area_Ratios_i))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (8
                                            - (s IDTransformation_to_Log_Area_Ratios_i))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (8
                                            - (s IDTransformation_to_Log_Area_Ratios_i))]
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
    | 60%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (8
                                            - (s IDTransformation_to_Log_Area_Ratios_i))]
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTransformation_to_Log_Area_Ratios_z)) (0))) (F_max0_ge_0 ((s IDTransformation_to_Log_Area_Ratios_z)))]
    | _ => []
  end.


Theorem Transformation_to_Log_Area_Ratios_ai_correct:
  forall s p' s', steps (g_start Transformation_to_Log_Area_Ratios) s (g_edges Transformation_to_Log_Area_Ratios) p' s' -> Transformation_to_Log_Area_Ratios_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Transformation_to_Log_Area_Ratios_pot_correct:
  forall s p' s',
    steps (g_start Transformation_to_Log_Area_Ratios) s (g_edges Transformation_to_Log_Area_Ratios) p' s' ->
    (Transformation_to_Log_Area_Ratios_pot (g_start Transformation_to_Log_Area_Ratios) s >= Transformation_to_Log_Area_Ratios_pot p' s')%Q.
Proof.
  check_lp Transformation_to_Log_Area_Ratios_ai_correct Transformation_to_Log_Area_Ratios_hints.
Qed.

