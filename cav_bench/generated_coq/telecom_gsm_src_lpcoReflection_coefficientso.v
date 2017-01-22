Require Import pasta.Pasta.

Notation IDReflection_coefficients_z := 1%positive.
Notation IDReflection_coefficients_L_ACF_dref_off0 := 2%positive.
Notation IDReflection_coefficients_P_off0 := 3%positive.
Notation IDReflection_coefficients_P_off2 := 4%positive.
Notation IDReflection_coefficients_i := 5%positive.
Notation IDReflection_coefficients_ltmp := 6%positive.
Notation IDReflection_coefficients_m := 7%positive.
Notation IDReflection_coefficients_n := 8%positive.
Notation IDReflection_coefficients_temp := 9%positive.
Notation IDReflection_coefficients_L_ACF := 10%positive.
Notation IDReflection_coefficients_r := 11%positive.
Definition Reflection_coefficients : graph := {|
  g_start := 1%positive;
  g_end := 161%positive;
  g_edges := (1%positive,(AAssign IDReflection_coefficients_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_L_ACF_dref_off0)
             s) = (eval (ENum (0)) s))%Z)),152%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_L_ACF_dref_off0)
             s) <> (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_L_ACF_dref_off0)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_L_ACF_dref_off0)
             s) = (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,161%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDReflection_coefficients_temp None),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) >=
             (eval (ENum (0)) s))%Z)),15%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) <
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,18%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) <
             (eval (ENum (32)) s))%Z)),20%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) >=
             (eval (ENum (32)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,161%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDReflection_coefficients_i
             (Some (ENum (0)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) <=
             (eval (ENum (8)) s))%Z)),145%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) >
             (eval (ENum (8)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDReflection_coefficients_i
             (Some (ENum (1)))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) <=
             (eval (ENum (7)) s))%Z)),138%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) >
             (eval (ENum (7)) s))%Z)),31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AAssign IDReflection_coefficients_i
             (Some (ENum (0)))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) <=
             (eval (ENum (8)) s))%Z)),131%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) >
             (eval (ENum (8)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDReflection_coefficients_n
             (Some (ENum (1)))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_n) s) <=
             (eval (ENum (8)) s))%Z)),42%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_n) s) >
             (eval (ENum (8)) s))%Z)),41%positive)::
             (41%positive,AWeaken,161%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDReflection_coefficients_temp
             (Some (EVar IDReflection_coefficients_P_off2))),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) <
             (eval (ENum (0)) s))%Z)),48%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_temp) s) >=
             (eval (ENum (0)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,55%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard (fun s => True)),53%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AGuard (fun s => True)),52%positive)::
             (52%positive,AWeaken,55%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDReflection_coefficients_temp None),
             56%positive)::(56%positive,AWeaken,57%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_P_off0) s) <
             (eval (EVar IDReflection_coefficients_temp) s))%Z)),
             116%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_P_off0) s) >=
             (eval (EVar IDReflection_coefficients_temp) s))%Z)),58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,62%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,161%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_P_off2) s) >
             (eval (ENum (0)) s))%Z)),66%positive)::
             (64%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_P_off2) s) <=
             (eval (ENum (0)) s))%Z)),65%positive)::
             (65%positive,AWeaken,69%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,72%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,AWeaken,161%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_n) s) =
             (eval (ENum (8)) s))%Z)),113%positive)::
             (74%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_n) s) <>
             (eval (ENum (8)) s))%Z)),75%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AAssign IDReflection_coefficients_temp None),
             77%positive)::
             (77%positive,(AAssign IDReflection_coefficients_ltmp
             (Some (EAdd (EVar IDReflection_coefficients_P_off0)
             (EVar IDReflection_coefficients_temp)))),78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,ANone,81%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,ANone,82%positive)::
             (81%positive,ANone,82%positive)::
             (82%positive,(AAssign IDReflection_coefficients_P_off0 None),
             83%positive)::
             (83%positive,(AAssign IDReflection_coefficients_m
             (Some (ENum (1)))),84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_m) s) <=
             (eval (ESub (ENum (8)) (EVar IDReflection_coefficients_n))
             s))%Z)),94%positive)::
             (86%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_m) s) >
             (eval (ESub (ENum (8)) (EVar IDReflection_coefficients_n))
             s))%Z)),87%positive)::(87%positive,AWeaken,88%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,(AAssign IDReflection_coefficients_n
             (Some (EAdd (EVar IDReflection_coefficients_n) (ENum (1))))),
             90%positive)::(90%positive,ANone,91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             93%positive)::(93%positive,AWeaken,40%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,(AAssign IDReflection_coefficients_temp None),
             96%positive)::
             (96%positive,(AAssign IDReflection_coefficients_ltmp None),
             97%positive)::(97%positive,AWeaken,98%positive)::
             (98%positive,ANone,100%positive)::
             (98%positive,ANone,99%positive)::
             (99%positive,ANone,101%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,(AAssign IDReflection_coefficients_temp None),
             102%positive)::
             (102%positive,(AAssign IDReflection_coefficients_ltmp None),
             103%positive)::(103%positive,AWeaken,104%positive)::
             (104%positive,ANone,106%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,ANone,107%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,ANone,108%positive)::
             (108%positive,(AAssign IDReflection_coefficients_m
             (Some (EAdd (EVar IDReflection_coefficients_m) (ENum (1))))),
             109%positive)::(109%positive,ANone,110%positive)::
             (110%positive,ANone,111%positive)::
             (111%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             112%positive)::(112%positive,AWeaken,86%positive)::
             (113%positive,AWeaken,114%positive)::
             (114%positive,ANone,115%positive)::
             (115%positive,AWeaken,161%positive)::
             (116%positive,AWeaken,117%positive)::
             (117%positive,(AAssign IDReflection_coefficients_i
             (Some (EVar IDReflection_coefficients_n))),118%positive)::
             (118%positive,ANone,119%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) <=
             (eval (ENum (8)) s))%Z)),124%positive)::
             (120%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) >
             (eval (ENum (8)) s))%Z)),121%positive)::
             (121%positive,AWeaken,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,AWeaken,161%positive)::
             (124%positive,AWeaken,125%positive)::
             (125%positive,ANone,126%positive)::
             (126%positive,(AAssign IDReflection_coefficients_i
             (Some (EAdd (EVar IDReflection_coefficients_i) (ENum (1))))),
             127%positive)::(127%positive,ANone,128%positive)::
             (128%positive,ANone,129%positive)::
             (129%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             130%positive)::(130%positive,AWeaken,120%positive)::
             (131%positive,AWeaken,132%positive)::
             (132%positive,ANone,133%positive)::
             (133%positive,(AAssign IDReflection_coefficients_i
             (Some (EAdd (EVar IDReflection_coefficients_i) (ENum (1))))),
             134%positive)::(134%positive,ANone,135%positive)::
             (135%positive,ANone,136%positive)::
             (136%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             137%positive)::(137%positive,AWeaken,35%positive)::
             (138%positive,AWeaken,139%positive)::
             (139%positive,ANone,140%positive)::
             (140%positive,(AAssign IDReflection_coefficients_i
             (Some (EAdd (EVar IDReflection_coefficients_i) (ENum (1))))),
             141%positive)::(141%positive,ANone,142%positive)::
             (142%positive,ANone,143%positive)::
             (143%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             144%positive)::(144%positive,AWeaken,30%positive)::
             (145%positive,AWeaken,146%positive)::
             (146%positive,ANone,147%positive)::
             (147%positive,(AAssign IDReflection_coefficients_i
             (Some (EAdd (EVar IDReflection_coefficients_i) (ENum (1))))),
             148%positive)::(148%positive,ANone,149%positive)::
             (149%positive,ANone,150%positive)::
             (150%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             151%positive)::(151%positive,AWeaken,25%positive)::
             (152%positive,AWeaken,153%positive)::
             (153%positive,(AAssign IDReflection_coefficients_i
             (Some (ENum (8)))),154%positive)::
             (154%positive,ANone,155%positive)::
             (155%positive,(AAssign IDReflection_coefficients_i
             (Some (EAdd (EVar IDReflection_coefficients_i) (ENum (-1))))),
             156%positive)::(156%positive,AWeaken,157%positive)::
             (157%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) <>
             (eval (ENum (0)) s))%Z)),162%positive)::
             (157%positive,(AGuard
             (fun s => ((eval (EVar IDReflection_coefficients_i) s) =
             (eval (ENum (0)) s))%Z)),158%positive)::
             (158%positive,AWeaken,159%positive)::
             (159%positive,ANone,160%positive)::
             (160%positive,AWeaken,161%positive)::
             (162%positive,AWeaken,163%positive)::
             (163%positive,ANone,164%positive)::
             (164%positive,ANone,165%positive)::
             (165%positive,ANone,166%positive)::
             (166%positive,(AAssign IDReflection_coefficients_z
             (Some (EAdd (ENum (1)) (EVar IDReflection_coefficients_z)))),
             155%positive)::nil
|}.

Definition Reflection_coefficients_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 3%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 4%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 5%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 6%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 7%positive => (-1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 8%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 9%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 10%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 11%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 12%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 13%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 14%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 16%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 17%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) + 32 <= 0)%Z
    | 18%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 19%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 20%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0)%Z
    | 21%positive => (1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 22%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0)%Z
    | 23%positive => (1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0)%Z
    | 24%positive => (-1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0)%Z
    | 25%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 26%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 27%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 28%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 30%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 8 <= 0)%Z
    | 32%positive => (-1 * (s IDReflection_coefficients_i) + 8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 33%positive => (1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0)%Z
    | 34%positive => (-1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0)%Z
    | 35%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 36%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 37%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 38%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 40%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 41%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 9 <= 0)%Z
    | 42%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 44%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 45%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 46%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 47%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 48%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 50%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 52%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 54%positive => (1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 55%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 56%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 57%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 58%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 59%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 60%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 61%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 62%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 63%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 64%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 65%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_P_off2) <= 0)%Z
    | 66%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_P_off2) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDReflection_coefficients_P_off2) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 68%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_P_off2) + 1 <= 0)%Z
    | 69%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 70%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 71%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 72%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 73%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 74%positive => (-1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 75%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0)%Z
    | 76%positive => (1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 77%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0)%Z
    | 78%positive => (1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 79%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0)%Z
    | 80%positive => (1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 81%positive => (1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 82%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0)%Z
    | 83%positive => (1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 84%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ 1 * (s IDReflection_coefficients_m) + -1 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0)%Z
    | 85%positive => (-1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_m) + -1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 86%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0)%Z
    | 87%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 9 <= 0)%Z
    | 88%positive => (-1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0)%Z
    | 89%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 9 <= 0)%Z
    | 90%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -10 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 10 <= 0)%Z
    | 91%positive => (-1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 10 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -10 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 92%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -10 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 10 <= 0)%Z
    | 93%positive => (-1 * (s IDReflection_coefficients_m)+ -1 * (s IDReflection_coefficients_n) + 10 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -10 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 94%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 95%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 97%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 98%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 99%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 100%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 101%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 102%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 103%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 104%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 106%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 107%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0)%Z
    | 108%positive => (-1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 109%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0)%Z
    | 110%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 2 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 111%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0)%Z
    | 112%positive => (1 * (s IDReflection_coefficients_m)+ 1 * (s IDReflection_coefficients_n) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_m) + 2 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 113%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 8 <= 0)%Z
    | 114%positive => (-1 * (s IDReflection_coefficients_n) + 8 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 115%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_P_off0)+ 1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 8 <= 0)%Z
    | 116%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 117%positive => (1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 118%positive => (1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0)%Z
    | 119%positive => (-1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0)%Z
    | 120%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 121%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 122%positive => (-1 * (s IDReflection_coefficients_i) + 9 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 123%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 9 <= 0)%Z
    | 124%positive => (1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 125%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0)%Z
    | 126%positive => (1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 127%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 128%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 129%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 130%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_P_off0)+ -1 * (s IDReflection_coefficients_temp) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_n) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_n) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 131%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 132%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 133%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 134%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 135%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 136%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 137%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 138%positive => (1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | 139%positive => (1 * (s IDReflection_coefficients_i) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0)%Z
    | 140%positive => (1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | 141%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 142%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 143%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 144%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 2 <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 145%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 146%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0)%Z
    | 147%positive => (-1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0)%Z
    | 148%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 149%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 150%positive => (-1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ 1 * (s IDReflection_coefficients_i) + -9 <= 0)%Z
    | 151%positive => (1 * (s IDReflection_coefficients_i) + -9 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 1 <= 0 /\ -1 * (s IDReflection_coefficients_temp) <= 0 /\ 1 * (s IDReflection_coefficients_temp) + -31 <= 0 /\ -1 * (s IDReflection_coefficients_z) + 1 <= 0)%Z
    | 152%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 153%positive => (-1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 154%positive => (1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_i) + 8 <= 0)%Z
    | 155%positive => (1 * (s IDReflection_coefficients_i) + -8 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 156%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | 157%positive => (1 * (s IDReflection_coefficients_i) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 158%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0)%Z
    | 159%positive => (-1 * (s IDReflection_coefficients_i) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 160%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) <= 0 /\ -1 * (s IDReflection_coefficients_i) <= 0)%Z
    | 161%positive => (-1 * (s IDReflection_coefficients_z) <= 0)%Z
    | 162%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | 163%positive => (1 * (s IDReflection_coefficients_i) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 164%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | 165%positive => (1 * (s IDReflection_coefficients_i) + -7 <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ 1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0)%Z
    | 166%positive => (1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_L_ACF_dref_off0) <= 0 /\ -1 * (s IDReflection_coefficients_z) <= 0 /\ 1 * (s IDReflection_coefficients_i) + -7 <= 0)%Z
    | _ => False
  end.

Definition Reflection_coefficients_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((82 # 1))%Q
    | 2%positive => ((82 # 1))%Q
    | 3%positive => ((82 # 1))%Q
    | 4%positive => ((82 # 1))%Q
    | 5%positive => ((82 # 1))%Q
    | 6%positive => ((82 # 1))%Q
    | 7%positive => ((82 # 1))%Q
    | 8%positive => ((82 # 1))%Q
    | 9%positive => ((82 # 1))%Q
    | 10%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 11%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 12%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 13%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 14%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 15%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 16%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 17%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 18%positive => ((s IDReflection_coefficients_z))%Q
    | 19%positive => ((s IDReflection_coefficients_z))%Q
    | 20%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 21%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 22%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 23%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i))
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 24%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i))
                      + max0(-(s IDReflection_coefficients_z)))%Q
    | 25%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 26%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 27%positive => ((73 # 1) + (s IDReflection_coefficients_z))%Q
    | 28%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                      + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 29%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                      + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 30%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                      + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 31%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                      + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 32%positive => ((66 # 1) + (s IDReflection_coefficients_z))%Q
    | 33%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 34%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 35%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 36%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 37%positive => ((57 # 31) * (s IDReflection_coefficients_temp)
                      + (s IDReflection_coefficients_z)
                      + max0(9 - (s IDReflection_coefficients_i))
                      + (57 # 31) * max0(31
                                         - (s IDReflection_coefficients_temp)))%Q
    | 38%positive => (-(393 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + (57 # 31) * (s IDReflection_coefficients_temp)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_i))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + (57 # 31) * max0(31
                                         - (s IDReflection_coefficients_temp)))%Q
    | 39%positive => (-(393 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + (57 # 31) * (s IDReflection_coefficients_temp)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_i))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + (57 # 31) * max0(31
                                         - (s IDReflection_coefficients_temp)))%Q
    | 40%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0((s IDReflection_coefficients_z)))%Q
    | 41%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0((s IDReflection_coefficients_z)))%Q
    | 42%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0((s IDReflection_coefficients_z)))%Q
    | 43%positive => (max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0((s IDReflection_coefficients_z)))%Q
    | 44%positive => (max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0((s IDReflection_coefficients_z)))%Q
    | 45%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 46%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 47%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 48%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 49%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 50%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 51%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 52%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 53%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 54%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 55%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 56%positive => ((s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      - (6 # 7) * max0(-1 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 57%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 58%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 59%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 60%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 61%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 62%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 63%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 64%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 65%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 66%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 67%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 68%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 69%positive => (-(7 # 1) + (1 # 8) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (9 # 8) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 70%positive => (-(7 # 1) + (1 # 8) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (9 # 8) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 71%positive => (-(7 # 1) + (1 # 8) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (9 # 8) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 72%positive => (-(7 # 1) + (1 # 8) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (9 # 8) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 73%positive => (-(7 # 1) + (1 # 8) * (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (9 # 8) * max0(8 - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 74%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 75%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 76%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 77%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 78%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 79%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 80%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 81%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 82%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 83%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 84%positive => ((3 # 1) - (s IDReflection_coefficients_m)
                      - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 85%positive => ((3 # 1) - (s IDReflection_coefficients_m)
                      - (s IDReflection_coefficients_n)
                      + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 86%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 87%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 88%positive => ((1 # 1) + (s IDReflection_coefficients_z)
                      + (7 # 1) * max0(8 - (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 89%positive => ((1 # 1) + (s IDReflection_coefficients_z)
                      + (7 # 1) * max0(8 - (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 90%positive => ((1 # 1) + (s IDReflection_coefficients_z)
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0(10 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 91%positive => ((1 # 1) + (s IDReflection_coefficients_z)
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0(10 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 92%positive => ((1 # 1) + (s IDReflection_coefficients_z)
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0(10 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 93%positive => ((s IDReflection_coefficients_z)
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n))
                      + max0(10 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n)))%Q
    | 94%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 95%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 96%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 97%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 98%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 99%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                      + max0(-2 + (s IDReflection_coefficients_n))
                      + max0(9 - (s IDReflection_coefficients_m)
                             - (s IDReflection_coefficients_n))
                      + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 100%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 101%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 102%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 103%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 104%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(8 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 105%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(8 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 106%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(8 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 107%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(8 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 108%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(8 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 109%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 110%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 111%positive => (-(5 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 112%positive => (-(6 # 1) + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + max0(9 - (s IDReflection_coefficients_m)
                              - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 113%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 114%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 115%positive => ((2 # 1) - (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 116%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 117%positive => ((6 # 7) - (6 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_n))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_n)))%Q
    | 118%positive => ((6 # 7) - (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 119%positive => ((6 # 7) - (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 120%positive => ((6 # 7) - (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 121%positive => ((6 # 7) - (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 122%positive => ((2 # 1) - (s IDReflection_coefficients_i)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 123%positive => ((2 # 1) - (s IDReflection_coefficients_i)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 124%positive => ((6 # 7) - (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 125%positive => ((62 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-1 + (s IDReflection_coefficients_i))
                       + (7 # 1) * max0(8 - (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n)))%Q
    | 126%positive => ((62 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-1 + (s IDReflection_coefficients_i))
                       + (7 # 1) * max0(8 - (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n)))%Q
    | 127%positive => ((76 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 128%positive => ((76 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 129%positive => ((76 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 130%positive => ((69 # 7) - (2 # 1) * (s IDReflection_coefficients_i)
                       + (1 # 7) * (s IDReflection_coefficients_n)
                       + (s IDReflection_coefficients_z)
                       + max0(-2 + (s IDReflection_coefficients_i))
                       + (1 # 7) * max0(8 - (s IDReflection_coefficients_n))
                       + (7 # 1) * max0(9 - (s IDReflection_coefficients_i)))%Q
    | 131%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 132%positive => ((58 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 133%positive => ((58 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 134%positive => ((58 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 135%positive => ((58 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 136%positive => ((58 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 137%positive => ((57 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 138%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 139%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 140%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 141%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 142%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 143%positive => ((66 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 144%positive => ((65 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 145%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 146%positive => ((74 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 147%positive => ((74 # 1) + (s IDReflection_coefficients_z)
                       + max0(8 - (s IDReflection_coefficients_i)))%Q
    | 148%positive => ((74 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 149%positive => ((74 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 150%positive => ((74 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 151%positive => ((73 # 1) + (s IDReflection_coefficients_z)
                       + max0(9 - (s IDReflection_coefficients_i)))%Q
    | 152%positive => ((82 # 1))%Q
    | 153%positive => ((82 # 1) + (s IDReflection_coefficients_z))%Q
    | 154%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | 155%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | 156%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 157%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 158%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 159%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 160%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 161%positive => ((s IDReflection_coefficients_z))%Q
    | 162%positive => ((82 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(7 - (s IDReflection_coefficients_i)))%Q
    | 163%positive => ((83 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | 164%positive => ((83 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | 165%positive => ((83 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | 166%positive => ((83 # 1) + (s IDReflection_coefficients_z)
                       - (82 # 7) * max0(8 - (s IDReflection_coefficients_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Reflection_coefficients_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-82 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDReflection_coefficients_z))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDReflection_coefficients_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDReflection_coefficients_z)))]
    | 9%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDReflection_coefficients_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDReflection_coefficients_z)))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*0 82*) F_one;
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDReflection_coefficients_z))) (F_check_ge (0) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-82 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDReflection_coefficients_z))) (F_check_ge (0) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDReflection_coefficients_z))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDReflection_coefficients_i)) (7
                                                                    - (s IDReflection_coefficients_i)));
                      (*-1 0*) F_max0_ge_0 (7
                                            - (s IDReflection_coefficients_i));
                      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                                             - (s IDReflection_coefficients_i)) (8
                                                                    - (s IDReflection_coefficients_i)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDReflection_coefficients_i)) (7
                                                                    - (s IDReflection_coefficients_i)));
                      (*-1 0*) F_max0_ge_0 (7
                                            - (s IDReflection_coefficients_i))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*0 1.83871*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (31
                                                                    - (s IDReflection_coefficients_temp)) (0))) (F_max0_ge_0 (31
                                                                    - (s IDReflection_coefficients_temp)))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDReflection_coefficients_i)) (7
                                                                    - (s IDReflection_coefficients_i)));
                      (*-1 0*) F_max0_ge_0 (7
                                            - (s IDReflection_coefficients_i));
                      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                                             - (s IDReflection_coefficients_i)) (8
                                                                    - (s IDReflection_coefficients_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDReflection_coefficients_z)) (0))) (F_max0_ge_0 ((s IDReflection_coefficients_z)));
                      (*-1.83871 0*) F_binom_monotonic 1 (F_max0_ge_arg (31
                                                                    - (s IDReflection_coefficients_temp))) (F_check_ge (31
                                                                    - (s IDReflection_coefficients_temp)) (0))]
    | 40%positive => []
    | 41%positive => [(*-7.14286 0*) F_max0_monotonic (F_check_ge (8
                                                                   - 
                                                                   (s IDReflection_coefficients_n)) (7
                                                                    - (s IDReflection_coefficients_n)));
                      (*-7.14286 0*) F_max0_ge_0 (7
                                                  - (s IDReflection_coefficients_n));
                      (*-7 0*) F_max0_monotonic (F_check_ge (9
                                                             - (s IDReflection_coefficients_n)) (8
                                                                    - (s IDReflection_coefficients_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDReflection_coefficients_z))) (F_check_ge ((s IDReflection_coefficients_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                   + 
                                                                   (s IDReflection_coefficients_n))) (F_check_ge (-2
                                                                    + (s IDReflection_coefficients_n)) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (-8
                                                                    + (s IDReflection_coefficients_n))) (F_check_ge (0) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-8
                                                                    + (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (-8
                                                                    + (s IDReflection_coefficients_n)))]
    | 42%positive => [(*0 0.857143*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDReflection_coefficients_n))) (F_check_ge (-1
                                                                    + (s IDReflection_coefficients_n)) (0))]
    | 43%positive => []
    | 44%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDReflection_coefficients_z))) (F_check_ge ((s IDReflection_coefficients_z)) (0))]
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
    | 56%positive => [(*-0.857143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDReflection_coefficients_n)))]
    | 57%positive => []
    | 58%positive => [(*0 0.857143*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDReflection_coefficients_n)))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-6 0*) F_max0_pre_decrement (9
                                                     - (s IDReflection_coefficients_n)) (1);
                      (*-8 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDReflection_coefficients_n)) (7
                                                                    - (s IDReflection_coefficients_n)));
                      (*-8 0*) F_max0_ge_0 (7
                                            - (s IDReflection_coefficients_n));
                      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                                             - (s IDReflection_coefficients_n)) (8
                                                                    - (s IDReflection_coefficients_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                 + (s IDReflection_coefficients_n))) (F_check_ge (0) (0))]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDReflection_coefficients_n)))]
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDReflection_coefficients_n)))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*-7 0*) F_max0_pre_decrement (9
                                                     - (s IDReflection_coefficients_n)) (1);
                      (*-8.125 0*) F_max0_monotonic (F_check_ge (8
                                                                 - (s IDReflection_coefficients_n)) (7
                                                                    - (s IDReflection_coefficients_n)));
                      (*-8.125 0*) F_max0_ge_0 (7
                                                - (s IDReflection_coefficients_n));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDReflection_coefficients_n))) (F_check_ge (0) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 ((s IDReflection_coefficients_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                 + (s IDReflection_coefficients_n))) (F_check_ge (0) (0))]
    | 72%positive => []
    | 73%positive => [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDReflection_coefficients_n))) (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDReflection_coefficients_m)
                                                                    - (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDReflection_coefficients_m)
                                                                    - (s IDReflection_coefficients_n)))]
    | 86%positive => []
    | 87%positive => [(*-7 0*) F_max0_pre_decrement (9
                                                     - (s IDReflection_coefficients_n)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                 + (s IDReflection_coefficients_n))) (F_check_ge (0) (0))]
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDReflection_coefficients_z)) (0))) (F_max0_ge_0 ((s IDReflection_coefficients_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (10
                                                                 - (s IDReflection_coefficients_m)
                                                                 - (s IDReflection_coefficients_n))) (F_check_ge (0) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDReflection_coefficients_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDReflection_coefficients_n)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDReflection_coefficients_n)))]
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                      - (s IDReflection_coefficients_m)
                                                      - (s IDReflection_coefficients_n)) (1)]
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => [(*-7 0*) F_max0_monotonic (F_check_ge (8
                                                              - (s IDReflection_coefficients_n)) (7
                                                                    - (s IDReflection_coefficients_n)));
                       (*-7 0*) F_max0_ge_0 (7
                                             - (s IDReflection_coefficients_n));
                       (*-7 0*) F_max0_monotonic (F_check_ge (9
                                                              - (s IDReflection_coefficients_n)) (8
                                                                    - (s IDReflection_coefficients_n)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + 
                                                                    (s IDReflection_coefficients_n))) (F_check_ge (-2
                                                                    + (s IDReflection_coefficients_n)) (0))]
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDReflection_coefficients_n))) (F_check_ge (8
                                                                    - (s IDReflection_coefficients_n)) (0))]
    | 122%positive => []
    | 123%positive => [(*-7 0*) F_max0_monotonic (F_check_ge (8
                                                              - (s IDReflection_coefficients_i)) (7
                                                                    - (s IDReflection_coefficients_i)));
                       (*-7 0*) F_max0_ge_0 (7
                                             - (s IDReflection_coefficients_i));
                       (*-7 0*) F_max0_monotonic (F_check_ge (9
                                                              - (s IDReflection_coefficients_i)) (8
                                                                    - (s IDReflection_coefficients_i)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + 
                                                                    (s IDReflection_coefficients_i))) (F_check_ge (-2
                                                                    + (s IDReflection_coefficients_i)) (0))]
    | 124%positive => [(*-7 0*) F_max0_pre_decrement (9
                                                      - (s IDReflection_coefficients_i)) (1);
                       (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDReflection_coefficients_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDReflection_coefficients_i)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                  + (s IDReflection_coefficients_i))) (F_check_ge (0) (0))]
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                                  - (s IDReflection_coefficients_i))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDReflection_coefficients_i)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDReflection_coefficients_i)))]
    | 131%positive => [(*0 1*) F_max0_pre_decrement (9
                                                     - (s IDReflection_coefficients_i)) (1)]
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | 138%positive => []
    | 139%positive => []
    | 140%positive => []
    | 141%positive => []
    | 142%positive => []
    | 143%positive => []
    | 144%positive => [(*-1 0*) F_max0_pre_decrement (9
                                                      - (s IDReflection_coefficients_i)) (1)]
    | 145%positive => [(*0 1*) F_max0_pre_decrement (9
                                                     - (s IDReflection_coefficients_i)) (1)]
    | 146%positive => []
    | 147%positive => []
    | 148%positive => []
    | 149%positive => []
    | 150%positive => []
    | 151%positive => []
    | 152%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDReflection_coefficients_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDReflection_coefficients_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDReflection_coefficients_z)))]
    | 153%positive => []
    | 154%positive => []
    | 155%positive => []
    | 156%positive => []
    | 157%positive => []
    | 158%positive => []
    | 159%positive => []
    | 160%positive => [(*-11.7143 0*) F_max0_pre_decrement (8
                                                            - (s IDReflection_coefficients_i)) (1);
                       (*-11.7143 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDReflection_coefficients_i))) (F_check_ge (0) (0));
                       (*-11.7143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDReflection_coefficients_i)) (0))) (F_max0_ge_0 ((s IDReflection_coefficients_i)));
                       (*-11.7143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDReflection_coefficients_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDReflection_coefficients_i)))]
    | 161%positive => []
    | 162%positive => [(*0 10.7143*) F_one;
                       (*-11.7143 0*) F_max0_pre_decrement (8
                                                            - (s IDReflection_coefficients_i)) (1)]
    | 163%positive => []
    | 164%positive => []
    | 165%positive => []
    | 166%positive => []
    | _ => []
  end.


Theorem Reflection_coefficients_ai_correct:
  forall s p' s', steps (g_start Reflection_coefficients) s (g_edges Reflection_coefficients) p' s' -> Reflection_coefficients_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Reflection_coefficients_pot_correct:
  forall s p' s',
    steps (g_start Reflection_coefficients) s (g_edges Reflection_coefficients) p' s' ->
    (Reflection_coefficients_pot (g_start Reflection_coefficients) s >= Reflection_coefficients_pot p' s')%Q.
Proof.
  check_lp Reflection_coefficients_ai_correct Reflection_coefficients_hints.
Qed.

