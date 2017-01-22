Require Import pasta.Pasta.

Notation IDCalculation_of_the_LTP_parameters_z := 1%positive.
Notation IDCalculation_of_the_LTP_parameters_L_max := 2%positive.
Notation IDCalculation_of_the_LTP_parameters_L_power := 3%positive.
Notation IDCalculation_of_the_LTP_parameters_L_result := 4%positive.
Notation IDCalculation_of_the_LTP_parameters_L_temp := 5%positive.
Notation IDCalculation_of_the_LTP_parameters_Nc := 6%positive.
Notation IDCalculation_of_the_LTP_parameters_Nc_out_dref := 7%positive.
Notation IDCalculation_of_the_LTP_parameters_R := 8%positive.
Notation IDCalculation_of_the_LTP_parameters_S := 9%positive.
Notation IDCalculation_of_the_LTP_parameters_bc := 10%positive.
Notation IDCalculation_of_the_LTP_parameters_bc_out_dref := 11%positive.
Notation IDCalculation_of_the_LTP_parameters_dmax := 12%positive.
Notation IDCalculation_of_the_LTP_parameters_k := 13%positive.
Notation IDCalculation_of_the_LTP_parameters_lambda := 14%positive.
Notation IDCalculation_of_the_LTP_parameters_scal := 15%positive.
Notation IDCalculation_of_the_LTP_parameters_temp := 16%positive.
Notation IDCalculation_of_the_LTP_parameters_Nc_out := 17%positive.
Notation IDCalculation_of_the_LTP_parameters_bc_out := 18%positive.
Notation IDCalculation_of_the_LTP_parameters_d := 19%positive.
Notation IDCalculation_of_the_LTP_parameters_dp := 20%positive.
Definition Calculation_of_the_LTP_parameters : graph := {|
  g_start := 1%positive;
  g_end := 115%positive;
  g_edges := (1%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDCalculation_of_the_LTP_parameters_dmax
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (ENum (0)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) <= (eval (ENum (39)) s))%Z)),186%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) > (eval (ENum (39)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDCalculation_of_the_LTP_parameters_temp
             (Some (ENum (0)))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s) = (eval (ENum (0)) s))%Z)),21%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s) <> (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s) > (eval (ENum (0)) s))%Z)),16%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s) <= (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,115%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDCalculation_of_the_LTP_parameters_temp
             None),19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,25%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDCalculation_of_the_LTP_parameters_scal
             (Some (ENum (0)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) > (eval (ENum (6)) s))%Z)),30%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) <= (eval (ENum (6)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDCalculation_of_the_LTP_parameters_scal
             (Some (ESub (ENum (6))
             (EVar IDCalculation_of_the_LTP_parameters_temp)))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,34%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AAssign IDCalculation_of_the_LTP_parameters_scal
             (Some (ENum (0)))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) >= (eval (ENum (0)) s))%Z)),38%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) < (eval (ENum (0)) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,115%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (ENum (0)))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) <= (eval (ENum (39)) s))%Z)),179%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) > (eval (ENum (39)) s))%Z)),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDCalculation_of_the_LTP_parameters_L_max
             (Some (ENum (0)))),46%positive)::
             (46%positive,(AAssign IDCalculation_of_the_LTP_parameters_Nc
             (Some (ENum (40)))),47%positive)::
             (47%positive,(AAssign IDCalculation_of_the_LTP_parameters_lambda
             (Some (ENum (40)))),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_lambda)
             s) <= (eval (ENum (120)) s))%Z)),125%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_lambda)
             s) > (eval (ENum (120)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_Nc_out_dref
             (Some (EVar IDCalculation_of_the_LTP_parameters_Nc))),
             53%positive)::
             (53%positive,(AAssign IDCalculation_of_the_LTP_parameters_L_max
             None),54%positive)::(54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) <= (eval (ENum (100)) s))%Z)),57%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) > (eval (ENum (100)) s))%Z)),56%positive)::
             (56%positive,AWeaken,60%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) >= (eval (ENum (-100)) s))%Z)),62%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_scal)
             s) < (eval (ENum (-100)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,115%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDCalculation_of_the_LTP_parameters_L_max
             None),65%positive)::(65%positive,AWeaken,66%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_Nc)
             s) <= (eval (ENum (120)) s))%Z)),68%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_Nc)
             s) > (eval (ENum (120)) s))%Z)),67%positive)::
             (67%positive,AWeaken,71%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_Nc)
             s) >= (eval (ENum (40)) s))%Z)),73%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_Nc)
             s) < (eval (ENum (40)) s))%Z)),70%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,115%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_power (Some (ENum (0)))),
             76%positive)::
             (76%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (ENum (0)))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) <= (eval (ENum (39)) s))%Z)),116%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_k)
             s) > (eval (ENum (39)) s))%Z)),80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_power None),82%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s) <= (eval (ENum (0)) s))%Z)),111%positive)::
             (83%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s) > (eval (ENum (0)) s))%Z)),84%positive)::
             (84%positive,AWeaken,85%positive)::
             (85%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s) >= (eval (EVar IDCalculation_of_the_LTP_parameters_L_power)
             s))%Z)),107%positive)::
             (85%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s) < (eval (EVar IDCalculation_of_the_LTP_parameters_L_power)
             s))%Z)),86%positive)::(86%positive,AWeaken,87%positive)::
             (87%positive,(AAssign IDCalculation_of_the_LTP_parameters_temp
             None),88%positive)::
             (88%positive,(AAssign IDCalculation_of_the_LTP_parameters_R
             None),89%positive)::
             (89%positive,(AAssign IDCalculation_of_the_LTP_parameters_S
             None),90%positive)::
             (90%positive,(AAssign IDCalculation_of_the_LTP_parameters_bc
             (Some (ENum (0)))),91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,AWeaken,93%positive)::
             (93%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_bc)
             s) <= (eval (ENum (2)) s))%Z)),95%positive)::
             (93%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_bc)
             s) > (eval (ENum (2)) s))%Z)),94%positive)::
             (94%positive,AWeaken,104%positive)::
             (95%positive,AWeaken,96%positive)::
             (96%positive,ANone,103%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,ANone,98%positive)::
             (98%positive,(AAssign IDCalculation_of_the_LTP_parameters_bc
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_bc)
             (ENum (1))))),99%positive)::(99%positive,ANone,100%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (EAdd (ENum (1))
             (EVar IDCalculation_of_the_LTP_parameters_z)))),102%positive)::
             (102%positive,AWeaken,93%positive)::
             (103%positive,ANone,104%positive)::
             (104%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_bc_out_dref
             (Some (EVar IDCalculation_of_the_LTP_parameters_bc))),
             105%positive)::(105%positive,ANone,106%positive)::
             (106%positive,AWeaken,115%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_bc_out_dref
             (Some (ENum (3)))),109%positive)::
             (109%positive,ANone,110%positive)::
             (110%positive,AWeaken,115%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_bc_out_dref
             (Some (ENum (0)))),113%positive)::
             (113%positive,ANone,114%positive)::
             (114%positive,AWeaken,115%positive)::
             (116%positive,AWeaken,117%positive)::
             (117%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_temp None),118%positive)::
             (118%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_power
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_L_power)
             (EMul (EVar IDCalculation_of_the_LTP_parameters_L_temp)
             (EVar IDCalculation_of_the_LTP_parameters_L_temp))))),
             119%positive)::(119%positive,ANone,120%positive)::
             (120%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_k)
             (ENum (1))))),121%positive)::(121%positive,ANone,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (EAdd (ENum (1))
             (EVar IDCalculation_of_the_LTP_parameters_z)))),124%positive)::
             (124%positive,AWeaken,79%positive)::
             (125%positive,AWeaken,126%positive)::
             (126%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),127%positive)::
             (127%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),128%positive)::
             (128%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),129%positive)::
             (129%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),130%positive)::
             (130%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),131%positive)::
             (131%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),132%positive)::
             (132%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),133%positive)::
             (133%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),134%positive)::
             (134%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),135%positive)::
             (135%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),136%positive)::
             (136%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),137%positive)::
             (137%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),138%positive)::
             (138%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),139%positive)::
             (139%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),140%positive)::
             (140%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),141%positive)::
             (141%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),142%positive)::
             (142%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),143%positive)::
             (143%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),144%positive)::
             (144%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),145%positive)::
             (145%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),146%positive)::
             (146%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),147%positive)::
             (147%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),148%positive)::
             (148%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),149%positive)::
             (149%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),150%positive)::
             (150%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),151%positive)::
             (151%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),152%positive)::
             (152%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),153%positive)::
             (153%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),154%positive)::
             (154%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),155%positive)::
             (155%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),156%positive)::
             (156%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),157%positive)::
             (157%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),158%positive)::
             (158%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),159%positive)::
             (159%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),160%positive)::
             (160%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),161%positive)::
             (161%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),162%positive)::
             (162%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),163%positive)::
             (163%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),164%positive)::
             (164%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),165%positive)::
             (165%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_L_result None),166%positive)::
             (166%positive,AWeaken,167%positive)::
             (167%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_result)
             s) > (eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s))%Z)),169%positive)::
             (167%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_L_result)
             s) <= (eval (EVar IDCalculation_of_the_LTP_parameters_L_max)
             s))%Z)),168%positive)::(168%positive,AWeaken,173%positive)::
             (169%positive,AWeaken,170%positive)::
             (170%positive,(AAssign IDCalculation_of_the_LTP_parameters_Nc
             (Some (EVar IDCalculation_of_the_LTP_parameters_lambda))),
             171%positive)::
             (171%positive,(AAssign IDCalculation_of_the_LTP_parameters_L_max
             (Some (EVar IDCalculation_of_the_LTP_parameters_L_result))),
             172%positive)::(172%positive,ANone,173%positive)::
             (173%positive,ANone,174%positive)::
             (174%positive,(AAssign
             IDCalculation_of_the_LTP_parameters_lambda
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_lambda)
             (ENum (1))))),175%positive)::(175%positive,ANone,176%positive)::
             (176%positive,ANone,177%positive)::
             (177%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (EAdd (ENum (1))
             (EVar IDCalculation_of_the_LTP_parameters_z)))),178%positive)::
             (178%positive,AWeaken,50%positive)::
             (179%positive,AWeaken,180%positive)::
             (180%positive,ANone,181%positive)::
             (181%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_k)
             (ENum (1))))),182%positive)::(182%positive,ANone,183%positive)::
             (183%positive,ANone,184%positive)::
             (184%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (EAdd (ENum (1))
             (EVar IDCalculation_of_the_LTP_parameters_z)))),185%positive)::
             (185%positive,AWeaken,43%positive)::
             (186%positive,AWeaken,187%positive)::
             (187%positive,(AAssign IDCalculation_of_the_LTP_parameters_temp
             None),188%positive)::(188%positive,AWeaken,189%positive)::
             (189%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) < (eval (ENum (0)) s))%Z)),192%positive)::
             (189%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) >= (eval (ENum (0)) s))%Z)),190%positive)::
             (190%positive,AWeaken,191%positive)::
             (191%positive,ANone,199%positive)::
             (192%positive,AWeaken,193%positive)::
             (193%positive,(AGuard (fun s => True)),197%positive)::
             (193%positive,ANone,194%positive)::
             (194%positive,ANone,195%positive)::
             (195%positive,(AGuard (fun s => True)),196%positive)::
             (196%positive,AWeaken,199%positive)::
             (197%positive,AWeaken,198%positive)::
             (198%positive,ANone,199%positive)::
             (199%positive,(AAssign IDCalculation_of_the_LTP_parameters_temp
             None),200%positive)::(200%positive,AWeaken,201%positive)::
             (201%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) > (eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s))%Z)),203%positive)::
             (201%positive,(AGuard
             (fun s => ((eval (EVar IDCalculation_of_the_LTP_parameters_temp)
             s) <= (eval (EVar IDCalculation_of_the_LTP_parameters_dmax)
             s))%Z)),202%positive)::(202%positive,AWeaken,206%positive)::
             (203%positive,AWeaken,204%positive)::
             (204%positive,(AAssign IDCalculation_of_the_LTP_parameters_dmax
             (Some (EVar IDCalculation_of_the_LTP_parameters_temp))),
             205%positive)::(205%positive,ANone,206%positive)::
             (206%positive,ANone,207%positive)::
             (207%positive,(AAssign IDCalculation_of_the_LTP_parameters_k
             (Some (EAdd (EVar IDCalculation_of_the_LTP_parameters_k)
             (ENum (1))))),208%positive)::(208%positive,ANone,209%positive)::
             (209%positive,ANone,210%positive)::
             (210%positive,(AAssign IDCalculation_of_the_LTP_parameters_z
             (Some (EAdd (ENum (1))
             (EVar IDCalculation_of_the_LTP_parameters_z)))),211%positive)::
             (211%positive,AWeaken,6%positive)::nil
|}.

Definition Calculation_of_the_LTP_parameters_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0)%Z
    | 3%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 4%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 5%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 6%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 7%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0)%Z
    | 8%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 9%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0)%Z
    | 10%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 11%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0)%Z
    | 12%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 13%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 14%positive => (1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 15%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 16%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 18%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 20%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 22%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 23%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 24%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 25%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 26%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + -6 <= 0)%Z
    | 27%positive => (1 * (s IDCalculation_of_the_LTP_parameters_temp) + -6 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 28%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + -6 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 29%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + -6 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 30%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) + 7 <= 0)%Z
    | 31%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_temp) + 7 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 32%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) + 7 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 33%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) + 7 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 34%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 35%positive => (False)%Z
    | 36%positive => (False)%Z
    | 37%positive => (False)%Z
    | 38%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 39%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 40%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 41%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 42%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0)%Z
    | 43%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 44%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0)%Z
    | 45%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 46%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0)%Z
    | 47%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 48%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0)%Z
    | 49%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 50%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0)%Z
    | 51%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0)%Z
    | 52%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0)%Z
    | 53%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 54%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0)%Z
    | 55%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 56%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) + 101 <= 0)%Z
    | 57%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 58%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 59%positive => (False)%Z
    | 60%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) + 101 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 61%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) + 101 <= 0)%Z
    | 62%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 63%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 64%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 65%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0)%Z
    | 66%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 67%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 121 <= 0)%Z
    | 68%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0)%Z
    | 69%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 70%positive => (False)%Z
    | 71%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 72%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 121 <= 0)%Z
    | 73%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0)%Z
    | 74%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0)%Z
    | 75%positive => (1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0)%Z
    | 76%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0)%Z
    | 77%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 78%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0)%Z
    | 79%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 80%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0)%Z
    | 81%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 82%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0)%Z
    | 83%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 84%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0)%Z
    | 85%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 86%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 87%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 88%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 89%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 90%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 91%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0)%Z
    | 92%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 93%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0)%Z
    | 94%positive => (1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) + 3 <= 0)%Z
    | 95%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -2 <= 0)%Z
    | 96%positive => (1 * (s IDCalculation_of_the_LTP_parameters_bc) + -2 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 97%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -2 <= 0)%Z
    | 98%positive => (1 * (s IDCalculation_of_the_LTP_parameters_bc) + -2 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 99%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) + 1 <= 0)%Z
    | 100%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 101%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) + 1 <= 0)%Z
    | 102%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) + 1 <= 0)%Z
    | 103%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -2 <= 0)%Z
    | 104%positive => (1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 105%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0)%Z
    | 106%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + -3 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_power) + 1 <= 0)%Z
    | 107%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0)%Z
    | 108%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 109%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + 3 <= 0)%Z
    | 110%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + 3 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) + -3 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ 1 * (s IDCalculation_of_the_LTP_parameters_L_power) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 111%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0)%Z
    | 112%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 113%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0)%Z
    | 114%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_bc_out_dref) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 115%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0)%Z
    | 116%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 117%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0)%Z
    | 118%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 119%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0)%Z
    | 120%positive => (1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 121%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 122%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0)%Z
    | 123%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 124%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc_out_dref) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_scal) + -100 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) + 1 <= 0)%Z
    | 125%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 126%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 127%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 128%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 129%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 130%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 131%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 132%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 133%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 134%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 135%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 136%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 137%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 138%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 139%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 140%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 141%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 142%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 143%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 144%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 145%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 146%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 147%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 148%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 149%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 150%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 151%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 152%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 153%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 154%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 155%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 156%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 157%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 158%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 159%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 160%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 161%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 162%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 163%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 164%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 165%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 166%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 167%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 168%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ 1 * (s IDCalculation_of_the_LTP_parameters_L_result) <= 0)%Z
    | 169%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_result) + 1 <= 0)%Z
    | 170%positive => (1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_result) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 171%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_L_max)+ -1 * (s IDCalculation_of_the_LTP_parameters_L_result) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 172%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_Nc) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 173%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0)%Z
    | 174%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -120 <= 0)%Z
    | 175%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 41 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0)%Z
    | 176%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 41 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0)%Z
    | 177%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 41 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0)%Z
    | 178%positive => (1 * (s IDCalculation_of_the_LTP_parameters_lambda) + -121 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_lambda) + 41 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_Nc) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) + 1 <= 0)%Z
    | 179%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 180%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0)%Z
    | 181%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 182%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 183%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0)%Z
    | 184%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0)%Z
    | 185%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_scal) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) + 1 <= 0)%Z
    | 186%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 187%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 188%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 189%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 190%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0)%Z
    | 191%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 192%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0)%Z
    | 193%positive => (1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 194%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0)%Z
    | 195%positive => (1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 196%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0)%Z
    | 197%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0)%Z
    | 198%positive => (1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 199%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 200%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 201%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 202%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_dmax)+ 1 * (s IDCalculation_of_the_LTP_parameters_temp) <= 0)%Z
    | 203%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_dmax)+ -1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0)%Z
    | 204%positive => (1 * (s IDCalculation_of_the_LTP_parameters_dmax)+ -1 * (s IDCalculation_of_the_LTP_parameters_temp) + 1 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 205%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 206%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0)%Z
    | 207%positive => (1 * (s IDCalculation_of_the_LTP_parameters_k) + -39 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) <= 0)%Z
    | 208%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0)%Z
    | 209%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0)%Z
    | 210%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_z) <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0)%Z
    | 211%positive => (-1 * (s IDCalculation_of_the_LTP_parameters_k) + 1 <= 0 /\ 1 * (s IDCalculation_of_the_LTP_parameters_k) + -40 <= 0 /\ -1 * (s IDCalculation_of_the_LTP_parameters_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Calculation_of_the_LTP_parameters_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((204 # 1))%Q
    | 2%positive => ((204 # 1)
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 3%positive => ((204 # 1)
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 4%positive => ((51 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 5%positive => ((51 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 6%positive => ((51 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 7%positive => ((51 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 8%positive => ((41 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 9%positive => ((41 # 10) * max0(40
                                      - (s IDCalculation_of_the_LTP_parameters_k))
                     + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                     + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 10%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 11%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 12%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 13%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 14%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 15%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 16%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 17%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 18%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 19%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 20%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 21%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 22%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 23%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 24%positive => ((s IDCalculation_of_the_LTP_parameters_z)
                      + (41 # 10) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 25%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 26%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 27%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 28%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 29%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 30%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 31%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 32%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 33%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 34%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 35%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 36%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 37%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 38%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 39%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 40%positive => ((164 # 1) + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 41%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 42%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 43%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 44%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 45%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 46%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 47%positive => ((81 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 48%positive => ((121 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      - (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 49%positive => ((121 # 1)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      - (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (83 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 50%positive => ((14468 # 107)
                      - (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 51%positive => ((14468 # 107)
                      - (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 52%positive => ((121 # 1)
                      - (78 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      - max0(121
                             - (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 53%positive => ((121 # 1)
                      - (78 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      - max0(121
                             - (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 54%positive => ((121 # 1)
                      - (78 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      - max0(121
                             - (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 55%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 56%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 57%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 58%positive => ((77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 59%positive => ((77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 60%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      - (43 # 39) * max0(-1
                                         + (s IDCalculation_of_the_LTP_parameters_k))
                      - (18 # 47) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 61%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      - (43 # 39) * max0(-1
                                         + (s IDCalculation_of_the_LTP_parameters_k))
                      - (18 # 47) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k))
                      + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 62%positive => ((77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (77 # 107) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 63%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 64%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 65%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 66%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 67%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 68%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + (43 # 40) * max0(40
                                         - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 69%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 70%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 71%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda)))%Q
    | 72%positive => (-(1521 # 107)
                      + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      - (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_lambda)))%Q
    | 73%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (43 # 121) * max0(-40
                                          + (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 74%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 75%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 76%positive => ((43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 77%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 78%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 79%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 80%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 81%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 82%positive => (-(40 # 1)
                      + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 83%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 84%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 85%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 86%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 87%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 88%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 89%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 90%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 91%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 92%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (43 # 121) * max0(-121
                                          + (s IDCalculation_of_the_LTP_parameters_lambda))
                      + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal))
                      + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 93%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 94%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 95%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 96%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 97%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 98%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 99%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_bc)
                      + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                      + (s IDCalculation_of_the_LTP_parameters_z)
                      + (3 # 100) * max0(100
                                         - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 100%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_bc)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 101%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_bc)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 102%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 103%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 104%positive => (-(s IDCalculation_of_the_LTP_parameters_bc)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 105%positive => (-(s IDCalculation_of_the_LTP_parameters_bc_out_dref)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 106%positive => (-(s IDCalculation_of_the_LTP_parameters_bc_out_dref)
                       + (3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal)))%Q
    | 107%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 108%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 109%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 110%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 111%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 112%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 113%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 114%positive => ((3 # 100) * (s IDCalculation_of_the_LTP_parameters_scal)
                       + (43 # 121) * max0(-121
                                           + (s IDCalculation_of_the_LTP_parameters_lambda))
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + (3 # 100) * max0(100
                                          - (s IDCalculation_of_the_LTP_parameters_scal))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 115%positive => ((s IDCalculation_of_the_LTP_parameters_z))%Q
    | 116%positive => (-(40 # 1)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0(40 - (s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 117%positive => (-(s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 118%positive => (-(s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 119%positive => (-(s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 120%positive => (-(s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 121%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 122%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 123%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 124%positive => ((1 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (43 # 121) * (s IDCalculation_of_the_LTP_parameters_lambda)
                       + max0(-1 + (s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 125%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 126%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 127%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 128%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 129%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 130%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 131%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 132%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 133%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 134%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 135%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 136%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 137%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 138%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 139%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 140%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 141%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 142%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 143%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 144%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 145%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 146%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 147%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 148%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 149%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 150%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 151%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 152%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 153%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 154%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 155%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 156%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 157%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 158%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 159%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 160%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 161%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 162%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 163%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 164%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 165%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 166%positive => ((14468 # 107)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 167%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 168%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 169%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 170%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 171%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 172%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 173%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 174%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 175%positive => ((14575 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 176%positive => ((14575 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 177%positive => ((14575 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 178%positive => ((14468 # 107)
                       + (77 # 107) * (s IDCalculation_of_the_LTP_parameters_k)
                       - (s IDCalculation_of_the_LTP_parameters_lambda)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (43 # 121) * max0(-40
                                           + (s IDCalculation_of_the_LTP_parameters_k))
                       + (77 # 107) * max0(40
                                           - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 179%positive => ((81 # 1)
                       + (43 # 40) * (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z)
                       + (83 # 40) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 180%positive => ((164 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 181%positive => ((164 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 182%positive => ((165 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 183%positive => ((165 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 184%positive => ((165 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 185%positive => ((164 # 1) - (s IDCalculation_of_the_LTP_parameters_k)
                       + (s IDCalculation_of_the_LTP_parameters_z))%Q
    | 186%positive => ((51 # 10) * max0(40
                                        - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 187%positive => ((51 # 10) * max0(40
                                        - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 188%positive => ((51 # 10) * max0(40
                                        - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 189%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 190%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 191%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 192%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 193%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 194%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 195%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 196%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 197%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 198%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 199%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 200%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 201%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 202%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 203%positive => ((51 # 10)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k))
                       + max0((s IDCalculation_of_the_LTP_parameters_z)))%Q
    | 204%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 205%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 206%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 207%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (51 # 10) * max0(39
                                          - (s IDCalculation_of_the_LTP_parameters_k))
                       + (41 # 10) * max0((s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 208%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (41 # 10) * max0(-1
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                       + (51 # 10) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 209%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (41 # 10) * max0(-1
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                       + (51 # 10) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 210%positive => ((51 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (41 # 10) * max0(-1
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                       + (51 # 10) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | 211%positive => ((41 # 10) + (s IDCalculation_of_the_LTP_parameters_z)
                       + (41 # 10) * max0(-1
                                          + (s IDCalculation_of_the_LTP_parameters_k))
                       + (51 # 10) * max0(40
                                          - (s IDCalculation_of_the_LTP_parameters_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Calculation_of_the_LTP_parameters_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (40
                                                                - (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 8%positive => []
    | 9%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-4.1 0*) F_max0_monotonic (F_check_ge (40
                                                               - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-4.1 0*) F_max0_ge_0 (39
                                              - (s IDCalculation_of_the_LTP_parameters_k));
                      (*-4.1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-4.1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0));
                      (*0 4.1*) F_binom_monotonic 1 (F_max0_ge_arg (40
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-4.1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0));
                      (*-4.1 0*) F_binom_monotonic 1 (F_max0_ge_arg (40
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))]
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
    | 37%positive => [(*-1.64 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                      (*-1.64 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_scal)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal)));
                      (*-1.64 0*) F_binom_monotonic 1 (F_max0_ge_0 (100
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                      (*-1.64 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal)) (0))) (F_max0_ge_0 (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal)))]
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
    | 49%positive => [(*-1.35537 0*) F_max0_monotonic (F_check_ge (40
                                                                   - 
                                                                   (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1.35537 0*) F_max0_ge_0 (39
                                                  - (s IDCalculation_of_the_LTP_parameters_k));
                      (*-0.719628 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k)));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)))]
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_ge_0 (121
                                            - (s IDCalculation_of_the_LTP_parameters_lambda));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (121
                                                                    - (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (121
                                                                    - (s IDCalculation_of_the_LTP_parameters_lambda)))]
    | 55%positive => []
    | 56%positive => [(*-0.382936 0.719628*) F_max0_monotonic (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1.10256 0*) F_binom_monotonic 1 (F_max0_ge_0 (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0));
                      (*-1.10256 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 57%positive => [(*0 0.719628*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 58%positive => []
    | 59%positive => [(*-1.10256 0*) F_max0_pre_decrement (40
                                                           - (s IDCalculation_of_the_LTP_parameters_k)) (1);
                      (*-1.10256 0*) F_max0_ge_0 (39
                                                  - (s IDCalculation_of_the_LTP_parameters_k));
                      (*-1.10256 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0));
                      (*-1.82219 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1.10256 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 60%positive => []
    | 61%positive => [(*-0.719628 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0));
                      (*-0.382936 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1.10256 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)));
                      (*0 0.355372*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 62%positive => [(*0 0.355372*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)))]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => [(*0 1.075*) F_max0_monotonic (F_check_ge (40
                                                                - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1.075 0*) F_binom_monotonic 1 (F_max0_ge_0 (39
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0));
                      (*0 0.355372*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 68%positive => [(*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (121
                                                                    - (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (0) (0));
                      (*0 0.355372*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (121
                                                                    - (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (121
                                                                    - (s IDCalculation_of_the_LTP_parameters_lambda)));
                      (*-1.075 0*) F_binom_monotonic 1 (F_max0_ge_arg (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0));
                      (*0 0.355372*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)))]
    | 69%positive => []
    | 70%positive => [(*-0.719628 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0));
                      (*-0.719628 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (-
                                                                    (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (0) (0));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 71%positive => []
    | 72%positive => [(*-1.075 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0));
                      (*-1.075 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k)));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)))]
    | 73%positive => [(*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (0) (0))]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_z)))]
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => [(*-0.03 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal)) (0))) (F_max0_ge_0 (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal)));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-121
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)) (0))) (F_max0_ge_0 (-121
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda)))]
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                                             - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                      (*-1 0*) F_max0_ge_0 (39
                                            - (s IDCalculation_of_the_LTP_parameters_k));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0));
                      (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-121
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (0) (0))]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => []
    | 105%positive => []
    | 106%positive => [(*0 0.03*) F_binom_monotonic 1 (F_max0_ge_arg (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (100
                                                                    - (s IDCalculation_of_the_LTP_parameters_scal)) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                  - (s IDCalculation_of_the_LTP_parameters_bc_out_dref))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDCalculation_of_the_LTP_parameters_bc_out_dref)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDCalculation_of_the_LTP_parameters_bc_out_dref)))]
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                                              - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                       (*-1 0*) F_max0_ge_0 (39
                                             - (s IDCalculation_of_the_LTP_parameters_k));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_scal)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal)));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_ge_0 (100
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                       (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-121
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (0) (0))]
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                                              - (s IDCalculation_of_the_LTP_parameters_k)) (39
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                       (*-1 0*) F_max0_ge_0 (39
                                             - (s IDCalculation_of_the_LTP_parameters_k));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_scal)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_scal)));
                       (*-0.03 0*) F_binom_monotonic 1 (F_max0_ge_0 (100
                                                                    - 
                                                                    (s IDCalculation_of_the_LTP_parameters_scal))) (F_check_ge (0) (0));
                       (*-0.355372 0*) F_binom_monotonic 1 (F_max0_ge_0 (-121
                                                                    + (s IDCalculation_of_the_LTP_parameters_lambda))) (F_check_ge (0) (0))]
    | 115%positive => []
    | 116%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (40
                                                                   - 
                                                                   (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_z)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)));
                       (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_z)) (0))]
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => []
    | 131%positive => []
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
    | 144%positive => []
    | 145%positive => []
    | 146%positive => []
    | 147%positive => []
    | 148%positive => []
    | 149%positive => []
    | 150%positive => []
    | 151%positive => []
    | 152%positive => []
    | 153%positive => []
    | 154%positive => []
    | 155%positive => []
    | 156%positive => []
    | 157%positive => []
    | 158%positive => []
    | 159%positive => []
    | 160%positive => []
    | 161%positive => []
    | 162%positive => []
    | 163%positive => []
    | 164%positive => []
    | 165%positive => []
    | 166%positive => [(*0 0.719628*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 167%positive => []
    | 168%positive => []
    | 169%positive => []
    | 170%positive => []
    | 171%positive => []
    | 172%positive => []
    | 173%positive => []
    | 174%positive => []
    | 175%positive => []
    | 176%positive => []
    | 177%positive => []
    | 178%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0));
                       (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_z)));
                       (*0 0.719628*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k)))]
    | 179%positive => [(*0 2.075*) F_binom_monotonic 1 (F_max0_ge_arg (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | 180%positive => []
    | 181%positive => []
    | 182%positive => []
    | 183%positive => []
    | 184%positive => []
    | 185%positive => [(*-2.075 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 (40
                                                                    - (s IDCalculation_of_the_LTP_parameters_k)))]
    | 186%positive => []
    | 187%positive => []
    | 188%positive => [(*0 5.1*) F_max0_pre_decrement (40
                                                       - (s IDCalculation_of_the_LTP_parameters_k)) (1)]
    | 189%positive => []
    | 190%positive => []
    | 191%positive => []
    | 192%positive => []
    | 193%positive => []
    | 194%positive => []
    | 195%positive => []
    | 196%positive => []
    | 197%positive => []
    | 198%positive => []
    | 199%positive => []
    | 200%positive => []
    | 201%positive => []
    | 202%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))]
    | 203%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDCalculation_of_the_LTP_parameters_z))) (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))]
    | 204%positive => []
    | 205%positive => []
    | 206%positive => []
    | 207%positive => []
    | 208%positive => []
    | 209%positive => []
    | 210%positive => []
    | 211%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_z)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_z)));
                       (*-4.1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDCalculation_of_the_LTP_parameters_k)) (0))) (F_max0_ge_0 ((s IDCalculation_of_the_LTP_parameters_k)));
                       (*-4.1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k))) (F_check_ge (-1
                                                                    + (s IDCalculation_of_the_LTP_parameters_k)) (0))]
    | _ => []
  end.


Theorem Calculation_of_the_LTP_parameters_ai_correct:
  forall s p' s', steps (g_start Calculation_of_the_LTP_parameters) s (g_edges Calculation_of_the_LTP_parameters) p' s' -> Calculation_of_the_LTP_parameters_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Calculation_of_the_LTP_parameters_pot_correct:
  forall s p' s',
    steps (g_start Calculation_of_the_LTP_parameters) s (g_edges Calculation_of_the_LTP_parameters) p' s' ->
    (Calculation_of_the_LTP_parameters_pot (g_start Calculation_of_the_LTP_parameters) s >= Calculation_of_the_LTP_parameters_pot p' s')%Q.
Proof.
  check_lp Calculation_of_the_LTP_parameters_ai_correct Calculation_of_the_LTP_parameters_hints.
Qed.

