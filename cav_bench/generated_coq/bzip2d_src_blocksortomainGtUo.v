Require Import pasta.Pasta.

Notation IDmainGtU_z := 1%positive.
Notation IDmainGtU__tmp := 2%positive.
Notation IDmainGtU__tmp1 := 3%positive.
Notation IDmainGtU__tmp2 := 4%positive.
Notation IDmainGtU__tmp3 := 5%positive.
Notation IDmainGtU_budget_dref := 6%positive.
Notation IDmainGtU_c1 := 7%positive.
Notation IDmainGtU_c2 := 8%positive.
Notation IDmainGtU_k := 9%positive.
Notation IDmainGtU_s1 := 10%positive.
Notation IDmainGtU_s2 := 11%positive.
Notation IDmainGtU_block := 12%positive.
Notation IDmainGtU_budget := 13%positive.
Notation IDmainGtU_i1 := 14%positive.
Notation IDmainGtU_i2 := 15%positive.
Notation IDmainGtU_nblock := 16%positive.
Notation IDmainGtU_quadrant := 17%positive.
Definition mainGtU : graph := {|
  g_start := 1%positive;
  g_end := 327%positive;
  g_edges := (1%positive,(AAssign IDmainGtU_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp3)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp2)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp1)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDmainGtU__tmp1
             (Some (EVar IDmainGtU_i1))),7%positive)::
             (7%positive,(AAssign IDmainGtU__tmp3
             (Some (EVar IDmainGtU_i2))),8%positive)::
             (8%positive,(AAssign IDmainGtU__tmp2
             (Some (EVar IDmainGtU_nblock))),9%positive)::
             (9%positive,(AAssign IDmainGtU_c1 None),10%positive)::
             (10%positive,(AAssign IDmainGtU_c2 None),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),323%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),15%positive)::
             (15%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),16%positive)::
             (16%positive,(AAssign IDmainGtU_c1 None),17%positive)::
             (17%positive,(AAssign IDmainGtU_c2 None),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),319%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),22%positive)::
             (22%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),23%positive)::
             (23%positive,(AAssign IDmainGtU_c1 None),24%positive)::
             (24%positive,(AAssign IDmainGtU_c2 None),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),315%positive)::
             (26%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),29%positive)::
             (29%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),30%positive)::
             (30%positive,(AAssign IDmainGtU_c1 None),31%positive)::
             (31%positive,(AAssign IDmainGtU_c2 None),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),311%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),36%positive)::
             (36%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),37%positive)::
             (37%positive,(AAssign IDmainGtU_c1 None),38%positive)::
             (38%positive,(AAssign IDmainGtU_c2 None),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),307%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),43%positive)::
             (43%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),44%positive)::
             (44%positive,(AAssign IDmainGtU_c1 None),45%positive)::
             (45%positive,(AAssign IDmainGtU_c2 None),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),303%positive)::
             (47%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),50%positive)::
             (50%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),51%positive)::
             (51%positive,(AAssign IDmainGtU_c1 None),52%positive)::
             (52%positive,(AAssign IDmainGtU_c2 None),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),299%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),55%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),57%positive)::
             (57%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),58%positive)::
             (58%positive,(AAssign IDmainGtU_c1 None),59%positive)::
             (59%positive,(AAssign IDmainGtU_c2 None),60%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),295%positive)::
             (61%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),62%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),64%positive)::
             (64%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),65%positive)::
             (65%positive,(AAssign IDmainGtU_c1 None),66%positive)::
             (66%positive,(AAssign IDmainGtU_c2 None),67%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),291%positive)::
             (68%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),69%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),71%positive)::
             (71%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),72%positive)::
             (72%positive,(AAssign IDmainGtU_c1 None),73%positive)::
             (73%positive,(AAssign IDmainGtU_c2 None),74%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),287%positive)::
             (75%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),76%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),78%positive)::
             (78%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),79%positive)::
             (79%positive,(AAssign IDmainGtU_c1 None),80%positive)::
             (80%positive,(AAssign IDmainGtU_c2 None),81%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),283%positive)::
             (82%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),83%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),85%positive)::
             (85%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),86%positive)::
             (86%positive,(AAssign IDmainGtU_c1 None),87%positive)::
             (87%positive,(AAssign IDmainGtU_c2 None),88%positive)::
             (88%positive,AWeaken,89%positive)::
             (89%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),279%positive)::
             (89%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),90%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),92%positive)::
             (92%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),93%positive)::
             (93%positive,(AAssign IDmainGtU_k
             (Some (EAdd (EVar IDmainGtU__tmp2) (ENum (8))))),94%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,(AAssign IDmainGtU_c1 None),96%positive)::
             (96%positive,(AAssign IDmainGtU_c2 None),97%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),275%positive)::
             (98%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),99%positive)::
             (99%positive,AWeaken,100%positive)::
             (100%positive,(AAssign IDmainGtU_s1 None),101%positive)::
             (101%positive,(AAssign IDmainGtU_s2 None),102%positive)::
             (102%positive,AWeaken,103%positive)::
             (103%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),271%positive)::
             (103%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),104%positive)::
             (104%positive,AWeaken,105%positive)::
             (105%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),106%positive)::
             (106%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),107%positive)::
             (107%positive,(AAssign IDmainGtU_c1 None),108%positive)::
             (108%positive,(AAssign IDmainGtU_c2 None),109%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),267%positive)::
             (110%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),111%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,(AAssign IDmainGtU_s1 None),113%positive)::
             (113%positive,(AAssign IDmainGtU_s2 None),114%positive)::
             (114%positive,AWeaken,115%positive)::
             (115%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),263%positive)::
             (115%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),116%positive)::
             (116%positive,AWeaken,117%positive)::
             (117%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),118%positive)::
             (118%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),119%positive)::
             (119%positive,(AAssign IDmainGtU_c1 None),120%positive)::
             (120%positive,(AAssign IDmainGtU_c2 None),121%positive)::
             (121%positive,AWeaken,122%positive)::
             (122%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),259%positive)::
             (122%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),123%positive)::
             (123%positive,AWeaken,124%positive)::
             (124%positive,(AAssign IDmainGtU_s1 None),125%positive)::
             (125%positive,(AAssign IDmainGtU_s2 None),126%positive)::
             (126%positive,AWeaken,127%positive)::
             (127%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),255%positive)::
             (127%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),128%positive)::
             (128%positive,AWeaken,129%positive)::
             (129%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),130%positive)::
             (130%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),131%positive)::
             (131%positive,(AAssign IDmainGtU_c1 None),132%positive)::
             (132%positive,(AAssign IDmainGtU_c2 None),133%positive)::
             (133%positive,AWeaken,134%positive)::
             (134%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),251%positive)::
             (134%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),135%positive)::
             (135%positive,AWeaken,136%positive)::
             (136%positive,(AAssign IDmainGtU_s1 None),137%positive)::
             (137%positive,(AAssign IDmainGtU_s2 None),138%positive)::
             (138%positive,AWeaken,139%positive)::
             (139%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),247%positive)::
             (139%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),140%positive)::
             (140%positive,AWeaken,141%positive)::
             (141%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),142%positive)::
             (142%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),143%positive)::
             (143%positive,(AAssign IDmainGtU_c1 None),144%positive)::
             (144%positive,(AAssign IDmainGtU_c2 None),145%positive)::
             (145%positive,AWeaken,146%positive)::
             (146%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),243%positive)::
             (146%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),147%positive)::
             (147%positive,AWeaken,148%positive)::
             (148%positive,(AAssign IDmainGtU_s1 None),149%positive)::
             (149%positive,(AAssign IDmainGtU_s2 None),150%positive)::
             (150%positive,AWeaken,151%positive)::
             (151%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),239%positive)::
             (151%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),152%positive)::
             (152%positive,AWeaken,153%positive)::
             (153%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),154%positive)::
             (154%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),155%positive)::
             (155%positive,(AAssign IDmainGtU_c1 None),156%positive)::
             (156%positive,(AAssign IDmainGtU_c2 None),157%positive)::
             (157%positive,AWeaken,158%positive)::
             (158%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),235%positive)::
             (158%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),159%positive)::
             (159%positive,AWeaken,160%positive)::
             (160%positive,(AAssign IDmainGtU_s1 None),161%positive)::
             (161%positive,(AAssign IDmainGtU_s2 None),162%positive)::
             (162%positive,AWeaken,163%positive)::
             (163%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),231%positive)::
             (163%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),164%positive)::
             (164%positive,AWeaken,165%positive)::
             (165%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),166%positive)::
             (166%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),167%positive)::
             (167%positive,(AAssign IDmainGtU_c1 None),168%positive)::
             (168%positive,(AAssign IDmainGtU_c2 None),169%positive)::
             (169%positive,AWeaken,170%positive)::
             (170%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),227%positive)::
             (170%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),171%positive)::
             (171%positive,AWeaken,172%positive)::
             (172%positive,(AAssign IDmainGtU_s1 None),173%positive)::
             (173%positive,(AAssign IDmainGtU_s2 None),174%positive)::
             (174%positive,AWeaken,175%positive)::
             (175%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),223%positive)::
             (175%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),176%positive)::
             (176%positive,AWeaken,177%positive)::
             (177%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),178%positive)::
             (178%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),179%positive)::
             (179%positive,(AAssign IDmainGtU_c1 None),180%positive)::
             (180%positive,(AAssign IDmainGtU_c2 None),181%positive)::
             (181%positive,AWeaken,182%positive)::
             (182%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) <>
             (eval (EVar IDmainGtU_c2) s))%Z)),219%positive)::
             (182%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_c1) s) =
             (eval (EVar IDmainGtU_c2) s))%Z)),183%positive)::
             (183%positive,AWeaken,184%positive)::
             (184%positive,(AAssign IDmainGtU_s1 None),185%positive)::
             (185%positive,(AAssign IDmainGtU_s2 None),186%positive)::
             (186%positive,AWeaken,187%positive)::
             (187%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) <>
             (eval (EVar IDmainGtU_s2) s))%Z)),215%positive)::
             (187%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_s1) s) =
             (eval (EVar IDmainGtU_s2) s))%Z)),188%positive)::
             (188%positive,AWeaken,189%positive)::
             (189%positive,(AAssign IDmainGtU__tmp1
             (Some (EAdd (EVar IDmainGtU__tmp1) (ENum (1))))),190%positive)::
             (190%positive,(AAssign IDmainGtU__tmp3
             (Some (EAdd (EVar IDmainGtU__tmp3) (ENum (1))))),191%positive)::
             (191%positive,AWeaken,192%positive)::
             (192%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp1)
             s) >= (eval (EVar IDmainGtU__tmp2) s))%Z)),194%positive)::
             (192%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp1)
             s) < (eval (EVar IDmainGtU__tmp2) s))%Z)),193%positive)::
             (193%positive,AWeaken,198%positive)::
             (194%positive,AWeaken,195%positive)::
             (195%positive,(AAssign IDmainGtU__tmp1
             (Some (ESub (EVar IDmainGtU__tmp1) (EVar IDmainGtU__tmp2)))),
             196%positive)::(196%positive,ANone,197%positive)::
             (197%positive,AWeaken,198%positive)::
             (198%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp3)
             s) >= (eval (EVar IDmainGtU__tmp2) s))%Z)),200%positive)::
             (198%positive,(AGuard (fun s => ((eval (EVar IDmainGtU__tmp3)
             s) < (eval (EVar IDmainGtU__tmp2) s))%Z)),199%positive)::
             (199%positive,AWeaken,203%positive)::
             (200%positive,AWeaken,201%positive)::
             (201%positive,(AAssign IDmainGtU__tmp3
             (Some (ESub (EVar IDmainGtU__tmp3) (EVar IDmainGtU__tmp2)))),
             202%positive)::(202%positive,ANone,203%positive)::
             (203%positive,(AAssign IDmainGtU_k
             (Some (ESub (EVar IDmainGtU_k) (ENum (8))))),204%positive)::
             (204%positive,(AAssign IDmainGtU_budget_dref
             (Some (EAdd (EVar IDmainGtU_budget_dref) (ENum (-1))))),
             205%positive)::(205%positive,ANone,206%positive)::
             (206%positive,AWeaken,207%positive)::
             (207%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_k) s) >=
             (eval (ENum (0)) s))%Z)),212%positive)::
             (207%positive,(AGuard (fun s => ((eval (EVar IDmainGtU_k) s) <
             (eval (ENum (0)) s))%Z)),208%positive)::
             (208%positive,AWeaken,209%positive)::
             (209%positive,(AAssign IDmainGtU__tmp (Some (ENum (0)))),
             210%positive)::(210%positive,ANone,211%positive)::
             (211%positive,AWeaken,327%positive)::
             (212%positive,AWeaken,213%positive)::
             (213%positive,ANone,214%positive)::
             (214%positive,(AAssign IDmainGtU_z (Some (EAdd (ENum (1))
             (EVar IDmainGtU_z)))),95%positive)::
             (215%positive,AWeaken,216%positive)::
             (216%positive,(AAssign IDmainGtU__tmp None),217%positive)::
             (217%positive,ANone,218%positive)::
             (218%positive,AWeaken,327%positive)::
             (219%positive,AWeaken,220%positive)::
             (220%positive,(AAssign IDmainGtU__tmp None),221%positive)::
             (221%positive,ANone,222%positive)::
             (222%positive,AWeaken,327%positive)::
             (223%positive,AWeaken,224%positive)::
             (224%positive,(AAssign IDmainGtU__tmp None),225%positive)::
             (225%positive,ANone,226%positive)::
             (226%positive,AWeaken,327%positive)::
             (227%positive,AWeaken,228%positive)::
             (228%positive,(AAssign IDmainGtU__tmp None),229%positive)::
             (229%positive,ANone,230%positive)::
             (230%positive,AWeaken,327%positive)::
             (231%positive,AWeaken,232%positive)::
             (232%positive,(AAssign IDmainGtU__tmp None),233%positive)::
             (233%positive,ANone,234%positive)::
             (234%positive,AWeaken,327%positive)::
             (235%positive,AWeaken,236%positive)::
             (236%positive,(AAssign IDmainGtU__tmp None),237%positive)::
             (237%positive,ANone,238%positive)::
             (238%positive,AWeaken,327%positive)::
             (239%positive,AWeaken,240%positive)::
             (240%positive,(AAssign IDmainGtU__tmp None),241%positive)::
             (241%positive,ANone,242%positive)::
             (242%positive,AWeaken,327%positive)::
             (243%positive,AWeaken,244%positive)::
             (244%positive,(AAssign IDmainGtU__tmp None),245%positive)::
             (245%positive,ANone,246%positive)::
             (246%positive,AWeaken,327%positive)::
             (247%positive,AWeaken,248%positive)::
             (248%positive,(AAssign IDmainGtU__tmp None),249%positive)::
             (249%positive,ANone,250%positive)::
             (250%positive,AWeaken,327%positive)::
             (251%positive,AWeaken,252%positive)::
             (252%positive,(AAssign IDmainGtU__tmp None),253%positive)::
             (253%positive,ANone,254%positive)::
             (254%positive,AWeaken,327%positive)::
             (255%positive,AWeaken,256%positive)::
             (256%positive,(AAssign IDmainGtU__tmp None),257%positive)::
             (257%positive,ANone,258%positive)::
             (258%positive,AWeaken,327%positive)::
             (259%positive,AWeaken,260%positive)::
             (260%positive,(AAssign IDmainGtU__tmp None),261%positive)::
             (261%positive,ANone,262%positive)::
             (262%positive,AWeaken,327%positive)::
             (263%positive,AWeaken,264%positive)::
             (264%positive,(AAssign IDmainGtU__tmp None),265%positive)::
             (265%positive,ANone,266%positive)::
             (266%positive,AWeaken,327%positive)::
             (267%positive,AWeaken,268%positive)::
             (268%positive,(AAssign IDmainGtU__tmp None),269%positive)::
             (269%positive,ANone,270%positive)::
             (270%positive,AWeaken,327%positive)::
             (271%positive,AWeaken,272%positive)::
             (272%positive,(AAssign IDmainGtU__tmp None),273%positive)::
             (273%positive,ANone,274%positive)::
             (274%positive,AWeaken,327%positive)::
             (275%positive,AWeaken,276%positive)::
             (276%positive,(AAssign IDmainGtU__tmp None),277%positive)::
             (277%positive,ANone,278%positive)::
             (278%positive,AWeaken,327%positive)::
             (279%positive,AWeaken,280%positive)::
             (280%positive,(AAssign IDmainGtU__tmp None),281%positive)::
             (281%positive,ANone,282%positive)::
             (282%positive,AWeaken,327%positive)::
             (283%positive,AWeaken,284%positive)::
             (284%positive,(AAssign IDmainGtU__tmp None),285%positive)::
             (285%positive,ANone,286%positive)::
             (286%positive,AWeaken,327%positive)::
             (287%positive,AWeaken,288%positive)::
             (288%positive,(AAssign IDmainGtU__tmp None),289%positive)::
             (289%positive,ANone,290%positive)::
             (290%positive,AWeaken,327%positive)::
             (291%positive,AWeaken,292%positive)::
             (292%positive,(AAssign IDmainGtU__tmp None),293%positive)::
             (293%positive,ANone,294%positive)::
             (294%positive,AWeaken,327%positive)::
             (295%positive,AWeaken,296%positive)::
             (296%positive,(AAssign IDmainGtU__tmp None),297%positive)::
             (297%positive,ANone,298%positive)::
             (298%positive,AWeaken,327%positive)::
             (299%positive,AWeaken,300%positive)::
             (300%positive,(AAssign IDmainGtU__tmp None),301%positive)::
             (301%positive,ANone,302%positive)::
             (302%positive,AWeaken,327%positive)::
             (303%positive,AWeaken,304%positive)::
             (304%positive,(AAssign IDmainGtU__tmp None),305%positive)::
             (305%positive,ANone,306%positive)::
             (306%positive,AWeaken,327%positive)::
             (307%positive,AWeaken,308%positive)::
             (308%positive,(AAssign IDmainGtU__tmp None),309%positive)::
             (309%positive,ANone,310%positive)::
             (310%positive,AWeaken,327%positive)::
             (311%positive,AWeaken,312%positive)::
             (312%positive,(AAssign IDmainGtU__tmp None),313%positive)::
             (313%positive,ANone,314%positive)::
             (314%positive,AWeaken,327%positive)::
             (315%positive,AWeaken,316%positive)::
             (316%positive,(AAssign IDmainGtU__tmp None),317%positive)::
             (317%positive,ANone,318%positive)::
             (318%positive,AWeaken,327%positive)::
             (319%positive,AWeaken,320%positive)::
             (320%positive,(AAssign IDmainGtU__tmp None),321%positive)::
             (321%positive,ANone,322%positive)::
             (322%positive,AWeaken,327%positive)::
             (323%positive,AWeaken,324%positive)::
             (324%positive,(AAssign IDmainGtU__tmp None),325%positive)::
             (325%positive,ANone,326%positive)::
             (326%positive,AWeaken,327%positive)::nil
|}.

Definition mainGtU_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp3) <= 0)%Z
    | 4%positive => (-1 * (s IDmainGtU__tmp3) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp2) <= 0)%Z
    | 5%positive => (-1 * (s IDmainGtU__tmp2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp3) <= 0 /\ -1 * (s IDmainGtU__tmp1) <= 0)%Z
    | 6%positive => (-1 * (s IDmainGtU__tmp1) <= 0 /\ -1 * (s IDmainGtU__tmp3) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp2) <= 0)%Z
    | 7%positive => (-1 * (s IDmainGtU__tmp2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp3) <= 0)%Z
    | 8%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU__tmp2) <= 0)%Z
    | 9%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 10%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 12%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 14%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 15%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 16%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 17%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 18%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 19%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 20%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 21%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 22%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 23%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 24%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 25%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 26%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 27%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 28%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 29%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 30%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 31%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 32%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 33%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 34%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 35%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 36%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 37%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 38%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 39%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 40%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 41%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 42%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 43%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 44%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 45%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 46%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 47%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 48%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 49%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 50%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 51%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 52%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 53%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 54%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 55%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 56%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 57%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 58%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 59%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 60%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 61%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 62%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 63%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 64%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 65%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 66%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 67%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 68%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 69%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 70%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 71%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 72%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 73%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 74%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 75%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 76%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 77%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 78%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 79%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 80%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 81%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 82%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 83%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 84%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 85%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 86%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 87%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 88%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 89%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 90%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 91%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 92%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 93%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 94%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 95%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 96%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 97%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 98%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 99%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 100%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 101%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 102%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 103%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 104%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 105%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 106%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 107%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 108%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 109%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 110%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 111%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 112%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 113%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 114%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 115%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 116%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 117%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 118%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 119%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 120%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 121%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 122%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 123%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 124%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 125%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 126%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 127%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 128%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 129%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 130%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 131%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 132%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 133%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 134%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 135%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 136%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 137%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 138%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 139%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 140%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 141%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 142%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 143%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 144%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 145%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 146%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 147%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 148%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 149%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 150%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 151%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 152%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 153%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 154%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 155%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 156%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 157%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 158%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 159%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 160%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 161%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 162%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 163%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 164%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 165%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 166%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 167%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 168%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 169%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 170%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 171%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 172%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 173%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 174%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 175%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 176%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 177%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 178%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 179%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 180%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 181%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 182%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 183%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 184%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 185%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 186%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 187%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 188%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 189%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 190%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 191%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 192%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 193%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU__tmp1)+ -1 * (s IDmainGtU__tmp2) + 1 <= 0)%Z
    | 194%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU__tmp1)+ 1 * (s IDmainGtU__tmp2) <= 0)%Z
    | 195%positive => (-1 * (s IDmainGtU__tmp1)+ 1 * (s IDmainGtU__tmp2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 196%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU__tmp1) <= 0)%Z
    | 197%positive => (-1 * (s IDmainGtU__tmp1) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 198%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 199%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU__tmp2)+ 1 * (s IDmainGtU__tmp3) + 1 <= 0)%Z
    | 200%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU__tmp2)+ -1 * (s IDmainGtU__tmp3) <= 0)%Z
    | 201%positive => (1 * (s IDmainGtU__tmp2)+ -1 * (s IDmainGtU__tmp3) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 202%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU__tmp3) <= 0)%Z
    | 203%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 204%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 205%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 206%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 207%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 208%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_k) + 1 <= 0)%Z
    | 209%positive => (1 * (s IDmainGtU_k) + 1 <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 210%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_k) + 1 <= 0 /\ 1 * (s IDmainGtU__tmp) <= 0 /\ -1 * (s IDmainGtU__tmp) <= 0)%Z
    | 211%positive => (-1 * (s IDmainGtU__tmp) <= 0 /\ 1 * (s IDmainGtU__tmp) <= 0 /\ 1 * (s IDmainGtU_k) + 1 <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 212%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_k) <= 0)%Z
    | 213%positive => (-1 * (s IDmainGtU_k) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 214%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_k) <= 0)%Z
    | 215%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 216%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 217%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 218%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 219%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 220%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 221%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 222%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 223%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 224%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 225%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 226%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 227%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 228%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 229%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 230%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 231%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 232%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 233%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 234%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 235%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 236%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 237%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 238%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 239%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 240%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 241%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 242%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 243%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 244%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 245%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 246%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 247%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 248%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 249%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 250%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 251%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 252%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 253%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 254%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 255%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 256%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 257%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 258%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 259%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 260%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 261%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 262%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 263%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 264%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 265%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 266%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 267%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 268%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 269%positive => (-1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 270%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_s1)+ -1 * (s IDmainGtU_s2) <= 0 /\ -1 * (s IDmainGtU_s1)+ 1 * (s IDmainGtU_s2) <= 0)%Z
    | 271%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 272%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 273%positive => (-1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 274%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_c1)+ -1 * (s IDmainGtU_c2) <= 0 /\ -1 * (s IDmainGtU_c1)+ 1 * (s IDmainGtU_c2) <= 0)%Z
    | 275%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 276%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 277%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 278%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | 279%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 280%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 281%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 282%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 283%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 284%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 285%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 286%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 287%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 288%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 289%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 290%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 291%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 292%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 293%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 294%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 295%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 296%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 297%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 298%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 299%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 300%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 301%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 302%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 303%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 304%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 305%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 306%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 307%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 308%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 309%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 310%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 311%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 312%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 313%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 314%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 315%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 316%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 317%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 318%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 319%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 320%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 321%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 322%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 323%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 324%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 325%positive => (-1 * (s IDmainGtU_z) <= 0 /\ 1 * (s IDmainGtU_z) <= 0)%Z
    | 326%positive => (1 * (s IDmainGtU_z) <= 0 /\ -1 * (s IDmainGtU_z) <= 0)%Z
    | 327%positive => (-1 * (s IDmainGtU_z) <= 0)%Z
    | _ => False
  end.

Definition mainGtU_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 2%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 3%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 4%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 5%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 6%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 7%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 8%positive => ((1 # 8) * max0(8 + (s IDmainGtU_nblock)))%Q
    | 9%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 10%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 11%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 12%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 13%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 14%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 15%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 16%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 17%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 18%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 19%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 20%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 21%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 22%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 23%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 24%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 25%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 26%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 27%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 28%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 29%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 30%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 31%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 32%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 33%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 34%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 35%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 36%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 37%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 38%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 39%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 40%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 41%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 42%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 43%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 44%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 45%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 46%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 47%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 48%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 49%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 50%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 51%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 52%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 53%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 54%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 55%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 56%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 57%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 58%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 59%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 60%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 61%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 62%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 63%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 64%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 65%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 66%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 67%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 68%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 69%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 70%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 71%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 72%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 73%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 74%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 75%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 76%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 77%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 78%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 79%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 80%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 81%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 82%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 83%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 84%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 85%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 86%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 87%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 88%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 89%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 90%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 91%positive => ((s IDmainGtU_z)
                      + (1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 92%positive => ((s IDmainGtU_z)
                      + (1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 93%positive => ((s IDmainGtU_z)
                      + (1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 94%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 95%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 96%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 97%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 98%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 99%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 100%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 101%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 102%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 103%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 104%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 105%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 106%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 107%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 108%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 109%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 110%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 111%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 112%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 113%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 114%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 115%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 116%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 117%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 118%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 119%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 120%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 121%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 122%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 123%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 124%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 125%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 126%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 127%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 128%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 129%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 130%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 131%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 132%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 133%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 134%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 135%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 136%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 137%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 138%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 139%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 140%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 141%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 142%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 143%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 144%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 145%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 146%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 147%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 148%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 149%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 150%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 151%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 152%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 153%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 154%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 155%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 156%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 157%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 158%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 159%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 160%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 161%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 162%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 163%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 164%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 165%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 166%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 167%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 168%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 169%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 170%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 171%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 172%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 173%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 174%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 175%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 176%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 177%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 178%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 179%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 180%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 181%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 182%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 183%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 184%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 185%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 186%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 187%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 188%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 189%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 190%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 191%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 192%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 193%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 194%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 195%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 196%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 197%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 198%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 199%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 200%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 201%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 202%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 203%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 204%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 205%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 206%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 207%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 208%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 209%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 210%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 211%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 212%positive => ((s IDmainGtU_z) + (1 # 8) * max0(8 + (s IDmainGtU_k)))%Q
    | 213%positive => ((1 # 1) + (s IDmainGtU_z)
                       + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 214%positive => ((1 # 1) + (s IDmainGtU_z)
                       + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 215%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 216%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 217%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 218%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 219%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 220%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 221%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 222%positive => ((1 # 8) * max0((s IDmainGtU_k))
                       + max0((s IDmainGtU_z)))%Q
    | 223%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 224%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 225%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 226%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 227%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 228%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 229%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 230%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 231%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 232%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 233%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 234%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 235%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 236%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 237%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 238%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 239%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 240%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 241%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 242%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 243%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 244%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 245%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 246%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 247%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 248%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 249%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 250%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 251%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 252%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 253%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 254%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 255%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 256%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 257%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 258%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 259%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 260%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 261%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 262%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 263%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 264%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 265%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 266%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 267%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 268%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 269%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 270%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 271%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 272%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 273%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 274%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 275%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 276%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 277%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 278%positive => ((s IDmainGtU_z) + (1 # 8) * max0((s IDmainGtU_k)))%Q
    | 279%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 280%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 281%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 282%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 283%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 284%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 285%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 286%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 287%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 288%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 289%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 290%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 291%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 292%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 293%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 294%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 295%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 296%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 297%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 298%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 299%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 300%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 301%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 302%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 303%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 304%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 305%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 306%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 307%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 308%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 309%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 310%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 311%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 312%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 313%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 314%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 315%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 316%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 317%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 318%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 319%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 320%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 321%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 322%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 323%positive => ((1 # 8) * max0(8 + (s IDmainGtU__tmp2)))%Q
    | 324%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 325%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 326%positive => ((s IDmainGtU_z)
                       + (1 # 8) * max0(8 + (s IDmainGtU__tmp2))
                       + max0(-(s IDmainGtU_z)))%Q
    | 327%positive => ((s IDmainGtU_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mainGtU_hints (p : node) (s : state) := 
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
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 91%positive => []
    | 92%positive => []
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
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
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
    | 166%positive => []
    | 167%positive => []
    | 168%positive => []
    | 169%positive => []
    | 170%positive => []
    | 171%positive => []
    | 172%positive => []
    | 173%positive => []
    | 174%positive => []
    | 175%positive => []
    | 176%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmainGtU_z)) (0))) (F_max0_ge_0 ((s IDmainGtU_z)))]
    | 177%positive => []
    | 178%positive => []
    | 179%positive => []
    | 180%positive => []
    | 181%positive => []
    | 182%positive => []
    | 183%positive => []
    | 184%positive => []
    | 185%positive => []
    | 186%positive => []
    | 187%positive => []
    | 188%positive => []
    | 189%positive => []
    | 190%positive => []
    | 191%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmainGtU_z))) (F_check_ge ((s IDmainGtU_z)) (0))]
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
    | 202%positive => []
    | 203%positive => []
    | 204%positive => []
    | 205%positive => []
    | 206%positive => []
    | 207%positive => []
    | 208%positive => []
    | 209%positive => []
    | 210%positive => []
    | 211%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge (8
                                                                  + (s IDmainGtU_k)) ((s IDmainGtU_k)));
                       (*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 212%positive => [(*-0.125 0*) F_max0_pre_decrement (8 + (s IDmainGtU_k)) (8)]
    | 213%positive => []
    | 214%positive => []
    | 215%positive => []
    | 216%positive => []
    | 217%positive => []
    | 218%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmainGtU_z))) (F_check_ge ((s IDmainGtU_z)) (0))]
    | 219%positive => []
    | 220%positive => []
    | 221%positive => []
    | 222%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmainGtU_z))) (F_check_ge ((s IDmainGtU_z)) (0))]
    | 223%positive => []
    | 224%positive => []
    | 225%positive => []
    | 226%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 227%positive => []
    | 228%positive => []
    | 229%positive => []
    | 230%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 231%positive => []
    | 232%positive => []
    | 233%positive => []
    | 234%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 235%positive => []
    | 236%positive => []
    | 237%positive => []
    | 238%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 239%positive => []
    | 240%positive => []
    | 241%positive => []
    | 242%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 243%positive => []
    | 244%positive => []
    | 245%positive => []
    | 246%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 247%positive => []
    | 248%positive => []
    | 249%positive => []
    | 250%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 251%positive => []
    | 252%positive => []
    | 253%positive => []
    | 254%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 255%positive => []
    | 256%positive => []
    | 257%positive => []
    | 258%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 259%positive => []
    | 260%positive => []
    | 261%positive => []
    | 262%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 263%positive => []
    | 264%positive => []
    | 265%positive => []
    | 266%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 267%positive => []
    | 268%positive => []
    | 269%positive => []
    | 270%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 271%positive => []
    | 272%positive => []
    | 273%positive => []
    | 274%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 275%positive => []
    | 276%positive => []
    | 277%positive => []
    | 278%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmainGtU_k))]
    | 279%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 280%positive => []
    | 281%positive => []
    | 282%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 283%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 284%positive => []
    | 285%positive => []
    | 286%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 287%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 288%positive => []
    | 289%positive => []
    | 290%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 291%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 292%positive => []
    | 293%positive => []
    | 294%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 295%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 296%positive => []
    | 297%positive => []
    | 298%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 299%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 300%positive => []
    | 301%positive => []
    | 302%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 303%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 304%positive => []
    | 305%positive => []
    | 306%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 307%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 308%positive => []
    | 309%positive => []
    | 310%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 311%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 312%positive => []
    | 313%positive => []
    | 314%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 315%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 316%positive => []
    | 317%positive => []
    | 318%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 319%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 320%positive => []
    | 321%positive => []
    | 322%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 323%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmainGtU_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmainGtU_z)))]
    | 324%positive => []
    | 325%positive => []
    | 326%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmainGtU_z))) (F_check_ge (0) (0));
                       (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    + (s IDmainGtU__tmp2))) (F_check_ge (0) (0))]
    | 327%positive => []
    | _ => []
  end.


Theorem mainGtU_ai_correct:
  forall s p' s', steps (g_start mainGtU) s (g_edges mainGtU) p' s' -> mainGtU_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mainGtU_pot_correct:
  forall s p' s',
    steps (g_start mainGtU) s (g_edges mainGtU) p' s' ->
    (mainGtU_pot (g_start mainGtU) s >= mainGtU_pot p' s')%Q.
Proof.
  check_lp mainGtU_ai_correct mainGtU_hints.
Qed.

