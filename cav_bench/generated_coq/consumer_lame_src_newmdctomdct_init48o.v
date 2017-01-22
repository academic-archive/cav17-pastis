Require Import pasta.Pasta.

Notation IDmdct_init48_z := 1%positive.
Notation IDmdct_init48_i := 2%positive.
Notation IDmdct_init48_j := 3%positive.
Notation IDmdct_init48_k := 4%positive.
Notation IDmdct_init48_m := 5%positive.
Definition mdct_init48 : graph := {|
  g_start := 1%positive;
  g_end := 113%positive;
  g_edges := (1%positive,(AAssign IDmdct_init48_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k) s) <
             (eval (ENum (8)) s))%Z)),278%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDmdct_init48_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (36)) s))%Z)),271%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (36)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDmdct_init48_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (18)) s))%Z)),264%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (18)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (24)) s))%Z)),257%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (24)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (30)) s))%Z)),250%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (30)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (36)) s))%Z)),243%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (36)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDmdct_init48_i (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (36)) s))%Z)),236%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (36)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDmdct_init48_j (Some (ENum (11)))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDmdct_init48_m None),37%positive)::
             (37%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (9)) s))%Z)),229%positive)::
             (40%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (9)) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (9)) s))%Z)),222%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (9)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDmdct_init48_j
             (Some (EAdd (EVar IDmdct_init48_j) (ENum (-1))))),49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) >=
             (eval (ENum (0)) s))%Z)),219%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) <
             (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AAssign IDmdct_init48_j (Some (ENum (3)))),
             53%positive)::(53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDmdct_init48_m None),55%positive)::
             (55%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             56%positive)::(56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (3)) s))%Z)),212%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (3)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDmdct_init48_k (Some (ENum (6)))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (9)) s))%Z)),205%positive)::
             (63%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (9)) s))%Z)),64%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDmdct_init48_j
             (Some (EAdd (EVar IDmdct_init48_j) (ENum (-1))))),67%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) >=
             (eval (ENum (0)) s))%Z)),202%positive)::
             (68%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) <
             (eval (ENum (0)) s))%Z)),69%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,(AAssign IDmdct_init48_j (Some (ENum (1)))),
             71%positive)::(71%positive,ANone,72%positive)::
             (72%positive,(AAssign IDmdct_init48_m None),73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDmdct_init48_j
             (Some (EAdd (EVar IDmdct_init48_j) (ENum (-1))))),75%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) >=
             (eval (ENum (0)) s))%Z)),199%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmdct_init48_j) (ENum (-1))) s) <
             (eval (ENum (0)) s))%Z)),77%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             79%positive)::(79%positive,ANone,80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (7)) s))%Z)),192%positive)::
             (81%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (7)) s))%Z)),82%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AAssign IDmdct_init48_i (Some (ENum (14)))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (0)) s))%Z)),173%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (0)) s))%Z)),87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             89%positive)::(89%positive,ANone,90%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (7)) s))%Z)),166%positive)::
             (91%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (7)) s))%Z)),92%positive)::
             (92%positive,AWeaken,93%positive)::
             (93%positive,(AAssign IDmdct_init48_i (Some (ENum (15)))),
             94%positive)::(94%positive,ANone,95%positive)::
             (95%positive,AWeaken,96%positive)::
             (96%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (0)) s))%Z)),147%positive)::
             (96%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (0)) s))%Z)),97%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             99%positive)::(99%positive,ANone,100%positive)::
             (100%positive,AWeaken,101%positive)::
             (101%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (4)) s))%Z)),140%positive)::
             (101%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (4)) s))%Z)),102%positive)::
             (102%positive,AWeaken,103%positive)::
             (103%positive,(AAssign IDmdct_init48_i (Some (ENum (0)))),
             104%positive)::(104%positive,ANone,105%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (36)) s))%Z)),133%positive)::
             (106%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (36)) s))%Z)),107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,(AAssign IDmdct_init48_i (Some (ENum (0)))),
             109%positive)::(109%positive,ANone,110%positive)::
             (110%positive,AWeaken,111%positive)::
             (111%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) < (eval (ENum (3)) s))%Z)),114%positive)::
             (111%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_i)
             s) >= (eval (ENum (3)) s))%Z)),112%positive)::
             (112%positive,AWeaken,113%positive)::
             (114%positive,AWeaken,115%positive)::
             (115%positive,(AAssign IDmdct_init48_m (Some (ENum (0)))),
             116%positive)::(116%positive,ANone,117%positive)::
             (117%positive,AWeaken,118%positive)::
             (118%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_m)
             s) < (eval (ENum (6)) s))%Z)),126%positive)::
             (118%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_m)
             s) >= (eval (ENum (6)) s))%Z)),119%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,ANone,121%positive)::
             (121%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),125%positive)::
             (125%positive,AWeaken,111%positive)::
             (126%positive,AWeaken,127%positive)::
             (127%positive,ANone,128%positive)::
             (128%positive,(AAssign IDmdct_init48_m
             (Some (EAdd (EVar IDmdct_init48_m) (ENum (1))))),129%positive)::
             (129%positive,ANone,130%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),132%positive)::
             (132%positive,AWeaken,118%positive)::
             (133%positive,AWeaken,134%positive)::
             (134%positive,ANone,135%positive)::
             (135%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),136%positive)::
             (136%positive,ANone,137%positive)::
             (137%positive,ANone,138%positive)::
             (138%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),139%positive)::
             (139%positive,AWeaken,106%positive)::
             (140%positive,AWeaken,141%positive)::
             (141%positive,ANone,142%positive)::
             (142%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),143%positive)::
             (143%positive,ANone,144%positive)::
             (144%positive,ANone,145%positive)::
             (145%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),146%positive)::
             (146%positive,AWeaken,101%positive)::
             (147%positive,AWeaken,148%positive)::
             (148%positive,(AAssign IDmdct_init48_k (Some (ENum (1)))),
             149%positive)::(149%positive,ANone,150%positive)::
             (150%positive,AWeaken,151%positive)::
             (151%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (32)) s))%Z)),159%positive)::
             (151%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (32)) s))%Z)),152%positive)::
             (152%positive,AWeaken,153%positive)::
             (153%positive,ANone,154%positive)::
             (154%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (-1))))),155%positive)::
             (155%positive,ANone,156%positive)::
             (156%positive,ANone,157%positive)::
             (157%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),158%positive)::
             (158%positive,AWeaken,96%positive)::
             (159%positive,AWeaken,160%positive)::
             (160%positive,ANone,161%positive)::
             (161%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),162%positive)::
             (162%positive,ANone,163%positive)::
             (163%positive,ANone,164%positive)::
             (164%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),165%positive)::
             (165%positive,AWeaken,151%positive)::
             (166%positive,AWeaken,167%positive)::
             (167%positive,ANone,168%positive)::
             (168%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),169%positive)::
             (169%positive,ANone,170%positive)::
             (170%positive,ANone,171%positive)::
             (171%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),172%positive)::
             (172%positive,AWeaken,91%positive)::
             (173%positive,AWeaken,174%positive)::
             (174%positive,(AAssign IDmdct_init48_k (Some (ENum (0)))),
             175%positive)::(175%positive,ANone,176%positive)::
             (176%positive,AWeaken,177%positive)::
             (177%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) < (eval (ENum (15)) s))%Z)),185%positive)::
             (177%positive,(AGuard (fun s => ((eval (EVar IDmdct_init48_k)
             s) >= (eval (ENum (15)) s))%Z)),178%positive)::
             (178%positive,AWeaken,179%positive)::
             (179%positive,ANone,180%positive)::
             (180%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (-1))))),181%positive)::
             (181%positive,ANone,182%positive)::
             (182%positive,ANone,183%positive)::
             (183%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),184%positive)::
             (184%positive,AWeaken,86%positive)::
             (185%positive,AWeaken,186%positive)::
             (186%positive,ANone,187%positive)::
             (187%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),188%positive)::
             (188%positive,ANone,189%positive)::
             (189%positive,ANone,190%positive)::
             (190%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),191%positive)::
             (191%positive,AWeaken,177%positive)::
             (192%positive,AWeaken,193%positive)::
             (193%positive,ANone,194%positive)::
             (194%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),195%positive)::
             (195%positive,ANone,196%positive)::
             (196%positive,ANone,197%positive)::
             (197%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),198%positive)::
             (198%positive,AWeaken,81%positive)::
             (199%positive,AWeaken,200%positive)::
             (200%positive,ANone,201%positive)::
             (201%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),72%positive)::
             (202%positive,AWeaken,203%positive)::
             (203%positive,ANone,204%positive)::
             (204%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),54%positive)::
             (205%positive,AWeaken,206%positive)::
             (206%positive,ANone,207%positive)::
             (207%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),208%positive)::
             (208%positive,ANone,209%positive)::
             (209%positive,ANone,210%positive)::
             (210%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),211%positive)::
             (211%positive,AWeaken,63%positive)::
             (212%positive,AWeaken,213%positive)::
             (213%positive,ANone,214%positive)::
             (214%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),215%positive)::
             (215%positive,ANone,216%positive)::
             (216%positive,ANone,217%positive)::
             (217%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),218%positive)::
             (218%positive,AWeaken,58%positive)::
             (219%positive,AWeaken,220%positive)::
             (220%positive,ANone,221%positive)::
             (221%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),36%positive)::
             (222%positive,AWeaken,223%positive)::
             (223%positive,ANone,224%positive)::
             (224%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),225%positive)::
             (225%positive,ANone,226%positive)::
             (226%positive,ANone,227%positive)::
             (227%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),228%positive)::
             (228%positive,AWeaken,45%positive)::
             (229%positive,AWeaken,230%positive)::
             (230%positive,ANone,231%positive)::
             (231%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),232%positive)::
             (232%positive,ANone,233%positive)::
             (233%positive,ANone,234%positive)::
             (234%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),235%positive)::
             (235%positive,AWeaken,40%positive)::
             (236%positive,AWeaken,237%positive)::
             (237%positive,ANone,238%positive)::
             (238%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),239%positive)::
             (239%positive,ANone,240%positive)::
             (240%positive,ANone,241%positive)::
             (241%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),242%positive)::
             (242%positive,AWeaken,32%positive)::
             (243%positive,AWeaken,244%positive)::
             (244%positive,ANone,245%positive)::
             (245%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),246%positive)::
             (246%positive,ANone,247%positive)::
             (247%positive,ANone,248%positive)::
             (248%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),249%positive)::
             (249%positive,AWeaken,27%positive)::
             (250%positive,AWeaken,251%positive)::
             (251%positive,ANone,252%positive)::
             (252%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),253%positive)::
             (253%positive,ANone,254%positive)::
             (254%positive,ANone,255%positive)::
             (255%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),256%positive)::
             (256%positive,AWeaken,23%positive)::
             (257%positive,AWeaken,258%positive)::
             (258%positive,ANone,259%positive)::
             (259%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),260%positive)::
             (260%positive,ANone,261%positive)::
             (261%positive,ANone,262%positive)::
             (262%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),263%positive)::
             (263%positive,AWeaken,19%positive)::
             (264%positive,AWeaken,265%positive)::
             (265%positive,ANone,266%positive)::
             (266%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),267%positive)::
             (267%positive,ANone,268%positive)::
             (268%positive,ANone,269%positive)::
             (269%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),270%positive)::
             (270%positive,AWeaken,15%positive)::
             (271%positive,AWeaken,272%positive)::
             (272%positive,ANone,273%positive)::
             (273%positive,(AAssign IDmdct_init48_i
             (Some (EAdd (EVar IDmdct_init48_i) (ENum (1))))),274%positive)::
             (274%positive,ANone,275%positive)::
             (275%positive,ANone,276%positive)::
             (276%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),277%positive)::
             (277%positive,AWeaken,10%positive)::
             (278%positive,AWeaken,279%positive)::
             (279%positive,ANone,280%positive)::
             (280%positive,(AAssign IDmdct_init48_k
             (Some (EAdd (EVar IDmdct_init48_k) (ENum (1))))),281%positive)::
             (281%positive,ANone,282%positive)::
             (282%positive,ANone,283%positive)::
             (283%positive,(AAssign IDmdct_init48_z (Some (EAdd (ENum (1))
             (EVar IDmdct_init48_z)))),284%positive)::
             (284%positive,AWeaken,5%positive)::nil
|}.

Definition mdct_init48_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 4%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 9%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 10%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 11%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0)%Z
    | 12%positive => (-1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 13%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 14%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0)%Z
    | 15%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -18 <= 0)%Z
    | 16%positive => (1 * (s IDmdct_init48_i) + -18 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 18 <= 0)%Z
    | 17%positive => (-1 * (s IDmdct_init48_i) + 18 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -18 <= 0)%Z
    | 18%positive => (1 * (s IDmdct_init48_i) + -18 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 18 <= 0)%Z
    | 19%positive => (-1 * (s IDmdct_init48_i) + 18 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -24 <= 0)%Z
    | 20%positive => (1 * (s IDmdct_init48_i) + -24 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 24 <= 0)%Z
    | 21%positive => (-1 * (s IDmdct_init48_i) + 24 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -24 <= 0)%Z
    | 22%positive => (1 * (s IDmdct_init48_i) + -24 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 24 <= 0)%Z
    | 23%positive => (-1 * (s IDmdct_init48_i) + 24 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -30 <= 0)%Z
    | 24%positive => (1 * (s IDmdct_init48_i) + -30 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 30 <= 0)%Z
    | 25%positive => (-1 * (s IDmdct_init48_i) + 30 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -30 <= 0)%Z
    | 26%positive => (1 * (s IDmdct_init48_i) + -30 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 30 <= 0)%Z
    | 27%positive => (-1 * (s IDmdct_init48_i) + 30 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 28%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0)%Z
    | 29%positive => (-1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 30%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 31%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 32%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 33%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0)%Z
    | 34%positive => (-1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 35%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_j) + 11 <= 0)%Z
    | 36%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 37%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0)%Z
    | 38%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 39%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0)%Z
    | 40%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 41%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 42%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 43%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 44%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0)%Z
    | 45%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 46%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 47%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 48%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 49%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -10 <= 0)%Z
    | 50%positive => (1 * (s IDmdct_init48_j) + -10 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 51%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 52%positive => (1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 53%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_j) + 3 <= 0)%Z
    | 54%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 55%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0)%Z
    | 56%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 57%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0)%Z
    | 58%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 59%positive => (1 * (s IDmdct_init48_k) + -3 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 3 <= 0)%Z
    | 60%positive => (-1 * (s IDmdct_init48_k) + 3 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 61%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0 /\ -1 * (s IDmdct_init48_k) + 6 <= 0)%Z
    | 62%positive => (-1 * (s IDmdct_init48_k) + 6 <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0)%Z
    | 63%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 6 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 64%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 65%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 66%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 67%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -2 <= 0)%Z
    | 68%positive => (1 * (s IDmdct_init48_j) + -2 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 69%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 70%positive => (1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 71%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -1 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDmdct_init48_j) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 73%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -1 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDmdct_init48_j) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 75%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 76%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 77%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 78%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 79%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 80%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 81%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 82%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0)%Z
    | 83%positive => (-1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 84%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_i) + 14 <= 0)%Z
    | 85%positive => (-1 * (s IDmdct_init48_i) + 14 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 86%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 87%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 88%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 89%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 90%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 91%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 92%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0)%Z
    | 93%positive => (-1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 94%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_i) + 15 <= 0)%Z
    | 95%positive => (-1 * (s IDmdct_init48_i) + 15 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 96%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 97%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 98%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 99%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 100%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 101%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0)%Z
    | 102%positive => (1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 103%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0)%Z
    | 104%positive => (1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 105%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0)%Z
    | 106%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 107%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0)%Z
    | 108%positive => (-1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 109%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 110%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 111%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 112%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 3 <= 0)%Z
    | 113%positive => (-1 * (s IDmdct_init48_i) + 3 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 114%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -2 <= 0)%Z
    | 115%positive => (1 * (s IDmdct_init48_i) + -2 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 116%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -2 <= 0 /\ 1 * (s IDmdct_init48_m) <= 0 /\ -1 * (s IDmdct_init48_m) <= 0)%Z
    | 117%positive => (-1 * (s IDmdct_init48_m) <= 0 /\ 1 * (s IDmdct_init48_m) <= 0 /\ 1 * (s IDmdct_init48_i) + -2 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 118%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_m) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0)%Z
    | 119%positive => (1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_m) + 6 <= 0)%Z
    | 120%positive => (-1 * (s IDmdct_init48_m) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0)%Z
    | 121%positive => (1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_m) + 6 <= 0)%Z
    | 122%positive => (-1 * (s IDmdct_init48_m) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 123%positive => (-1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_m) + 6 <= 0)%Z
    | 124%positive => (-1 * (s IDmdct_init48_m) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 125%positive => (-1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_m) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 126%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_m) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_m) + -5 <= 0)%Z
    | 127%positive => (1 * (s IDmdct_init48_m) + -5 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_m) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 128%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_m) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_m) + -5 <= 0)%Z
    | 129%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_m) + 1 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0)%Z
    | 130%positive => (1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_m) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 131%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_m) + 1 <= 0 /\ 1 * (s IDmdct_init48_m) + -6 <= 0)%Z
    | 132%positive => (1 * (s IDmdct_init48_m) + -6 <= 0 /\ -1 * (s IDmdct_init48_m) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 133%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 134%positive => (1 * (s IDmdct_init48_i) + -35 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0)%Z
    | 135%positive => (-1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 136%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 137%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 138%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 139%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 4 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 140%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 141%positive => (1 * (s IDmdct_init48_k) + -3 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 142%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 143%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0)%Z
    | 144%positive => (1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 145%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -4 <= 0)%Z
    | 146%positive => (1 * (s IDmdct_init48_k) + -4 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 147%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 148%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 149%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -1 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0)%Z
    | 150%positive => (-1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -1 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 151%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0)%Z
    | 152%positive => (1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 32 <= 0)%Z
    | 153%positive => (-1 * (s IDmdct_init48_k) + 32 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0)%Z
    | 154%positive => (1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 32 <= 0)%Z
    | 155%positive => (-1 * (s IDmdct_init48_k) + 32 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0)%Z
    | 156%positive => (1 * (s IDmdct_init48_i) + -14 <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 32 <= 0)%Z
    | 157%positive => (-1 * (s IDmdct_init48_k) + 32 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0)%Z
    | 158%positive => (1 * (s IDmdct_init48_i) + -14 <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 32 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 159%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -31 <= 0)%Z
    | 160%positive => (1 * (s IDmdct_init48_k) + -31 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0)%Z
    | 161%positive => (-1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -31 <= 0)%Z
    | 162%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 2 <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0)%Z
    | 163%positive => (1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_k) + 2 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 164%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 2 <= 0 /\ 1 * (s IDmdct_init48_k) + -32 <= 0)%Z
    | 165%positive => (1 * (s IDmdct_init48_k) + -32 <= 0 /\ -1 * (s IDmdct_init48_k) + 2 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -15 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 166%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0)%Z
    | 167%positive => (1 * (s IDmdct_init48_k) + -6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0)%Z
    | 168%positive => (1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0)%Z
    | 169%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 170%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 171%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 172%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 173%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0)%Z
    | 174%positive => (-1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 175%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 176%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 177%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0)%Z
    | 178%positive => (1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 15 <= 0)%Z
    | 179%positive => (-1 * (s IDmdct_init48_k) + 15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0)%Z
    | 180%positive => (1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 15 <= 0)%Z
    | 181%positive => (-1 * (s IDmdct_init48_k) + 15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_i) + -13 <= 0)%Z
    | 182%positive => (1 * (s IDmdct_init48_i) + -13 <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 15 <= 0)%Z
    | 183%positive => (-1 * (s IDmdct_init48_k) + 15 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_i) + -13 <= 0)%Z
    | 184%positive => (1 * (s IDmdct_init48_i) + -13 <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 15 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 185%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -14 <= 0)%Z
    | 186%positive => (1 * (s IDmdct_init48_k) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 187%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -14 <= 0)%Z
    | 188%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0)%Z
    | 189%positive => (1 * (s IDmdct_init48_k) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 190%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -15 <= 0)%Z
    | 191%positive => (1 * (s IDmdct_init48_k) + -15 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_i) + -14 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 192%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0)%Z
    | 193%positive => (1 * (s IDmdct_init48_k) + -6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0)%Z
    | 194%positive => (1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -6 <= 0)%Z
    | 195%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 196%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 197%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 198%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_j) <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 199%positive => (False)%Z
    | 200%positive => (False)%Z
    | 201%positive => (False)%Z
    | 202%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -2 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 203%positive => (-1 * (s IDmdct_init48_j) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -2 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 204%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -2 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 205%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 206%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 6 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0)%Z
    | 207%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) + 6 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 208%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 209%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 210%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 211%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 7 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 212%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -2 <= 0)%Z
    | 213%positive => (1 * (s IDmdct_init48_k) + -2 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0)%Z
    | 214%positive => (1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -2 <= 0)%Z
    | 215%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 216%positive => (1 * (s IDmdct_init48_k) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 217%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -3 <= 0)%Z
    | 218%positive => (1 * (s IDmdct_init48_k) + -3 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -3 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 219%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -10 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 220%positive => (-1 * (s IDmdct_init48_j) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -10 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 9 <= 0)%Z
    | 221%positive => (-1 * (s IDmdct_init48_k) + 9 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0 /\ 1 * (s IDmdct_init48_j) + -10 <= 0 /\ -1 * (s IDmdct_init48_j) + 1 <= 0)%Z
    | 222%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 223%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0)%Z
    | 224%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 225%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 226%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 227%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 228%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 229%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 230%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0)%Z
    | 231%positive => (1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 232%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 233%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 234%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -9 <= 0)%Z
    | 235%positive => (1 * (s IDmdct_init48_k) + -9 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_j) + -11 <= 0 /\ -1 * (s IDmdct_init48_i) + 36 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 236%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 237%positive => (1 * (s IDmdct_init48_i) + -35 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0)%Z
    | 238%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 239%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 240%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 241%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 242%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 243%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 30 <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 244%positive => (1 * (s IDmdct_init48_i) + -35 <= 0 /\ -1 * (s IDmdct_init48_i) + 30 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 245%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 30 <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 246%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 31 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 247%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 31 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 248%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 31 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 249%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 31 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 250%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 24 <= 0 /\ 1 * (s IDmdct_init48_i) + -29 <= 0)%Z
    | 251%positive => (1 * (s IDmdct_init48_i) + -29 <= 0 /\ -1 * (s IDmdct_init48_i) + 24 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 252%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 24 <= 0 /\ 1 * (s IDmdct_init48_i) + -29 <= 0)%Z
    | 253%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 25 <= 0 /\ 1 * (s IDmdct_init48_i) + -30 <= 0)%Z
    | 254%positive => (1 * (s IDmdct_init48_i) + -30 <= 0 /\ -1 * (s IDmdct_init48_i) + 25 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 255%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 25 <= 0 /\ 1 * (s IDmdct_init48_i) + -30 <= 0)%Z
    | 256%positive => (1 * (s IDmdct_init48_i) + -30 <= 0 /\ -1 * (s IDmdct_init48_i) + 25 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 257%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 18 <= 0 /\ 1 * (s IDmdct_init48_i) + -23 <= 0)%Z
    | 258%positive => (1 * (s IDmdct_init48_i) + -23 <= 0 /\ -1 * (s IDmdct_init48_i) + 18 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 259%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) + 18 <= 0 /\ 1 * (s IDmdct_init48_i) + -23 <= 0)%Z
    | 260%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 19 <= 0 /\ 1 * (s IDmdct_init48_i) + -24 <= 0)%Z
    | 261%positive => (1 * (s IDmdct_init48_i) + -24 <= 0 /\ -1 * (s IDmdct_init48_i) + 19 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 262%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 19 <= 0 /\ 1 * (s IDmdct_init48_i) + -24 <= 0)%Z
    | 263%positive => (1 * (s IDmdct_init48_i) + -24 <= 0 /\ -1 * (s IDmdct_init48_i) + 19 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 264%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -17 <= 0)%Z
    | 265%positive => (1 * (s IDmdct_init48_i) + -17 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 266%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -17 <= 0)%Z
    | 267%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -18 <= 0)%Z
    | 268%positive => (1 * (s IDmdct_init48_i) + -18 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 269%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -18 <= 0)%Z
    | 270%positive => (1 * (s IDmdct_init48_i) + -18 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 271%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 272%positive => (1 * (s IDmdct_init48_i) + -35 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0)%Z
    | 273%positive => (-1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_i) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_i) + -35 <= 0)%Z
    | 274%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 275%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 276%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ 1 * (s IDmdct_init48_i) + -36 <= 0)%Z
    | 277%positive => (1 * (s IDmdct_init48_i) + -36 <= 0 /\ -1 * (s IDmdct_init48_i) + 1 <= 0 /\ -1 * (s IDmdct_init48_k) + 8 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | 278%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 279%positive => (1 * (s IDmdct_init48_k) + -7 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) <= 0)%Z
    | 280%positive => (-1 * (s IDmdct_init48_k) <= 0 /\ -1 * (s IDmdct_init48_z) <= 0 /\ 1 * (s IDmdct_init48_k) + -7 <= 0)%Z
    | 281%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 282%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) <= 0)%Z
    | 283%positive => (-1 * (s IDmdct_init48_z) <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ 1 * (s IDmdct_init48_k) + -8 <= 0)%Z
    | 284%positive => (1 * (s IDmdct_init48_k) + -8 <= 0 /\ -1 * (s IDmdct_init48_k) + 1 <= 0 /\ -1 * (s IDmdct_init48_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mdct_init48_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1171 # 1))%Q
    | 2%positive => ((1171 # 1) + (s IDmdct_init48_z))%Q
    | 3%positive => ((1171 # 1) + (s IDmdct_init48_z)
                     - max0((s IDmdct_init48_k)))%Q
    | 4%positive => ((1171 # 1) + (s IDmdct_init48_z)
                     - max0((s IDmdct_init48_k)))%Q
    | 5%positive => ((1171 # 1) + (s IDmdct_init48_z)
                     - max0((s IDmdct_init48_k)))%Q
    | 6%positive => ((1171 # 1) + (s IDmdct_init48_z)
                     - max0((s IDmdct_init48_k)))%Q
    | 7%positive => ((1171 # 1) + (s IDmdct_init48_z)
                     - max0((s IDmdct_init48_k)))%Q
    | 8%positive => ((1135 # 1) + (s IDmdct_init48_z)
                     + max0(36 - (s IDmdct_init48_i))
                     - max0((s IDmdct_init48_k)))%Q
    | 9%positive => ((1135 # 1) + (s IDmdct_init48_z)
                     + max0(36 - (s IDmdct_init48_i))
                     - max0((s IDmdct_init48_k)))%Q
    | 10%positive => ((1135 # 1) + (s IDmdct_init48_z)
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 11%positive => ((1135 # 1) + (s IDmdct_init48_z)
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 12%positive => ((1135 # 1) + (s IDmdct_init48_z)
                      - max0((s IDmdct_init48_k)))%Q
    | 13%positive => ((2261 # 2) - (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 14%positive => ((2261 # 2) - (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 15%positive => ((2261 # 2) - (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 16%positive => ((2261 # 2) - (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 17%positive => ((1120 # 1) + (s IDmdct_init48_z)
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 18%positive => ((1120 # 1) + (s IDmdct_init48_z)
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 19%positive => ((1120 # 1) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 20%positive => ((1120 # 1) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 21%positive => ((4461 # 4) + (1 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 22%positive => ((4461 # 4) + (1 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 23%positive => ((1115 # 1) + (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 24%positive => ((1115 # 1) + (3 # 4) * (s IDmdct_init48_i)
                      + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 25%positive => ((2251 # 2) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 26%positive => ((2251 # 2) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 27%positive => ((2251 # 2) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 28%positive => ((2251 # 2) + (s IDmdct_init48_z)
                      - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                      - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                      - max0(7 - (s IDmdct_init48_k))
                      + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                      - (1 # 4) * max0((s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 29%positive => ((1099 # 1) + (s IDmdct_init48_z)
                      - max0(7 - (s IDmdct_init48_k))
                      - max0((s IDmdct_init48_k)))%Q
    | 30%positive => ((1063 # 1) + (s IDmdct_init48_z)
                      - max0(7 - (s IDmdct_init48_k))
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 31%positive => ((1063 # 1) + (s IDmdct_init48_z)
                      - max0(7 - (s IDmdct_init48_k))
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 32%positive => ((1063 # 1) + (s IDmdct_init48_z)
                      - max0(7 - (s IDmdct_init48_k))
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 33%positive => ((1063 # 1) + (s IDmdct_init48_z)
                      - max0(7 - (s IDmdct_init48_k))
                      + max0(36 - (s IDmdct_init48_i))
                      - max0((s IDmdct_init48_k)))%Q
    | 34%positive => ((1055 # 1) + (s IDmdct_init48_z)
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 35%positive => ((865 # 1) - (19 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 36%positive => ((865 # 1) - (19 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 37%positive => ((865 # 1) - (19 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 38%positive => ((865 # 1) - (19 # 1) * (s IDmdct_init48_j)
                      - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 39%positive => ((865 # 1) - (19 # 1) * (s IDmdct_init48_j)
                      - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 40%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 41%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 42%positive => ((856 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 43%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 44%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 45%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 46%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 47%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 48%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 49%positive => ((847 # 1) + (s IDmdct_init48_z)
                      + max0(9 - (s IDmdct_init48_k))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 50%positive => ((847 # 1) + (s IDmdct_init48_z)
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 51%positive => ((847 # 1) + (s IDmdct_init48_z)
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 52%positive => ((847 # 1) + (s IDmdct_init48_z)
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 53%positive => ((1519 # 2) + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 54%positive => ((1519 # 2) + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 55%positive => ((1519 # 2) + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 56%positive => ((756 # 1) + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                      + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 57%positive => ((756 # 1) + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                      + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 58%positive => ((756 # 1) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                      + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 59%positive => ((756 # 1) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                      + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 60%positive => ((1513 # 2) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 61%positive => ((1507 # 2) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 62%positive => ((1507 # 2) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 63%positive => ((1507 # 2) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 64%positive => ((1507 # 2) + (7 # 1) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 65%positive => ((1577 # 2) + (7 # 2) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 66%positive => ((1577 # 2) + (7 # 2) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                      + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i)))%Q
    | 67%positive => ((792 # 1) + (7 # 2) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z) + max0(9 - (s IDmdct_init48_k))
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 68%positive => ((792 # 1) + (7 # 2) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 69%positive => ((792 # 1) + (7 # 2) * (s IDmdct_init48_j)
                      + (s IDmdct_init48_z)
                      + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                      - max0(35 - (s IDmdct_init48_i))
                      + max0(36 - (s IDmdct_init48_i))
                      + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 70%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 71%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 72%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 73%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 74%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 75%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 76%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 77%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 78%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 79%positive => ((2456 # 3) + (s IDmdct_init48_z)
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (2 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 80%positive => ((2456 # 3) + (s IDmdct_init48_z)
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (2 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 81%positive => ((819 # 1) + (s IDmdct_init48_z)
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 82%positive => ((819 # 1) + (s IDmdct_init48_z)
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 83%positive => ((819 # 1) + (s IDmdct_init48_z)
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 84%positive => ((579 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 85%positive => ((579 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 86%positive => ((580 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i)))%Q
    | 87%positive => ((580 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i)))%Q
    | 88%positive => ((580 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i)))%Q
    | 89%positive => ((572 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 90%positive => ((572 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 91%positive => ((572 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 92%positive => ((572 # 1) + (s IDmdct_init48_z)
                      + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                      + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 93%positive => ((77926 # 103) - (3746 # 61) * (s IDmdct_init48_k)
                      + (s IDmdct_init48_z)
                      + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (2 # 3) * max0(8 - (s IDmdct_init48_k)))%Q
    | 94%positive => ((21540 # 89) - (3746 # 61) * (s IDmdct_init48_k)
                      + (s IDmdct_init48_z)
                      + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (2 # 3) * max0(8 - (s IDmdct_init48_k))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 95%positive => ((21540 # 89) - (3746 # 61) * (s IDmdct_init48_k)
                      + (s IDmdct_init48_z)
                      + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                      + (2 # 3) * max0(8 - (s IDmdct_init48_k))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 96%positive => ((17959 # 74) - (3746 # 61) * (s IDmdct_init48_k)
                      + (s IDmdct_init48_z)
                      + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 97%positive => ((17959 # 74) - (3746 # 61) * (s IDmdct_init48_k)
                      + (s IDmdct_init48_z)
                      + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 98%positive => ((2163 # 37) + (s IDmdct_init48_z)
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 99%positive => ((2015 # 37) + (s IDmdct_init48_z)
                      + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                      + max0(4 - (s IDmdct_init48_k))
                      - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                      + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 100%positive => ((2015 # 37) + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 101%positive => ((2015 # 37) + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 102%positive => ((2015 # 37) + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 103%positive => ((57 # 1) + (s IDmdct_init48_z)
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 104%positive => ((57 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 105%positive => ((57 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 106%positive => ((57 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 107%positive => ((57 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 108%positive => ((21 # 1) + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 109%positive => ((7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 110%positive => ((7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 111%positive => ((s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 112%positive => ((s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 113%positive => ((s IDmdct_init48_z))%Q
    | 114%positive => ((s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 115%positive => ((s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 116%positive => (-(s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 117%positive => (-(s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 118%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 119%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 120%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 121%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 122%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 123%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 124%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 125%positive => ((6 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(3 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 126%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 127%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 128%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 129%positive => ((8 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 130%positive => ((8 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 131%positive => ((8 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 132%positive => ((7 # 1) - (s IDmdct_init48_m) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(2 - (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 133%positive => ((57 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 134%positive => ((57 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 135%positive => ((57 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 136%positive => ((58 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 137%positive => ((58 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 138%positive => ((58 # 1) - (s IDmdct_init48_i)
                       + max0(4 - (s IDmdct_init48_k))
                       + max0((s IDmdct_init48_z)))%Q
    | 139%positive => ((58 # 1) - (s IDmdct_init48_i)
                       + max0(-1 + (s IDmdct_init48_z))
                       + max0(4 - (s IDmdct_init48_k)))%Q
    | 140%positive => ((2015 # 37) + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(4 - (s IDmdct_init48_k))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 141%positive => ((2163 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 142%positive => ((2163 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 143%positive => ((2200 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 144%positive => ((2200 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 145%positive => ((2200 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 146%positive => ((2163 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 147%positive => ((17959 # 74) - (3746 # 61) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (3746 # 61) * max0(-3 + (s IDmdct_init48_k))
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 148%positive => ((3253 # 37) + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 149%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 150%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 151%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 152%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 153%positive => (-(94 # 37) + (94 # 37) * (s IDmdct_init48_i)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       - (94 # 37) * max0(36 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 154%positive => (-(94 # 37) + (94 # 37) * (s IDmdct_init48_i)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       - (94 # 37) * max0(36 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 155%positive => ((94 # 37) * (s IDmdct_init48_i) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 156%positive => ((94 # 37) * (s IDmdct_init48_i) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 157%positive => ((94 # 37) * (s IDmdct_init48_i) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 158%positive => (-(1 # 1) + (94 # 37) * (s IDmdct_init48_i)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (32 # 1) * max0(1 + (s IDmdct_init48_i))
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (188 # 37) * max0(36 - (s IDmdct_init48_i)))%Q
    | 159%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 160%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 161%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 162%positive => ((3327 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 163%positive => ((3327 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 164%positive => ((3327 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 165%positive => ((3290 # 37) - (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       - (94 # 37) * max0(35 - (s IDmdct_init48_i))
                       + (94 # 37) * max0(37 - (s IDmdct_init48_i))
                       + (32 # 1) * max0((s IDmdct_init48_i)))%Q
    | 166%positive => ((572 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 167%positive => ((575 # 1) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k)))%Q
    | 168%positive => ((575 # 1) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k)))%Q
    | 169%positive => ((1726 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 170%positive => ((1726 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 171%positive => ((1726 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 172%positive => ((1723 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 173%positive => ((580 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i)))%Q
    | 174%positive => ((580 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i)))%Q
    | 175%positive => ((565 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 176%positive => ((565 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 177%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 178%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 179%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 180%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 181%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 182%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 183%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 184%positive => ((580 # 1) + (s IDmdct_init48_z)
                       + (16 # 1) * max0(1 + (s IDmdct_init48_i))
                       + max0(15 - (s IDmdct_init48_k)))%Q
    | 185%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 186%positive => ((582 # 1) + (s IDmdct_init48_z)
                       + max0(14 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 187%positive => ((582 # 1) + (s IDmdct_init48_z)
                       + max0(14 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 188%positive => ((582 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 189%positive => ((582 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 190%positive => ((582 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 191%positive => ((581 # 1) + (s IDmdct_init48_z)
                       + max0(15 - (s IDmdct_init48_k))
                       + (16 # 1) * max0((s IDmdct_init48_i)))%Q
    | 192%positive => ((819 # 1) + (s IDmdct_init48_z)
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 193%positive => ((822 # 1) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k)))%Q
    | 194%positive => ((822 # 1) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(7 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k)))%Q
    | 195%positive => ((2467 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 196%positive => ((2467 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 197%positive => ((2467 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 198%positive => ((2464 # 3) - (1 # 3) * (s IDmdct_init48_k)
                       + (s IDmdct_init48_z)
                       + (1 # 3) * max0(8 - (s IDmdct_init48_k))
                       + (1 # 3) * max0(9 - (s IDmdct_init48_k)))%Q
    | 199%positive => ((827 # 1) + (s IDmdct_init48_z))%Q
    | 200%positive => ((828 # 1) + (s IDmdct_init48_z))%Q
    | 201%positive => ((828 # 1) + (s IDmdct_init48_z))%Q
    | 202%positive => ((792 # 1) + (7 # 2) * (s IDmdct_init48_j)
                       + (s IDmdct_init48_z)
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 203%positive => ((1521 # 2) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 204%positive => ((1521 # 2) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (7 # 1) * max0((s IDmdct_init48_j)))%Q
    | 205%positive => ((1507 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 206%positive => ((1525 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 207%positive => ((1525 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 208%positive => ((1527 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 209%positive => ((1527 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 210%positive => ((1527 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 211%positive => ((1525 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 212%positive => ((756 # 1) + (7 # 1) * (s IDmdct_init48_j)
                       + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                       + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 213%positive => ((758 # 1) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 214%positive => ((758 # 1) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(3 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 215%positive => ((1517 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 216%positive => ((1517 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 217%positive => ((1517 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 218%positive => ((1515 # 2) + (7 # 1) * (s IDmdct_init48_j)
                       - (1 # 2) * (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (7 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + (1 # 2) * max0(4 - (s IDmdct_init48_k))
                       + (7 # 2) * max0(10 - (s IDmdct_init48_j))
                       + (7 # 2) * max0(11 - (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 219%positive => ((847 # 1) + (s IDmdct_init48_z)
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 220%positive => ((866 # 1) - (19 # 1) * (s IDmdct_init48_j)
                       + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 221%positive => ((866 # 1) - (19 # 1) * (s IDmdct_init48_j)
                       + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i))
                       + (19 # 1) * max0((s IDmdct_init48_j)))%Q
    | 222%positive => ((847 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 223%positive => ((848 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(8 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 224%positive => ((848 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(8 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 225%positive => ((848 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 226%positive => ((848 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 227%positive => ((848 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 228%positive => ((847 # 1) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       + max0(9 - (s IDmdct_init48_k))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 229%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 230%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 231%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 232%positive => ((866 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 233%positive => ((866 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 234%positive => ((866 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 235%positive => ((865 # 1) - (s IDmdct_init48_k) + (s IDmdct_init48_z)
                       + (19 # 1) * max0(-1 + (s IDmdct_init48_j))
                       - max0(35 - (s IDmdct_init48_i))
                       + max0(36 - (s IDmdct_init48_i)))%Q
    | 236%positive => ((1063 # 1) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 237%positive => ((1099 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 238%positive => ((1099 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 239%positive => ((1100 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 240%positive => ((1100 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 241%positive => ((1100 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 242%positive => ((1099 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0(7 - (s IDmdct_init48_k))
                       - max0((s IDmdct_init48_k)))%Q
    | 243%positive => ((2251 # 2) + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 244%positive => ((2261 # 2) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 245%positive => ((2261 # 2) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 246%positive => ((1131 # 1) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 247%positive => ((1131 # 1) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 248%positive => ((1131 # 1) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 249%positive => ((1130 # 1) - (1 # 2) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 250%positive => ((1115 # 1) + (3 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-19 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 251%positive => ((2249 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 252%positive => ((2249 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - (1 # 4) * max0((s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 253%positive => ((2249 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 254%positive => ((2249 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 255%positive => ((2249 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 256%positive => ((2247 # 2) + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - (1 # 4) * max0(-1 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 257%positive => ((1120 # 1) + (s IDmdct_init48_z)
                       - (1 # 4) * max0(-18 + (s IDmdct_init48_i))
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 258%positive => ((2249 # 2) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 259%positive => ((2249 # 2) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-14 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(17 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 260%positive => ((4499 # 4) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 261%positive => ((4499 # 4) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 262%positive => ((4499 # 4) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 263%positive => ((4495 # 4) - (1 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       - (3 # 4) * max0(-15 + (s IDmdct_init48_i))
                       - max0(7 - (s IDmdct_init48_k))
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 264%positive => ((2261 # 2) - (3 # 4) * (s IDmdct_init48_i)
                       + (s IDmdct_init48_z)
                       + (1 # 4) * max0(18 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 265%positive => ((1135 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 266%positive => ((1135 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 267%positive => ((1136 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 268%positive => ((1136 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 269%positive => ((1136 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 270%positive => ((1135 # 1) - (s IDmdct_init48_i) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 271%positive => ((1135 # 1) + (s IDmdct_init48_z)
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 272%positive => ((1136 # 1) + (s IDmdct_init48_z)
                       + max0(35 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 273%positive => ((1136 # 1) + (s IDmdct_init48_z)
                       + max0(35 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 274%positive => ((1136 # 1) + (s IDmdct_init48_z)
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 275%positive => ((1136 # 1) + (s IDmdct_init48_z)
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 276%positive => ((1136 # 1) + (s IDmdct_init48_z)
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 277%positive => ((1135 # 1) + (s IDmdct_init48_z)
                       + max0(36 - (s IDmdct_init48_i))
                       - max0((s IDmdct_init48_k)))%Q
    | 278%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 279%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 280%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0((s IDmdct_init48_k)))%Q
    | 281%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0(-1 + (s IDmdct_init48_k)))%Q
    | 282%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0(-1 + (s IDmdct_init48_k)))%Q
    | 283%positive => ((1171 # 1) + (s IDmdct_init48_z)
                       - max0(-1 + (s IDmdct_init48_k)))%Q
    | 284%positive => ((1170 # 1) + (s IDmdct_init48_z)
                       - max0(-1 + (s IDmdct_init48_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mdct_init48_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDmdct_init48_i)) (35
                                                                    - (s IDmdct_init48_i)));
                      (*-1 0*) F_max0_ge_0 (35 - (s IDmdct_init48_i))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*0 0.75*) F_binom_monotonic 1 (F_max0_ge_arg (-14
                                                                    + 
                                                                    (s IDmdct_init48_i))) (F_check_ge (-14
                                                                    + (s IDmdct_init48_i)) (0))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_ge_0 (7 - (s IDmdct_init48_k));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 (-18
                                                                    + 
                                                                    (s IDmdct_init48_i))) (F_check_ge (0) (0))]
    | 19%positive => []
    | 20%positive => [(*0 0.25*) F_binom_monotonic 1 (F_max0_ge_arg (-19
                                                                    + 
                                                                    (s IDmdct_init48_i))) (F_check_ge (-19
                                                                    + (s IDmdct_init48_i)) (0))]
    | 21%positive => []
    | 22%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (18
                                                                - (s IDmdct_init48_i)) (17
                                                                    - (s IDmdct_init48_i)));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_i))) (F_check_ge ((s IDmdct_init48_i)) (0));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-1
                                                                    + (s IDmdct_init48_i)) (0))]
    | 23%positive => []
    | 24%positive => [(*0 0.75*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-14
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-14
                                                                    + (s IDmdct_init48_i)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-0.25 0*) F_max0_ge_0 (17 - (s IDmdct_init48_i));
                      (*-1 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDmdct_init48_i)) (35
                                                                    - (s IDmdct_init48_i)));
                      (*-1 0*) F_max0_ge_0 (35 - (s IDmdct_init48_i));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmdct_init48_i)) (0))) (F_max0_ge_0 ((s IDmdct_init48_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (36
                                                                    - (s IDmdct_init48_i)));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_i)));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-18
                                                                    + (s IDmdct_init48_i)));
                      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-19
                                                                    + (s IDmdct_init48_i)))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_pre_decrement (9 - (s IDmdct_init48_k)) (1);
                      (*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDmdct_init48_k)) (7
                                                                    - (s IDmdct_init48_k)));
                      (*-1 0*) F_max0_ge_0 (35 - (s IDmdct_init48_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmdct_init48_k)) (0))) (F_max0_ge_0 ((s IDmdct_init48_k)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDmdct_init48_k)))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-19 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_j))) (F_check_ge ((s IDmdct_init48_j)) (0))]
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_ge_0 (9 - (s IDmdct_init48_k));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDmdct_init48_k)))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*0 1*) F_max0_ge_0 (9 - (s IDmdct_init48_k))]
    | 50%positive => []
    | 51%positive => [(*-19 0*) F_max0_monotonic (F_check_ge ((s IDmdct_init48_j)) (-1
                                                                    + (s IDmdct_init48_j)));
                      (*-19 0*) F_max0_ge_0 (-1 + (s IDmdct_init48_j))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-7 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_j))) (F_check_ge ((s IDmdct_init48_j)) (0))]
    | 58%positive => []
    | 59%positive => [(*-0.5 0*) F_max0_pre_decrement (4
                                                       - (s IDmdct_init48_k)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                 - (s IDmdct_init48_k))) (F_check_ge (0) (0))]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => [(*-3.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (10
                                                                    - 
                                                                    (s IDmdct_init48_j))) (F_check_ge (10
                                                                    - (s IDmdct_init48_j)) (0))]
    | 65%positive => []
    | 66%positive => []
    | 67%positive => [(*0 1*) F_max0_monotonic (F_check_ge (9
                                                            - (s IDmdct_init48_k)) (8
                                                                    - (s IDmdct_init48_k)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                - (s IDmdct_init48_k))) (F_check_ge (0) (0))]
    | 68%positive => []
    | 69%positive => [(*-7 0*) F_max0_monotonic (F_check_ge ((s IDmdct_init48_j)) (-1
                                                                    + (s IDmdct_init48_j)));
                      (*-7 0*) F_max0_ge_0 (-1 + (s IDmdct_init48_j));
                      (*-1 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDmdct_init48_i)) (35
                                                                    - (s IDmdct_init48_i)));
                      (*-3.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (10
                                                                    - 
                                                                    (s IDmdct_init48_j))) (F_check_ge (10
                                                                    - (s IDmdct_init48_j)) (0))]
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
    | 80%positive => [(*-0.333333 0*) F_max0_pre_decrement (9
                                                            - (s IDmdct_init48_k)) (1)]
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (7
                                                             - (s IDmdct_init48_k)) (6
                                                                    - (s IDmdct_init48_k)));
                      (*-1 0*) F_max0_ge_0 (6 - (s IDmdct_init48_k));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDmdct_init48_k))) (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDmdct_init48_k))) (F_check_ge (8
                                                                    - (s IDmdct_init48_k)) (0));
                      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDmdct_init48_k)))]
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => [(*0 0.333333*) F_max0_pre_decrement (9
                                                           - (s IDmdct_init48_k)) (1);
                      (*-16 0*) F_max0_monotonic (F_check_ge (1
                                                              + (s IDmdct_init48_i)) ((s IDmdct_init48_i)));
                      (*-16 0*) F_max0_ge_0 ((s IDmdct_init48_i));
                      (*0 61.4099*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDmdct_init48_k)))]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => [(*-0.666667 0*) F_max0_pre_decrement (8
                                                            - (s IDmdct_init48_k)) (1);
                      (*-1 0*) F_max0_monotonic (F_check_ge (7
                                                             - (s IDmdct_init48_k)) (6
                                                                    - (s IDmdct_init48_k)));
                      (*-1 0*) F_max0_ge_0 (6 - (s IDmdct_init48_k))]
    | 96%positive => []
    | 97%positive => [(*-61.4099 0*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDmdct_init48_k))) (F_check_ge (-3
                                                                    + (s IDmdct_init48_k)) (0))]
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => [(*-32 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDmdct_init48_i)) ((s IDmdct_init48_i)));
                       (*-32 0*) F_max0_ge_0 ((s IDmdct_init48_i));
                       (*-2.54054 0*) F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - (s IDmdct_init48_i))) (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0));
                       (*-2.54054 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (35
                                                                    - (s IDmdct_init48_i)))]
    | 103%positive => []
    | 104%positive => []
    | 105%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmdct_init48_z)) (0))) (F_max0_ge_0 ((s IDmdct_init48_z)))]
    | 106%positive => []
    | 107%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (36
                                                              - (s IDmdct_init48_i)) (35
                                                                    - (s IDmdct_init48_i)));
                       (*-1 0*) F_max0_ge_0 (35 - (s IDmdct_init48_i));
                       (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (36
                                                                    - (s IDmdct_init48_i)))]
    | 108%positive => []
    | 109%positive => []
    | 110%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_z))) (F_check_ge ((s IDmdct_init48_z)) (0))]
    | 111%positive => []
    | 112%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                              - (s IDmdct_init48_k)) (2
                                                                    - (s IDmdct_init48_k)));
                       (*-7 0*) F_max0_ge_0 (3 - (s IDmdct_init48_i));
                       (*-1 0*) F_max0_monotonic (F_check_ge (4
                                                              - (s IDmdct_init48_k)) (3
                                                                    - (s IDmdct_init48_k)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                  - (s IDmdct_init48_k))) (F_check_ge (0) (0))]
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => [(*-7 0*) F_max0_pre_decrement (3 - (s IDmdct_init48_i)) (1)]
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => [(*0 1*) F_max0_monotonic (F_check_ge (6
                                                             - (s IDmdct_init48_m)) (5
                                                                    - (s IDmdct_init48_m)));
                       (*-1 0*) F_max0_ge_0 (5 - (s IDmdct_init48_m));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - (s IDmdct_init48_m)) (0))) (F_max0_ge_0 (6
                                                                    - (s IDmdct_init48_m)))]
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
    | 139%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmdct_init48_z)) (0))) (F_max0_ge_0 ((s IDmdct_init48_z)));
                       (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDmdct_init48_z))) (F_check_ge (-1
                                                                    + (s IDmdct_init48_z)) (0))]
    | 140%positive => [(*0 1*) F_max0_pre_decrement (4 - (s IDmdct_init48_k)) (1);
                       (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                   - 
                                                                   (s IDmdct_init48_k))) (F_check_ge (3
                                                                    - (s IDmdct_init48_k)) (0))]
    | 141%positive => []
    | 142%positive => []
    | 143%positive => []
    | 144%positive => []
    | 145%positive => []
    | 146%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDmdct_init48_k)))]
    | 147%positive => [(*0 32*) F_max0_pre_decrement (1 + (s IDmdct_init48_i)) (1);
                       (*0 2.54054*) F_max0_pre_increment (36
                                                           - (s IDmdct_init48_i)) (1);
                       (*0 61.4099*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + (s IDmdct_init48_k))) (F_check_ge (-3
                                                                    + (s IDmdct_init48_k)) (0))]
    | 148%positive => []
    | 149%positive => []
    | 150%positive => []
    | 151%positive => []
    | 152%positive => [(*-2.54054 0*) F_max0_pre_increment (36
                                                            - (s IDmdct_init48_i)) (1);
                       (*0 2.54054*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (35
                                                                    - (s IDmdct_init48_i)))]
    | 153%positive => []
    | 154%positive => []
    | 155%positive => []
    | 156%positive => []
    | 157%positive => []
    | 158%positive => [(*0 1*) F_max0_monotonic (F_check_ge (32
                                                             - (s IDmdct_init48_k)) (31
                                                                    - (s IDmdct_init48_k)));
                       (*-1 0*) F_max0_ge_0 (31 - (s IDmdct_init48_k));
                       (*-2.54054 0*) F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - (s IDmdct_init48_i))) (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (32
                                                                    - (s IDmdct_init48_k)));
                       (*-61.4099 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDmdct_init48_k)))]
    | 159%positive => []
    | 160%positive => []
    | 161%positive => []
    | 162%positive => []
    | 163%positive => []
    | 164%positive => []
    | 165%positive => []
    | 166%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDmdct_init48_k))) (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))]
    | 167%positive => []
    | 168%positive => []
    | 169%positive => []
    | 170%positive => []
    | 171%positive => []
    | 172%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDmdct_init48_k)))]
    | 173%positive => []
    | 174%positive => []
    | 175%positive => []
    | 176%positive => [(*-16 0*) F_max0_pre_decrement (1
                                                       + (s IDmdct_init48_i)) (1)]
    | 177%positive => []
    | 178%positive => []
    | 179%positive => []
    | 180%positive => []
    | 181%positive => []
    | 182%positive => []
    | 183%positive => []
    | 184%positive => [(*0 1*) F_max0_monotonic (F_check_ge (15
                                                             - (s IDmdct_init48_k)) (14
                                                                    - (s IDmdct_init48_k)));
                       (*-1 0*) F_max0_ge_0 (14 - (s IDmdct_init48_k))]
    | 185%positive => [(*-1 0*) F_max0_pre_decrement (15
                                                      - (s IDmdct_init48_k)) (1)]
    | 186%positive => []
    | 187%positive => []
    | 188%positive => []
    | 189%positive => []
    | 190%positive => []
    | 191%positive => []
    | 192%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDmdct_init48_k))) (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))]
    | 193%positive => []
    | 194%positive => []
    | 195%positive => []
    | 196%positive => []
    | 197%positive => []
    | 198%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDmdct_init48_k)))]
    | 199%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDmdct_init48_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDmdct_init48_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDmdct_init48_z)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDmdct_init48_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_z)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_z)))]
    | 200%positive => []
    | 201%positive => []
    | 202%positive => [(*-3.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                                    - (s IDmdct_init48_j)) (0))) (F_max0_ge_0 (11
                                                                    - (s IDmdct_init48_j)));
                       (*-7 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_j)))]
    | 203%positive => []
    | 204%positive => []
    | 205%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - 
                                                                    (s IDmdct_init48_k))) (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))]
    | 206%positive => []
    | 207%positive => []
    | 208%positive => []
    | 209%positive => []
    | 210%positive => []
    | 211%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDmdct_init48_k)))]
    | 212%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDmdct_init48_k))) (F_check_ge (4
                                                                    - (s IDmdct_init48_k)) (0))]
    | 213%positive => []
    | 214%positive => []
    | 215%positive => []
    | 216%positive => []
    | 217%positive => []
    | 218%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDmdct_init48_k)))]
    | 219%positive => [(*0 19*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_j)))]
    | 220%positive => []
    | 221%positive => []
    | 222%positive => [(*-1 0*) F_max0_pre_decrement (9 - (s IDmdct_init48_k)) (1)]
    | 223%positive => []
    | 224%positive => []
    | 225%positive => []
    | 226%positive => []
    | 227%positive => []
    | 228%positive => []
    | 229%positive => []
    | 230%positive => []
    | 231%positive => []
    | 232%positive => []
    | 233%positive => []
    | 234%positive => []
    | 235%positive => []
    | 236%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    (s IDmdct_init48_i))) (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0))]
    | 237%positive => []
    | 238%positive => []
    | 239%positive => []
    | 240%positive => []
    | 241%positive => []
    | 242%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (36
                                                                    - (s IDmdct_init48_i)))]
    | 243%positive => [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_i)));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-19
                                                                    + (s IDmdct_init48_i)))]
    | 244%positive => []
    | 245%positive => []
    | 246%positive => []
    | 247%positive => []
    | 248%positive => []
    | 249%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (18
                                                                 - (s IDmdct_init48_i)) (17
                                                                    - (s IDmdct_init48_i)));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_i))) (F_check_ge ((s IDmdct_init48_i)) (0));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0))]
    | 250%positive => [(*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_i)));
                       (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-18
                                                                    + (s IDmdct_init48_i)));
                       (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-19
                                                                    + (s IDmdct_init48_i)))]
    | 251%positive => []
    | 252%positive => []
    | 253%positive => []
    | 254%positive => []
    | 255%positive => []
    | 256%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (18
                                                                 - (s IDmdct_init48_i)) (17
                                                                    - (s IDmdct_init48_i)));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_i))) (F_check_ge ((s IDmdct_init48_i)) (0));
                       (*-0.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-14
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-14
                                                                    + (s IDmdct_init48_i)) (0));
                       (*-0.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-15
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-15
                                                                    + (s IDmdct_init48_i)));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-19
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-19
                                                                    + (s IDmdct_init48_i)) (0))]
    | 257%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (18
                                                                 - (s IDmdct_init48_i)) (17
                                                                    - (s IDmdct_init48_i)));
                       (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-18
                                                                    + (s IDmdct_init48_i)))]
    | 258%positive => []
    | 259%positive => []
    | 260%positive => []
    | 261%positive => []
    | 262%positive => []
    | 263%positive => [(*-0.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-14
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-14
                                                                    + (s IDmdct_init48_i)) (0));
                       (*-0.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-15
                                                                    + (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (-15
                                                                    + (s IDmdct_init48_i)));
                       (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18
                                                                    + (s IDmdct_init48_i))) (F_check_ge (-18
                                                                    + (s IDmdct_init48_i)) (0))]
    | 264%positive => [(*0 0.25*) F_binom_monotonic 1 (F_max0_ge_arg (18
                                                                    - (s IDmdct_init48_i))) (F_check_ge (18
                                                                    - (s IDmdct_init48_i)) (0))]
    | 265%positive => []
    | 266%positive => []
    | 267%positive => []
    | 268%positive => []
    | 269%positive => []
    | 270%positive => [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                                    - (s IDmdct_init48_i)) (0))) (F_max0_ge_0 (18
                                                                    - (s IDmdct_init48_i)))]
    | 271%positive => [(*-1 0*) F_max0_pre_decrement (36
                                                      - (s IDmdct_init48_i)) (1)]
    | 272%positive => []
    | 273%positive => []
    | 274%positive => []
    | 275%positive => []
    | 276%positive => []
    | 277%positive => []
    | 278%positive => []
    | 279%positive => []
    | 280%positive => []
    | 281%positive => []
    | 282%positive => []
    | 283%positive => []
    | 284%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmdct_init48_k))) (F_check_ge ((s IDmdct_init48_k)) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmdct_init48_k)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmdct_init48_k)))]
    | _ => []
  end.


Theorem mdct_init48_ai_correct:
  forall s p' s', steps (g_start mdct_init48) s (g_edges mdct_init48) p' s' -> mdct_init48_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mdct_init48_pot_correct:
  forall s p' s',
    steps (g_start mdct_init48) s (g_edges mdct_init48) p' s' ->
    (mdct_init48_pot (g_start mdct_init48) s >= mdct_init48_pot p' s')%Q.
Proof.
  check_lp mdct_init48_ai_correct mdct_init48_hints.
Qed.

