Require Import pasta.Pasta.

Notation IDparse_switches_z := 1%positive.
Notation IDparse_switches__tmp := 2%positive.
Notation IDparse_switches__tmp1 := 3%positive.
Notation IDparse_switches__tmp2 := 4%positive.
Notation IDparse_switches_argn := 5%positive.
Notation IDparse_switches_force_baseline := 6%positive.
Notation IDparse_switches_is_targa := 7%positive.
Notation IDparse_switches_parse_switches.printed_version := 8%positive.
Notation IDparse_switches_q_scale_factor := 9%positive.
Notation IDparse_switches_simple_progressive := 10%positive.
Notation IDparse_switches_argc := 11%positive.
Notation IDparse_switches_argv := 12%positive.
Notation IDparse_switches_cinfo := 13%positive.
Notation IDparse_switches_for_real := 14%positive.
Notation IDparse_switches_last_file_arg_seen := 15%positive.
Definition parse_switches : graph := {|
  g_start := 1%positive;
  g_end := 253%positive;
  g_edges := (1%positive,(AAssign IDparse_switches_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDparse_switches__tmp
             (Some (EVar IDparse_switches_argc))),3%positive)::
             (3%positive,(AAssign IDparse_switches__tmp1
             (Some (EVar IDparse_switches_last_file_arg_seen))),4%positive)::
             (4%positive,(AAssign IDparse_switches__tmp2
             (Some (EVar IDparse_switches_for_real))),5%positive)::
             (5%positive,(AAssign IDparse_switches_q_scale_factor
             (Some (ENum (100)))),6%positive)::
             (6%positive,(AAssign IDparse_switches_force_baseline
             (Some (ENum (0)))),7%positive)::
             (7%positive,(AAssign IDparse_switches_simple_progressive
             (Some (ENum (0)))),8%positive)::
             (8%positive,(AAssign IDparse_switches_is_targa
             (Some (ENum (0)))),9%positive)::
             (9%positive,(AAssign IDparse_switches_argn (Some (ENum (1)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) <
             (eval (EVar IDparse_switches__tmp) s))%Z)),14%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) >=
             (eval (EVar IDparse_switches__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,217%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,212%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,210%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,206%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,186%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,178%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,177%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,175%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,175%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,158%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,156%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,156%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,148%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,145%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,133%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,125%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,117%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,93%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,85%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,77%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,60%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,57%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,59%positive)::
             (57%positive,(AAssign IDparse_switches_is_targa
             (Some (ENum (1)))),58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,ANone,76%positive)::
             (60%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             61%positive)::(61%positive,AWeaken,62%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),64%positive)::
             (62%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),63%positive)::
             (63%positive,AWeaken,67%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,69%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,71%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,74%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,74%positive)::
             (73%positive,ANone,75%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,ANone,84%positive)::
             (77%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             78%positive)::(78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),81%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),80%positive)::
             (80%positive,AWeaken,83%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,ANone,84%positive)::
             (84%positive,ANone,92%positive)::
             (85%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             86%positive)::(86%positive,AWeaken,87%positive)::
             (87%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),89%positive)::
             (87%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),88%positive)::
             (88%positive,AWeaken,91%positive)::
             (89%positive,AWeaken,90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,ANone,116%positive)::
             (93%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             94%positive)::(94%positive,AWeaken,95%positive)::
             (95%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),97%positive)::
             (95%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),96%positive)::
             (96%positive,AWeaken,100%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,ANone,99%positive)::
             (99%positive,AWeaken,100%positive)::
             (100%positive,ANone,102%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,AWeaken,104%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,AWeaken,104%positive)::
             (104%positive,ANone,108%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,ANone,108%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,AWeaken,110%positive)::
             (108%positive,ANone,109%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,ANone,114%positive)::
             (110%positive,ANone,111%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,ANone,114%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,ANone,115%positive)::
             (114%positive,ANone,115%positive)::
             (115%positive,ANone,116%positive)::
             (116%positive,ANone,124%positive)::
             (117%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             118%positive)::(118%positive,AWeaken,119%positive)::
             (119%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),121%positive)::
             (119%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),120%positive)::
             (120%positive,AWeaken,123%positive)::
             (121%positive,AWeaken,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,ANone,132%positive)::
             (125%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             126%positive)::(126%positive,AWeaken,127%positive)::
             (127%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),129%positive)::
             (127%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),128%positive)::
             (128%positive,AWeaken,131%positive)::
             (129%positive,AWeaken,130%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,ANone,132%positive)::
             (132%positive,ANone,144%positive)::
             (133%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             134%positive)::(134%positive,AWeaken,135%positive)::
             (135%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),137%positive)::
             (135%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),136%positive)::
             (136%positive,AWeaken,140%positive)::
             (137%positive,AWeaken,138%positive)::
             (138%positive,ANone,139%positive)::
             (139%positive,AWeaken,140%positive)::
             (140%positive,ANone,141%positive)::
             (140%positive,ANone,142%positive)::
             (141%positive,ANone,142%positive)::
             (142%positive,(AAssign IDparse_switches_q_scale_factor None),
             143%positive)::(143%positive,ANone,144%positive)::
             (144%positive,ANone,147%positive)::
             (145%positive,(AAssign IDparse_switches_simple_progressive
             (Some (ENum (1)))),146%positive)::
             (146%positive,ANone,147%positive)::
             (147%positive,ANone,155%positive)::
             (148%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             149%positive)::(149%positive,AWeaken,150%positive)::
             (150%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),152%positive)::
             (150%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),151%positive)::
             (151%positive,AWeaken,154%positive)::
             (152%positive,AWeaken,153%positive)::
             (153%positive,ANone,154%positive)::
             (154%positive,ANone,155%positive)::
             (155%positive,ANone,157%positive)::
             (156%positive,ANone,157%positive)::
             (157%positive,ANone,174%positive)::
             (158%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             159%positive)::(159%positive,AWeaken,160%positive)::
             (160%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),162%positive)::
             (160%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),161%positive)::
             (161%positive,AWeaken,165%positive)::
             (162%positive,AWeaken,163%positive)::
             (163%positive,ANone,164%positive)::
             (164%positive,AWeaken,165%positive)::
             (165%positive,ANone,167%positive)::
             (165%positive,ANone,166%positive)::
             (166%positive,AWeaken,169%positive)::
             (167%positive,ANone,168%positive)::
             (168%positive,AWeaken,169%positive)::
             (169%positive,ANone,172%positive)::
             (169%positive,ANone,170%positive)::
             (170%positive,AWeaken,171%positive)::
             (171%positive,ANone,172%positive)::
             (171%positive,ANone,173%positive)::
             (172%positive,ANone,173%positive)::
             (173%positive,ANone,174%positive)::
             (174%positive,ANone,176%positive)::
             (175%positive,ANone,176%positive)::
             (176%positive,ANone,185%positive)::
             (177%positive,AWeaken,179%positive)::
             (178%positive,AWeaken,179%positive)::
             (179%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_parse_switches.printed_version)
             s) <> (eval (ENum (0)) s))%Z)),183%positive)::
             (179%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_parse_switches.printed_version)
             s) = (eval (ENum (0)) s))%Z)),180%positive)::
             (180%positive,AWeaken,181%positive)::
             (181%positive,(AAssign
             IDparse_switches_parse_switches.printed_version
             (Some (ENum (1)))),182%positive)::
             (182%positive,ANone,184%positive)::
             (183%positive,AWeaken,184%positive)::
             (184%positive,ANone,185%positive)::
             (185%positive,ANone,205%positive)::
             (186%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             187%positive)::(187%positive,AWeaken,188%positive)::
             (188%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),190%positive)::
             (188%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),189%positive)::
             (189%positive,AWeaken,193%positive)::
             (190%positive,AWeaken,191%positive)::
             (191%positive,ANone,192%positive)::
             (192%positive,AWeaken,193%positive)::
             (193%positive,ANone,203%positive)::
             (193%positive,ANone,194%positive)::
             (194%positive,AWeaken,195%positive)::
             (195%positive,ANone,201%positive)::
             (195%positive,ANone,196%positive)::
             (196%positive,AWeaken,197%positive)::
             (197%positive,ANone,199%positive)::
             (197%positive,ANone,198%positive)::
             (198%positive,ANone,200%positive)::
             (199%positive,ANone,200%positive)::
             (200%positive,ANone,202%positive)::
             (201%positive,ANone,202%positive)::
             (202%positive,ANone,204%positive)::
             (203%positive,ANone,204%positive)::
             (204%positive,ANone,205%positive)::
             (205%positive,ANone,208%positive)::
             (206%positive,(AAssign IDparse_switches_force_baseline
             (Some (ENum (1)))),207%positive)::
             (207%positive,ANone,208%positive)::
             (208%positive,ANone,209%positive)::
             (209%positive,ANone,256%positive)::
             (210%positive,ANone,211%positive)::
             (211%positive,AWeaken,253%positive)::
             (212%positive,AWeaken,213%positive)::
             (213%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) <=
             (eval (EVar IDparse_switches__tmp1) s))%Z)),254%positive)::
             (213%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) >
             (eval (EVar IDparse_switches__tmp1) s))%Z)),214%positive)::
             (214%positive,AWeaken,215%positive)::
             (215%positive,ANone,216%positive)::
             (216%positive,AWeaken,217%positive)::
             (217%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches__tmp2) s) <>
             (eval (ENum (0)) s))%Z)),219%positive)::
             (217%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches__tmp2) s) =
             (eval (ENum (0)) s))%Z)),218%positive)::
             (218%positive,AWeaken,253%positive)::
             (219%positive,AWeaken,220%positive)::
             (220%positive,ANone,222%positive)::
             (220%positive,ANone,221%positive)::
             (221%positive,AWeaken,227%positive)::
             (222%positive,AWeaken,223%positive)::
             (223%positive,ANone,225%positive)::
             (223%positive,ANone,224%positive)::
             (224%positive,ANone,225%positive)::
             (225%positive,ANone,226%positive)::
             (226%positive,AWeaken,227%positive)::
             (227%positive,ANone,229%positive)::
             (227%positive,ANone,228%positive)::
             (228%positive,AWeaken,234%positive)::
             (229%positive,AWeaken,230%positive)::
             (230%positive,ANone,232%positive)::
             (230%positive,ANone,231%positive)::
             (231%positive,ANone,232%positive)::
             (232%positive,ANone,233%positive)::
             (233%positive,AWeaken,234%positive)::
             (234%positive,ANone,236%positive)::
             (234%positive,ANone,235%positive)::
             (235%positive,AWeaken,241%positive)::
             (236%positive,AWeaken,237%positive)::
             (237%positive,ANone,239%positive)::
             (237%positive,ANone,238%positive)::
             (238%positive,ANone,239%positive)::
             (239%positive,ANone,240%positive)::
             (240%positive,AWeaken,241%positive)::
             (241%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_simple_progressive)
             s) <> (eval (ENum (0)) s))%Z)),243%positive)::
             (241%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_simple_progressive) s) =
             (eval (ENum (0)) s))%Z)),242%positive)::
             (242%positive,AWeaken,246%positive)::
             (243%positive,AWeaken,244%positive)::
             (244%positive,ANone,245%positive)::
             (245%positive,AWeaken,246%positive)::
             (246%positive,ANone,247%positive)::
             (246%positive,ANone,251%positive)::
             (247%positive,AWeaken,248%positive)::
             (248%positive,ANone,250%positive)::
             (248%positive,ANone,249%positive)::
             (249%positive,ANone,250%positive)::
             (250%positive,ANone,251%positive)::
             (251%positive,ANone,252%positive)::
             (252%positive,AWeaken,253%positive)::
             (254%positive,AWeaken,255%positive)::
             (255%positive,ANone,256%positive)::
             (256%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             257%positive)::(257%positive,ANone,258%positive)::
             (258%positive,ANone,259%positive)::
             (259%positive,(AAssign IDparse_switches_z (Some (EAdd (ENum (1))
             (EVar IDparse_switches_z)))),260%positive)::
             (260%positive,AWeaken,12%positive)::nil
|}.

Definition parse_switches_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 3%positive => (-1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0)%Z
    | 4%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 5%positive => (-1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0)%Z
    | 6%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_q_scale_factor) + 100 <= 0)%Z
    | 7%positive => (-1 * (s IDparse_switches_q_scale_factor) + 100 <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 8%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_q_scale_factor) + 100 <= 0 /\ 1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 9%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ 1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_q_scale_factor) + 100 <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0)%Z
    | 10%positive => (-1 * (s IDparse_switches_is_targa) <= 0 /\ 1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_q_scale_factor) + 100 <= 0 /\ 1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ 1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_q_scale_factor) + 100 <= 0 /\ 1 * (s IDparse_switches_q_scale_factor) + -100 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0)%Z
    | 12%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 13%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) <= 0)%Z
    | 14%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 16%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 18%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 20%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 22%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 24%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 26%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 28%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 30%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 32%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 34%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 36%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 38%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 40%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 42%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 44%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 46%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 48%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 50%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 52%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 54%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 56%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ 1 * (s IDparse_switches_is_targa) + -1 <= 0 /\ -1 * (s IDparse_switches_is_targa) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 62%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 63%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 64%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 65%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 66%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 67%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 68%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 69%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 70%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 71%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 72%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 73%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 74%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 75%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 76%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 77%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 78%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 79%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 80%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 81%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 82%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 83%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 84%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 85%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 86%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 87%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 88%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 89%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 90%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 91%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 92%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 93%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 94%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 95%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 96%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 97%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 98%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 99%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 100%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 101%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 102%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 103%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 104%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 105%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 106%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 107%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 108%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 109%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 110%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 111%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 112%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 113%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 114%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 115%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 116%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 117%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 118%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 119%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 120%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 121%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 122%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 123%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 124%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 125%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 126%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 127%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 128%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 129%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 130%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 131%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 132%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 133%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 134%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 135%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 136%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 137%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 138%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 139%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 140%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 141%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 142%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 143%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 144%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 145%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 146%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ 1 * (s IDparse_switches_simple_progressive) + -1 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) + 1 <= 0)%Z
    | 147%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 148%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 149%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 150%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 151%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 152%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 153%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 154%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 155%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 156%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 157%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 158%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 159%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 160%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 161%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 162%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 163%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 164%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 165%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 166%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 167%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 168%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 169%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 170%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 171%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 172%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 173%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 174%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 175%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 176%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 177%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 178%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 179%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 180%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ -1 * (s IDparse_switches_parse_switches.printed_version) <= 0)%Z
    | 181%positive => (-1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 182%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) + -1 <= 0 /\ -1 * (s IDparse_switches_parse_switches.printed_version) + 1 <= 0)%Z
    | 183%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 184%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 185%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 186%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 187%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 188%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 189%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 190%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 191%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 192%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 193%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 194%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 195%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 196%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 197%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 198%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 199%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 200%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 201%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 202%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 203%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 204%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 205%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 206%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 207%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ 1 * (s IDparse_switches_force_baseline) + -1 <= 0 /\ -1 * (s IDparse_switches_force_baseline) + 1 <= 0)%Z
    | 208%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 209%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 210%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 211%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 212%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 213%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 214%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 215%positive => (1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 216%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 217%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 218%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches__tmp2) <= 0 /\ -1 * (s IDparse_switches__tmp2) <= 0)%Z
    | 219%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 220%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 221%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 222%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 223%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 224%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 225%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 226%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 227%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 228%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 229%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 230%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 231%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 232%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 233%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 234%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 235%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 236%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 237%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 238%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 239%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 240%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 241%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 242%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 243%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) + 1 <= 0)%Z
    | 244%positive => (-1 * (s IDparse_switches_simple_progressive) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 245%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) + 1 <= 0)%Z
    | 246%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 247%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 248%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 249%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 250%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 251%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 252%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0)%Z
    | 253%positive => (-1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 254%positive => (-1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp1)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 255%positive => (-1 * (s IDparse_switches__tmp1)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0)%Z
    | 256%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 257%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 258%positive => (-1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 259%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 260%positive => (-1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_simple_progressive) <= 0 /\ -1 * (s IDparse_switches_force_baseline) <= 0 /\ -1 * (s IDparse_switches_is_targa) <= 0 /\ -1 * (s IDparse_switches_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition parse_switches_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDparse_switches_argc)))%Q
    | 2%positive => ((s IDparse_switches_z) + max0((s IDparse_switches_argc)))%Q
    | 3%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 4%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 5%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 6%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 7%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 8%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 9%positive => ((s IDparse_switches_z) + max0((s IDparse_switches__tmp)))%Q
    | 10%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 11%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 12%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 13%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 14%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 15%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 16%positive => ((s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 17%positive => ((2 # 1) + (s IDparse_switches_z)
                      + max0(-1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 18%positive => ((2 # 1) + (s IDparse_switches_z)
                      + max0(-1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 19%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 20%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 21%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 22%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 23%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 24%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 25%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 26%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 27%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 28%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 29%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 30%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 31%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 32%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 33%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 34%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 35%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 36%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 37%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 38%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 39%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 40%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 41%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 42%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 43%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 44%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 45%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 46%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 47%positive => ((2 # 1) * (s IDparse_switches__tmp)
                      - (2 # 1) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - max0(-1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 48%positive => ((2 # 1) * (s IDparse_switches__tmp)
                      - (2 # 1) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - max0(-1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 49%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 50%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 51%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 52%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 53%positive => ((1 # 1) - (s IDparse_switches__tmp)
                      + (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + (2 # 1) * max0((s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 54%positive => ((1 # 1) - (s IDparse_switches__tmp)
                      + (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + (2 # 1) * max0((s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 55%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 56%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 57%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 58%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 59%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 60%positive => ((1 # 1) - (s IDparse_switches__tmp)
                      + (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + (2 # 1) * max0((s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 61%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 62%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 63%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 64%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 65%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 66%positive => (-(s IDparse_switches__tmp) + (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      + (2 # 1) * max0(1 + (s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 67%positive => ((1 # 1) + (s IDparse_switches_z)
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 68%positive => ((1 # 1) + (s IDparse_switches_z)
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 69%positive => ((1 # 1) + (s IDparse_switches_z)
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 70%positive => ((1 # 1) + (s IDparse_switches_z)
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 71%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 72%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 73%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 74%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 75%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 76%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 77%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 78%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 79%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 80%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 81%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 82%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 83%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 84%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 85%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 86%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 87%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 88%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 89%positive => ((2 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 90%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 91%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 92%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 93%positive => ((2 # 1) * (s IDparse_switches__tmp)
                      - (2 # 1) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - max0(-1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 94%positive => ((2 # 1) + (2 # 1) * (s IDparse_switches__tmp)
                      - (2 # 1) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 95%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn))
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 96%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn))
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 97%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn))
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 98%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn))
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 99%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn))
                      - max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 100%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 101%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 102%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 103%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 104%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 105%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 106%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 107%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 108%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 109%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 110%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 111%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 112%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 113%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 114%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 115%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 116%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 117%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 118%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 119%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 120%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 121%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 122%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 123%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 124%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 125%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 126%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 127%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 128%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 129%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 130%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 131%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 132%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 133%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 134%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 135%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 136%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 137%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 138%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 139%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 140%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 141%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 142%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 143%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 144%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 145%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 146%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 147%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 148%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 149%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 150%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 151%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 152%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 153%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 154%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 155%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 156%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 157%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 158%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 159%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 160%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 161%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 162%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 163%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 164%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 165%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 166%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 167%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 168%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 169%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 170%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 171%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 172%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 173%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 174%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 175%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 176%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 177%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 178%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 179%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 180%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 181%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 182%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 183%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 184%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 185%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 186%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 187%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 188%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 189%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 190%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 191%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 192%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 193%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 194%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 195%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 196%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 197%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 198%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 199%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 200%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 201%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 202%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 203%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 204%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 205%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 206%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 207%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 208%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 209%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 210%positive => ((2 # 1) + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 211%positive => ((2 # 1) + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 212%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 213%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 214%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 215%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 216%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 217%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 218%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 219%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 220%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 221%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 222%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 223%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 224%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 225%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 226%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 227%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 228%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 229%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 230%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 231%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 232%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 233%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 234%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 235%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 236%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 237%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 238%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 239%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 240%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 241%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 242%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 243%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 244%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 245%positive => ((s IDparse_switches_z)
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 246%positive => ((s IDparse_switches_z))%Q
    | 247%positive => ((s IDparse_switches_z))%Q
    | 248%positive => ((s IDparse_switches_z))%Q
    | 249%positive => ((s IDparse_switches_z))%Q
    | 250%positive => ((s IDparse_switches_z))%Q
    | 251%positive => ((s IDparse_switches_z))%Q
    | 252%positive => ((s IDparse_switches_z))%Q
    | 253%positive => ((s IDparse_switches_z))%Q
    | 254%positive => ((s IDparse_switches_z)
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 255%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 256%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 257%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 258%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 259%positive => ((2 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | 260%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition parse_switches_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDparse_switches__tmp)
                                                             - (s IDparse_switches_argn)) ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                  + (s IDparse_switches__tmp)
                                                                  - (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
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
    | 46%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 47%positive => []
    | 48%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 53%positive => []
    | 54%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDparse_switches__tmp)
                                                             - (s IDparse_switches_argn)) ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-2 0*) F_max0_pre_decrement (1
                                                     + (s IDparse_switches__tmp)
                                                     - (s IDparse_switches_argn)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                            - (s IDparse_switches_argn));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDparse_switches__tmp)
                                                                 + (s IDparse_switches_argn))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)))]
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => [(*-1 0*) F_one]
    | 81%positive => [(*-1 0*) F_one]
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => [(*-1 0*) F_one]
    | 89%positive => [(*-1 0*) F_one]
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 95%positive => []
    | 96%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDparse_switches__tmp)
                                                             - (s IDparse_switches_argn)) ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 97%positive => []
    | 98%positive => []
    | 99%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDparse_switches__tmp)
                                                     - (s IDparse_switches_argn)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                            - (s IDparse_switches_argn));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDparse_switches__tmp)
                                                                 + (s IDparse_switches_argn))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)))]
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
    | 120%positive => [(*-1 0*) F_one]
    | 121%positive => [(*-1 0*) F_one]
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => [(*-1 0*) F_one]
    | 129%positive => [(*-1 0*) F_one]
    | 130%positive => []
    | 131%positive => []
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => [(*-1 0*) F_one]
    | 137%positive => []
    | 138%positive => []
    | 139%positive => [(*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                             - (s IDparse_switches_argn));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                  - (s IDparse_switches__tmp)
                                                                  + (s IDparse_switches_argn))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDparse_switches__tmp)
                                                                    + (s IDparse_switches_argn)))]
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
    | 151%positive => [(*-1 0*) F_one]
    | 152%positive => [(*-1 0*) F_one]
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
    | 166%positive => [(*-1 0*) F_one]
    | 167%positive => []
    | 168%positive => [(*-1 0*) F_one]
    | 169%positive => []
    | 170%positive => []
    | 171%positive => []
    | 172%positive => []
    | 173%positive => []
    | 174%positive => []
    | 175%positive => []
    | 176%positive => []
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
    | 189%positive => [(*-1 0*) F_one]
    | 190%positive => []
    | 191%positive => []
    | 192%positive => [(*-1 0*) F_one]
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
    | 211%positive => [(*-2 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDparse_switches__tmp)
                                                                  - (s IDparse_switches_argn))) (F_check_ge (0) (0))]
    | 212%positive => []
    | 213%positive => []
    | 214%positive => []
    | 215%positive => []
    | 216%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_max0_pre_decrement (1
                                                      + (s IDparse_switches__tmp)
                                                      - (s IDparse_switches_argn)) (1)]
    | 217%positive => []
    | 218%positive => [(*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                             - (s IDparse_switches_argn))]
    | 219%positive => []
    | 220%positive => []
    | 221%positive => []
    | 222%positive => []
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
    | 236%positive => []
    | 237%positive => []
    | 238%positive => []
    | 239%positive => []
    | 240%positive => []
    | 241%positive => []
    | 242%positive => [(*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                             - (s IDparse_switches_argn))]
    | 243%positive => []
    | 244%positive => []
    | 245%positive => [(*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                             - (s IDparse_switches_argn))]
    | 246%positive => []
    | 247%positive => []
    | 248%positive => []
    | 249%positive => []
    | 250%positive => []
    | 251%positive => []
    | 252%positive => []
    | 253%positive => []
    | 254%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                      + (s IDparse_switches__tmp)
                                                      - (s IDparse_switches_argn)) (1);
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 255%positive => []
    | 256%positive => []
    | 257%positive => []
    | 258%positive => []
    | 259%positive => []
    | 260%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_z))) (F_check_ge (-1
                                                                    + (s IDparse_switches_z)) (0));
                       (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches_z)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches_z)))]
    | _ => []
  end.


Theorem parse_switches_ai_correct:
  forall s p' s', steps (g_start parse_switches) s (g_edges parse_switches) p' s' -> parse_switches_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem parse_switches_pot_correct:
  forall s p' s',
    steps (g_start parse_switches) s (g_edges parse_switches) p' s' ->
    (parse_switches_pot (g_start parse_switches) s >= parse_switches_pot p' s')%Q.
Proof.
  check_lp parse_switches_ai_correct parse_switches_hints.
Qed.

