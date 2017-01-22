Require Import pasta.Pasta.

Notation IDparse_switches_z := 1%positive.
Notation IDparse_switches__tmp := 2%positive.
Notation IDparse_switches__tmp1 := 3%positive.
Notation IDparse_switches__tmp2 := 4%positive.
Notation IDparse_switches_argn := 5%positive.
Notation IDparse_switches_parse_switches.printed_version := 6%positive.
Notation IDparse_switches_requested_fmt := 7%positive.
Notation IDparse_switches_argc := 8%positive.
Notation IDparse_switches_argv := 9%positive.
Notation IDparse_switches_cinfo := 10%positive.
Notation IDparse_switches_for_real := 11%positive.
Notation IDparse_switches_last_file_arg_seen := 12%positive.
Definition parse_switches : graph := {|
  g_start := 1%positive;
  g_end := 208%positive;
  g_edges := (1%positive,(AAssign IDparse_switches_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDparse_switches__tmp
             (Some (EVar IDparse_switches_argc))),3%positive)::
             (3%positive,(AAssign IDparse_switches__tmp1
             (Some (EVar IDparse_switches_last_file_arg_seen))),4%positive)::
             (4%positive,(AAssign IDparse_switches__tmp2
             (Some (EVar IDparse_switches_for_real))),5%positive)::
             (5%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (3)))),6%positive)::
             (6%positive,(AAssign IDparse_switches_argn (Some (ENum (1)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) <
             (eval (EVar IDparse_switches__tmp) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) >=
             (eval (EVar IDparse_switches__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,208%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,203%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,200%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,189%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,189%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,189%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,189%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,169%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,149%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,141%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,140%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,135%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,132%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,130%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,130%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,114%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,97%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,95%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,93%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,90%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,82%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,79%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,79%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,ANone,76%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,ANone,65%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,62%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,ANone,64%positive)::
             (62%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (5)))),63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,75%positive)::
             (65%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             66%positive)::(66%positive,AWeaken,67%positive)::
             (67%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),69%positive)::
             (67%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),68%positive)::
             (68%positive,AWeaken,72%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,ANone,73%positive)::
             (72%positive,ANone,74%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,ANone,78%positive)::
             (76%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (4)))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,ANone,81%positive)::
             (79%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (3)))),80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,ANone,89%positive)::
             (82%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             83%positive)::(83%positive,AWeaken,84%positive)::
             (84%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),86%positive)::
             (84%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),85%positive)::
             (85%positive,AWeaken,88%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,ANone,92%positive)::
             (90%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (2)))),91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,ANone,94%positive)::
             (93%positive,ANone,94%positive)::
             (94%positive,ANone,96%positive)::
             (95%positive,ANone,96%positive)::
             (96%positive,ANone,113%positive)::
             (97%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             98%positive)::(98%positive,AWeaken,99%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),101%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),100%positive)::
             (100%positive,AWeaken,104%positive)::
             (101%positive,AWeaken,102%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,AWeaken,104%positive)::
             (104%positive,ANone,106%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,AWeaken,108%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,ANone,111%positive)::
             (108%positive,ANone,109%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,ANone,111%positive)::
             (110%positive,ANone,112%positive)::
             (111%positive,ANone,112%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,ANone,127%positive)::
             (114%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             115%positive)::(115%positive,AWeaken,116%positive)::
             (116%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),118%positive)::
             (116%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),117%positive)::
             (117%positive,AWeaken,121%positive)::
             (118%positive,AWeaken,119%positive)::
             (119%positive,ANone,120%positive)::
             (120%positive,AWeaken,121%positive)::
             (121%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches__tmp2) s) <>
             (eval (ENum (0)) s))%Z)),123%positive)::
             (121%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches__tmp2) s) =
             (eval (ENum (0)) s))%Z)),122%positive)::
             (122%positive,AWeaken,126%positive)::
             (123%positive,AWeaken,124%positive)::
             (124%positive,ANone,128%positive)::
             (124%positive,ANone,125%positive)::
             (125%positive,ANone,126%positive)::
             (126%positive,ANone,127%positive)::
             (127%positive,ANone,131%positive)::
             (128%positive,ANone,129%positive)::
             (129%positive,AWeaken,208%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,ANone,134%positive)::
             (132%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (1)))),133%positive)::
             (133%positive,ANone,134%positive)::
             (134%positive,ANone,139%positive)::
             (135%positive,AWeaken,136%positive)::
             (136%positive,ANone,138%positive)::
             (136%positive,ANone,137%positive)::
             (137%positive,ANone,138%positive)::
             (138%positive,ANone,139%positive)::
             (139%positive,ANone,148%positive)::
             (140%positive,AWeaken,142%positive)::
             (141%positive,AWeaken,142%positive)::
             (142%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_parse_switches.printed_version)
             s) <> (eval (ENum (0)) s))%Z)),146%positive)::
             (142%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_parse_switches.printed_version)
             s) = (eval (ENum (0)) s))%Z)),143%positive)::
             (143%positive,AWeaken,144%positive)::
             (144%positive,(AAssign
             IDparse_switches_parse_switches.printed_version
             (Some (ENum (1)))),145%positive)::
             (145%positive,ANone,147%positive)::
             (146%positive,AWeaken,147%positive)::
             (147%positive,ANone,148%positive)::
             (148%positive,ANone,168%positive)::
             (149%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             150%positive)::(150%positive,AWeaken,151%positive)::
             (151%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),153%positive)::
             (151%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),152%positive)::
             (152%positive,AWeaken,156%positive)::
             (153%positive,AWeaken,154%positive)::
             (154%positive,ANone,155%positive)::
             (155%positive,AWeaken,156%positive)::
             (156%positive,ANone,166%positive)::
             (156%positive,ANone,157%positive)::
             (157%positive,AWeaken,158%positive)::
             (158%positive,ANone,164%positive)::
             (158%positive,ANone,159%positive)::
             (159%positive,AWeaken,160%positive)::
             (160%positive,ANone,162%positive)::
             (160%positive,ANone,161%positive)::
             (161%positive,ANone,163%positive)::
             (162%positive,ANone,163%positive)::
             (163%positive,ANone,165%positive)::
             (164%positive,ANone,165%positive)::
             (165%positive,ANone,167%positive)::
             (166%positive,ANone,167%positive)::
             (167%positive,ANone,168%positive)::
             (168%positive,ANone,188%positive)::
             (169%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             170%positive)::(170%positive,AWeaken,171%positive)::
             (171%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),173%positive)::
             (171%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),172%positive)::
             (172%positive,AWeaken,176%positive)::
             (173%positive,AWeaken,174%positive)::
             (174%positive,ANone,175%positive)::
             (175%positive,AWeaken,176%positive)::
             (176%positive,ANone,186%positive)::
             (176%positive,ANone,177%positive)::
             (177%positive,AWeaken,178%positive)::
             (178%positive,ANone,184%positive)::
             (178%positive,ANone,179%positive)::
             (179%positive,AWeaken,180%positive)::
             (180%positive,ANone,182%positive)::
             (180%positive,ANone,181%positive)::
             (181%positive,ANone,183%positive)::
             (182%positive,ANone,183%positive)::
             (183%positive,ANone,185%positive)::
             (184%positive,ANone,185%positive)::
             (185%positive,ANone,187%positive)::
             (186%positive,ANone,187%positive)::
             (187%positive,ANone,188%positive)::
             (188%positive,ANone,199%positive)::
             (189%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             190%positive)::(190%positive,AWeaken,191%positive)::
             (191%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) >= (eval (EVar IDparse_switches__tmp) s))%Z)),193%positive)::
             (191%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDparse_switches_argn) (ENum (1)))
             s) < (eval (EVar IDparse_switches__tmp) s))%Z)),192%positive)::
             (192%positive,AWeaken,196%positive)::
             (193%positive,AWeaken,194%positive)::
             (194%positive,ANone,195%positive)::
             (195%positive,AWeaken,196%positive)::
             (196%positive,ANone,197%positive)::
             (196%positive,ANone,198%positive)::
             (197%positive,ANone,198%positive)::
             (198%positive,ANone,199%positive)::
             (199%positive,ANone,202%positive)::
             (200%positive,(AAssign IDparse_switches_requested_fmt
             (Some (ENum (0)))),201%positive)::
             (201%positive,ANone,202%positive)::
             (202%positive,ANone,211%positive)::
             (203%positive,AWeaken,204%positive)::
             (204%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) <=
             (eval (EVar IDparse_switches__tmp1) s))%Z)),209%positive)::
             (204%positive,(AGuard
             (fun s => ((eval (EVar IDparse_switches_argn) s) >
             (eval (EVar IDparse_switches__tmp1) s))%Z)),205%positive)::
             (205%positive,AWeaken,206%positive)::
             (206%positive,ANone,207%positive)::
             (207%positive,AWeaken,208%positive)::
             (209%positive,AWeaken,210%positive)::
             (210%positive,ANone,211%positive)::
             (211%positive,(AAssign IDparse_switches_argn
             (Some (EAdd (EVar IDparse_switches_argn) (ENum (1))))),
             212%positive)::(212%positive,ANone,213%positive)::
             (213%positive,ANone,214%positive)::
             (214%positive,(AAssign IDparse_switches_z (Some (EAdd (ENum (1))
             (EVar IDparse_switches_z)))),215%positive)::
             (215%positive,AWeaken,9%positive)::nil
|}.

Definition parse_switches_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 3%positive => (-1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0)%Z
    | 4%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 5%positive => (-1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0)%Z
    | 6%positive => (1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -3 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDparse_switches_requested_fmt) + 3 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -3 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ 1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -3 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 3 <= 0)%Z
    | 9%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) <= 0)%Z
    | 11%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -5 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 5 <= 0)%Z
    | 64%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 67%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 68%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 69%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 70%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 71%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 72%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 73%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 74%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 75%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 76%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 77%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -4 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 4 <= 0)%Z
    | 78%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 79%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 80%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -3 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 3 <= 0)%Z
    | 81%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 82%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 83%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 84%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 85%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 86%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 87%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 88%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 89%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 90%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 91%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -2 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 2 <= 0)%Z
    | 92%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 93%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 94%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 95%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 97%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 98%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 99%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 100%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 101%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 102%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 103%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 104%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 105%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 106%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 107%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 108%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 109%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 110%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 111%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 112%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 113%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 114%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 115%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 116%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 117%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 118%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 119%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 120%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 121%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 122%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp2) <= 0 /\ -1 * (s IDparse_switches__tmp2) <= 0)%Z
    | 123%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 124%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 125%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 126%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 127%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 128%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 129%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 130%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 131%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 132%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 133%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) + -1 <= 0 /\ -1 * (s IDparse_switches_requested_fmt) + 1 <= 0)%Z
    | 134%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 135%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 136%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 137%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 138%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 139%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 140%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 141%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 142%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 143%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ -1 * (s IDparse_switches_parse_switches.printed_version) <= 0)%Z
    | 144%positive => (-1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 145%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_parse_switches.printed_version) + -1 <= 0 /\ -1 * (s IDparse_switches_parse_switches.printed_version) + 1 <= 0)%Z
    | 146%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 147%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 148%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 149%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 150%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 151%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 152%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 153%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 154%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 155%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 156%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 157%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 158%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 159%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 160%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 161%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 162%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 163%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 164%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 165%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 166%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 167%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 168%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 169%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 170%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 171%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 172%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 173%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 174%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 175%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 176%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 177%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 178%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 179%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 180%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 181%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 182%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 183%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 184%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 185%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 186%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 187%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 188%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 189%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 190%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 191%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 192%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 193%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 194%positive => (1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 195%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ 1 * (s IDparse_switches__tmp)+ -1 * (s IDparse_switches_argn) + -1 <= 0)%Z
    | 196%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 197%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 198%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 199%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 200%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 201%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches_requested_fmt) <= 0 /\ -1 * (s IDparse_switches_requested_fmt) <= 0)%Z
    | 202%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 203%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 204%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 205%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 206%positive => (1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 207%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ 1 * (s IDparse_switches__tmp1)+ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 208%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 209%positive => (-1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches__tmp1)+ 1 * (s IDparse_switches_argn) <= 0)%Z
    | 210%positive => (-1 * (s IDparse_switches__tmp1)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0)%Z
    | 211%positive => (-1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) <= 0 /\ -1 * (s IDparse_switches_argn) + 1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 212%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 213%positive => (-1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_z) <= 0)%Z
    | 214%positive => (-1 * (s IDparse_switches_z) <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_argn) + 2 <= 0)%Z
    | 215%positive => (-1 * (s IDparse_switches_argn) + 2 <= 0 /\ -1 * (s IDparse_switches__tmp)+ 1 * (s IDparse_switches_argn) + -1 <= 0 /\ -1 * (s IDparse_switches_z) + 1 <= 0)%Z
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
    | 7%positive => (-(1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                     + (s IDparse_switches_z)
                     - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                     + max0(1 + (s IDparse_switches__tmp)
                            - (s IDparse_switches_argn)))%Q
    | 8%positive => (-(1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                     + (s IDparse_switches_z)
                     - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                     + max0(1 + (s IDparse_switches__tmp)
                            - (s IDparse_switches_argn)))%Q
    | 9%positive => (-(1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                     + (s IDparse_switches_z)
                     - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                     + max0(1 + (s IDparse_switches__tmp)
                            - (s IDparse_switches_argn)))%Q
    | 10%positive => (-(1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 11%positive => (-(1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 12%positive => ((1 # 2) + (3 # 2) * (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      - (1 # 2) * max0((s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 13%positive => ((1 # 2) + (3 # 2) * (s IDparse_switches__tmp)
                      - (s IDparse_switches_argn) + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      - (1 # 2) * max0((s IDparse_switches__tmp)
                                       - (s IDparse_switches_argn)))%Q
    | 14%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 15%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 16%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 17%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 18%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 19%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 20%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 21%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 22%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 23%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 24%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 25%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 26%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 27%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 28%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 29%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 30%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 31%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 32%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 33%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 34%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 35%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 36%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 37%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 38%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 39%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 40%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 41%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 42%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 43%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 44%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 45%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 46%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 47%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 48%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 49%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 50%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 51%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 52%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 53%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 54%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 55%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 56%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 57%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 58%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 59%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 60%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 61%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 62%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 63%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 64%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 65%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 66%positive => ((1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 67%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 68%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 69%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 70%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 71%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 72%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 73%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 74%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 75%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 76%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 77%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 78%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 79%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 80%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 81%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 82%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 83%positive => ((1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 84%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 85%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 86%positive => ((1 # 1) + (s IDparse_switches__tmp)
                      - (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 87%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 88%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 89%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 90%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 91%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 92%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 93%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 94%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 95%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 96%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 97%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                      + max0((s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 98%positive => ((1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 99%positive => ((1 # 2) * (s IDparse_switches_argn)
                      + (s IDparse_switches_z)
                      - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                      + max0(1 + (s IDparse_switches__tmp)
                             - (s IDparse_switches_argn)))%Q
    | 100%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 101%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 102%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 103%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 104%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 105%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 106%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 107%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 108%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 109%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 110%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 111%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 112%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 113%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 114%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 115%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 116%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 117%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 118%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 119%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 120%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 121%positive => ((1 # 1) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 122%positive => ((1 # 1) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 123%positive => ((1 # 1) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 124%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 125%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 126%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 127%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 128%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 129%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 130%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 131%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 132%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 133%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 134%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 135%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 136%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 137%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 138%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 139%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 140%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 141%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 142%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 143%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 144%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 145%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 146%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 147%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 148%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 149%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 150%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 151%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 152%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 153%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 154%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 155%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 156%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 157%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 158%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 159%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 160%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 161%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 162%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 163%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 164%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 165%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 166%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 167%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 168%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 169%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 170%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 171%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 172%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 173%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 174%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 175%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 176%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 177%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 178%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 179%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 180%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 181%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 182%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 183%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 184%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 185%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 186%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 187%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 188%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 189%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 190%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 191%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 192%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 193%positive => ((1 # 1) + (s IDparse_switches__tmp)
                       - (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn)))%Q
    | 194%positive => ((1 # 2) + (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn)))%Q
    | 195%positive => ((1 # 2) + (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn)))%Q
    | 196%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 197%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 198%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 199%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 200%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 201%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 202%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 203%positive => ((1 # 2) + (3 # 2) * (s IDparse_switches__tmp)
                       - (s IDparse_switches_argn) + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 204%positive => ((3 # 2) + (1 # 2) * (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 205%positive => ((3 # 2) + (1 # 2) * (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 206%positive => ((3 # 2) + (1 # 2) * (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 207%positive => ((3 # 2) + (1 # 2) * (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 208%positive => ((s IDparse_switches_z))%Q
    | 209%positive => ((3 # 2) + (1 # 2) * (s IDparse_switches__tmp)
                       + (s IDparse_switches_z)
                       + max0(-1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn))
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       - (1 # 2) * max0((s IDparse_switches__tmp)
                                        - (s IDparse_switches_argn)))%Q
    | 210%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 211%positive => ((1 # 2) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-1 + (s IDparse_switches_argn))
                       + max0((s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 212%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 213%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 214%positive => ((1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
    | 215%positive => (-(1 # 1) + (1 # 2) * (s IDparse_switches_argn)
                       + (s IDparse_switches_z)
                       - (1 # 2) * max0(-2 + (s IDparse_switches_argn))
                       + max0(1 + (s IDparse_switches__tmp)
                              - (s IDparse_switches_argn)))%Q
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDparse_switches__tmp)
                                                             - (s IDparse_switches_argn)) ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                            - (s IDparse_switches_argn));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches_argn)))]
    | 11%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 12%positive => []
    | 13%positive => [(*-1.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
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
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
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
    | 83%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDparse_switches__tmp)
                                                                   - 
                                                                   (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 86%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                              + (s IDparse_switches__tmp)
                                                              - (s IDparse_switches_argn)) ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 101%positive => []
    | 102%positive => []
    | 103%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 104%positive => []
    | 105%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 106%positive => []
    | 107%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                      + (s IDparse_switches__tmp)
                                                      - (s IDparse_switches_argn)) (1)]
    | 118%positive => []
    | 119%positive => []
    | 120%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 121%positive => []
    | 122%positive => [(*-1 0*) F_one;
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 123%positive => [(*-1 0*) F_one;
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_max0_ge_0 ((s IDparse_switches__tmp)
                                             - (s IDparse_switches_argn));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches_argn)))]
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
    | 150%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 151%positive => []
    | 152%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 153%positive => []
    | 154%positive => []
    | 155%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
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
    | 170%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 171%positive => []
    | 172%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 173%positive => []
    | 174%positive => []
    | 175%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
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
    | 189%positive => []
    | 190%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 191%positive => []
    | 192%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 193%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0))]
    | 194%positive => []
    | 195%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
    | 196%positive => []
    | 197%positive => []
    | 198%positive => []
    | 199%positive => []
    | 200%positive => []
    | 201%positive => []
    | 202%positive => []
    | 203%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)))]
    | 204%positive => []
    | 205%positive => []
    | 206%positive => []
    | 207%positive => [(*-2 0*) F_one;
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 ((s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDparse_switches__tmp)
                                                                  - (s IDparse_switches_argn))) (F_check_ge (0) (0))]
    | 208%positive => []
    | 209%positive => [(*-1.5 0*) F_max0_pre_decrement (1
                                                        + (s IDparse_switches__tmp)
                                                        - (s IDparse_switches_argn)) (1);
                       (*-1.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches__tmp)
                                                                    - 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches__tmp)
                                                                    - (s IDparse_switches_argn)) (0))]
    | 210%positive => []
    | 211%positive => []
    | 212%positive => []
    | 213%positive => []
    | 214%positive => []
    | 215%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDparse_switches_argn))) (F_check_ge (-1
                                                                    + (s IDparse_switches_argn)) (0));
                       (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDparse_switches_argn)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDparse_switches_argn)))]
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

