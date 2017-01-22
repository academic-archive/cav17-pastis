Require Import pasta.Pasta.

Notation IDuncompress_z := 1%positive.
Notation IDuncompress_cantGuess := 2%positive.
Notation IDuncompress_deleteOutputOnInterrupt := 3%positive.
Notation IDuncompress_forceOverwrite := 4%positive.
Notation IDuncompress_i := 5%positive.
Notation IDuncompress_keepInputFiles := 6%positive.
Notation IDuncompress_magicNumberOK := 7%positive.
Notation IDuncompress_n := 8%positive.
Notation IDuncompress_noisy := 9%positive.
Notation IDuncompress_retVal := 10%positive.
Notation IDuncompress_retVal1 := 11%positive.
Notation IDuncompress_srcMode := 12%positive.
Notation IDuncompress_unzFailsExist := 13%positive.
Notation IDuncompress_verbosity := 14%positive.
Notation IDuncompress_name := 15%positive.
Definition uncompress : graph := {|
  g_start := 1%positive;
  g_end := 203%positive;
  g_edges := (1%positive,(AAssign IDuncompress_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDuncompress_deleteOutputOnInterrupt
             (Some (ENum (0)))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,5%positive)::
             (4%positive,ANone,8%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDuncompress_srcMode)
             s) <> (eval (ENum (1)) s))%Z)),202%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDuncompress_srcMode)
             s) = (eval (ENum (1)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDuncompress_cantGuess (Some (ENum (0)))),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,ANone,30%positive)::
             (10%positive,ANone,29%positive)::
             (10%positive,ANone,12%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,30%positive)::
             (12%positive,(AAssign IDuncompress_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDuncompress_i) s) <
             (eval (ENum (4)) s))%Z)),19%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDuncompress_i)
             s) >= (eval (ENum (4)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDuncompress_cantGuess (Some (ENum (1)))),
             18%positive)::(18%positive,ANone,30%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,27%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDuncompress_i
             (Some (EAdd (EVar IDuncompress_i) (ENum (1))))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDuncompress_z (Some (EAdd (ENum (1))
             (EVar IDuncompress_z)))),26%positive)::
             (26%positive,AWeaken,15%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,32%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (1)) s))%Z)),34%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (1)) s))%Z)),33%positive)::
             (33%positive,AWeaken,37%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,194%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (1)) s))%Z)),39%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (1)) s))%Z)),38%positive)::
             (38%positive,AWeaken,43%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,42%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,200%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),48%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (2)) s))%Z)),47%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (2)) s))%Z)),46%positive)::
             (46%positive,AWeaken,52%positive)::
             (47%positive,AWeaken,49%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,193%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),54%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),53%positive)::
             (53%positive,AWeaken,66%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) <>
             (eval (ENum (0)) s))%Z)),65%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) =
             (eval (ENum (0)) s))%Z)),56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,59%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,AWeaken,66%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) <> (eval (ENum (0)) s))%Z)),62%positive)::
             (60%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) = (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,64%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,200%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_cantGuess) s) <>
             (eval (ENum (0)) s))%Z)),68%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_cantGuess) s) =
             (eval (ENum (0)) s))%Z)),67%positive)::
             (67%positive,AWeaken,75%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) <> (eval (ENum (0)) s))%Z)),71%positive)::
             (69%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) = (eval (ENum (0)) s))%Z)),70%positive)::
             (70%positive,AWeaken,73%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),77%positive)::
             (75%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),76%positive)::
             (76%positive,AWeaken,88%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,ANone,80%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,AWeaken,88%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) <>
             (eval (ENum (0)) s))%Z)),84%positive)::
             (81%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) =
             (eval (ENum (0)) s))%Z)),82%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,ANone,200%positive)::
             (84%positive,AWeaken,85%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,ANone,87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),90%positive)::
             (88%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),89%positive)::
             (89%positive,AWeaken,99%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) <>
             (eval (ENum (0)) s))%Z)),98%positive)::
             (91%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_forceOverwrite) s) =
             (eval (ENum (0)) s))%Z)),92%positive)::
             (92%positive,AWeaken,93%positive)::
             (93%positive,(AAssign IDuncompress_n None),94%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,ANone,97%positive)::
             (95%positive,ANone,96%positive)::
             (96%positive,AWeaken,99%positive)::
             (97%positive,ANone,200%positive)::
             (98%positive,AWeaken,99%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),101%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),100%positive)::
             (100%positive,AWeaken,104%positive)::
             (101%positive,AWeaken,102%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,AWeaken,104%positive)::
             (104%positive,ANone,191%positive)::
             (104%positive,ANone,127%positive)::
             (104%positive,ANone,119%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,ANone,115%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,ANone,111%positive)::
             (108%positive,ANone,109%positive)::
             (109%positive,ANone,110%positive)::
             (110%positive,AWeaken,131%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,ANone,113%positive)::
             (112%positive,ANone,114%positive)::
             (113%positive,ANone,114%positive)::
             (114%positive,ANone,200%positive)::
             (115%positive,AWeaken,116%positive)::
             (116%positive,ANone,117%positive)::
             (116%positive,ANone,118%positive)::
             (117%positive,ANone,118%positive)::
             (118%positive,ANone,200%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,ANone,123%positive)::
             (120%positive,ANone,121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,AWeaken,131%positive)::
             (123%positive,AWeaken,124%positive)::
             (124%positive,ANone,125%positive)::
             (124%positive,ANone,126%positive)::
             (125%positive,ANone,126%positive)::
             (126%positive,ANone,200%positive)::
             (127%positive,AWeaken,128%positive)::
             (128%positive,ANone,190%positive)::
             (128%positive,ANone,129%positive)::
             (129%positive,ANone,130%positive)::
             (130%positive,AWeaken,131%positive)::
             (131%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) >=
             (eval (ENum (1)) s))%Z)),133%positive)::
             (131%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) <
             (eval (ENum (1)) s))%Z)),132%positive)::
             (132%positive,AWeaken,135%positive)::
             (133%positive,AWeaken,134%positive)::
             (134%positive,ANone,135%positive)::
             (135%positive,(AAssign IDuncompress_deleteOutputOnInterrupt
             (Some (ENum (1)))),136%positive)::
             (136%positive,(AAssign IDuncompress_magicNumberOK None),
             137%positive)::(137%positive,AWeaken,138%positive)::
             (138%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_magicNumberOK) s) <>
             (eval (ENum (0)) s))%Z)),155%positive)::
             (138%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_magicNumberOK) s) =
             (eval (ENum (0)) s))%Z)),139%positive)::
             (139%positive,AWeaken,140%positive)::
             (140%positive,(AAssign IDuncompress_unzFailsExist
             (Some (ENum (1)))),141%positive)::
             (141%positive,(AAssign IDuncompress_deleteOutputOnInterrupt
             (Some (ENum (0)))),142%positive)::
             (142%positive,AWeaken,143%positive)::
             (143%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),145%positive)::
             (143%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),144%positive)::
             (144%positive,AWeaken,151%positive)::
             (145%positive,AWeaken,146%positive)::
             (146%positive,(AAssign IDuncompress_retVal1 None),147%positive)::
             (147%positive,AWeaken,148%positive)::
             (148%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_retVal1) s) <>
             (eval (ENum (0)) s))%Z)),152%positive)::
             (148%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_retVal1) s) =
             (eval (ENum (0)) s))%Z)),149%positive)::
             (149%positive,AWeaken,150%positive)::
             (150%positive,ANone,151%positive)::
             (151%positive,ANone,174%positive)::
             (152%positive,AWeaken,153%positive)::
             (153%positive,ANone,154%positive)::
             (154%positive,AWeaken,203%positive)::
             (155%positive,AWeaken,156%positive)::
             (156%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) =
             (eval (ENum (3)) s))%Z)),158%positive)::
             (156%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_srcMode) s) <>
             (eval (ENum (3)) s))%Z)),157%positive)::
             (157%positive,AWeaken,173%positive)::
             (158%positive,AWeaken,159%positive)::
             (159%positive,(AAssign IDuncompress_deleteOutputOnInterrupt
             (Some (ENum (0)))),160%positive)::
             (160%positive,AWeaken,161%positive)::
             (161%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_keepInputFiles) s) <>
             (eval (ENum (0)) s))%Z)),171%positive)::
             (161%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_keepInputFiles) s) =
             (eval (ENum (0)) s))%Z)),162%positive)::
             (162%positive,AWeaken,163%positive)::
             (163%positive,(AAssign IDuncompress_retVal None),164%positive)::
             (164%positive,AWeaken,165%positive)::
             (165%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_retVal) s) <>
             (eval (ENum (0)) s))%Z)),168%positive)::
             (165%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_retVal) s) =
             (eval (ENum (0)) s))%Z)),166%positive)::
             (166%positive,AWeaken,167%positive)::
             (167%positive,ANone,172%positive)::
             (168%positive,AWeaken,169%positive)::
             (169%positive,ANone,170%positive)::
             (170%positive,AWeaken,203%positive)::
             (171%positive,AWeaken,172%positive)::
             (172%positive,ANone,173%positive)::
             (173%positive,ANone,174%positive)::
             (174%positive,(AAssign IDuncompress_deleteOutputOnInterrupt
             (Some (ENum (0)))),175%positive)::
             (175%positive,AWeaken,176%positive)::
             (176%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_magicNumberOK) s) <>
             (eval (ENum (0)) s))%Z)),184%positive)::
             (176%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_magicNumberOK) s) =
             (eval (ENum (0)) s))%Z)),177%positive)::
             (177%positive,AWeaken,178%positive)::
             (178%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) >=
             (eval (ENum (1)) s))%Z)),181%positive)::
             (178%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) <
             (eval (ENum (1)) s))%Z)),179%positive)::
             (179%positive,AWeaken,180%positive)::
             (180%positive,ANone,183%positive)::
             (181%positive,AWeaken,182%positive)::
             (182%positive,ANone,183%positive)::
             (183%positive,ANone,200%positive)::
             (184%positive,AWeaken,185%positive)::
             (185%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) >=
             (eval (ENum (1)) s))%Z)),187%positive)::
             (185%positive,(AGuard
             (fun s => ((eval (EVar IDuncompress_verbosity) s) <
             (eval (ENum (1)) s))%Z)),186%positive)::
             (186%positive,AWeaken,189%positive)::
             (187%positive,AWeaken,188%positive)::
             (188%positive,ANone,189%positive)::
             (189%positive,ANone,200%positive)::
             (190%positive,ANone,200%positive)::
             (191%positive,ANone,192%positive)::
             (192%positive,AWeaken,203%positive)::
             (193%positive,ANone,200%positive)::
             (194%positive,AWeaken,195%positive)::
             (195%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) <> (eval (ENum (0)) s))%Z)),197%positive)::
             (195%positive,(AGuard (fun s => ((eval (EVar IDuncompress_noisy)
             s) = (eval (ENum (0)) s))%Z)),196%positive)::
             (196%positive,AWeaken,199%positive)::
             (197%positive,AWeaken,198%positive)::
             (198%positive,ANone,199%positive)::
             (199%positive,ANone,200%positive)::
             (200%positive,ANone,201%positive)::
             (201%positive,AWeaken,203%positive)::
             (202%positive,AWeaken,203%positive)::nil
|}.

Definition uncompress_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 3%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 4%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 5%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 6%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 7%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -1 <= 0 /\ -1 * (s IDuncompress_srcMode) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 9%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 10%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 11%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 12%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 13%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) <= 0 /\ -1 * (s IDuncompress_i) <= 0)%Z
    | 14%positive => (-1 * (s IDuncompress_i) <= 0 /\ 1 * (s IDuncompress_i) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 15%positive => (-1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDuncompress_i) + -4 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) + 4 <= 0)%Z
    | 17%positive => (-1 * (s IDuncompress_i) + 4 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDuncompress_i) + -4 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) + 4 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) + -3 <= 0)%Z
    | 20%positive => (1 * (s IDuncompress_i) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 21%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) + -3 <= 0)%Z
    | 22%positive => (1 * (s IDuncompress_i) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 23%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0 /\ -1 * (s IDuncompress_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDuncompress_i) + 1 <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 25%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0 /\ -1 * (s IDuncompress_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDuncompress_i) + 1 <= 0 /\ 1 * (s IDuncompress_i) + -4 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDuncompress_i) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_i) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 29%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 30%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 31%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 32%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 33%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -1 <= 0 /\ -1 * (s IDuncompress_srcMode) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 35%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 36%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 37%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 38%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -1 <= 0 /\ -1 * (s IDuncompress_srcMode) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 40%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 41%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 42%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 43%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 44%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 45%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 46%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 47%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -2 <= 0 /\ -1 * (s IDuncompress_srcMode) + 2 <= 0)%Z
    | 48%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 49%positive => (-1 * (s IDuncompress_srcMode) + 2 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 50%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 2 <= 0)%Z
    | 51%positive => (-1 * (s IDuncompress_srcMode) + 2 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 52%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 53%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 54%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 55%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 56%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 57%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 58%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 59%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 60%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 61%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_noisy) <= 0 /\ -1 * (s IDuncompress_noisy) <= 0)%Z
    | 62%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 63%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 64%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 65%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 66%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 67%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 68%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 69%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 70%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_noisy) <= 0 /\ -1 * (s IDuncompress_noisy) <= 0)%Z
    | 71%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 72%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 73%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 74%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) + 1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 75%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 76%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 77%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 78%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 79%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 80%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 81%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 82%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 83%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 84%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 85%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 86%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 87%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 88%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 89%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 90%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 91%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 92%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 93%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 94%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 95%positive => (-1 * (s IDuncompress_forceOverwrite) <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 96%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 97%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_forceOverwrite) <= 0 /\ -1 * (s IDuncompress_forceOverwrite) <= 0)%Z
    | 98%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 99%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 100%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 101%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 102%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 103%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 104%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 105%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 106%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 107%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 108%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 109%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 110%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 111%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 112%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 113%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 114%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 115%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 116%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 117%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 118%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 119%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 120%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 121%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 122%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 123%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 124%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 125%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 126%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 127%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 128%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 129%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 130%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 131%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 132%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_verbosity) <= 0)%Z
    | 133%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_verbosity) + 1 <= 0)%Z
    | 134%positive => (-1 * (s IDuncompress_verbosity) + 1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 135%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 136%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0)%Z
    | 137%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 138%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0)%Z
    | 139%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0)%Z
    | 140%positive => (-1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0)%Z
    | 141%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 142%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 143%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 144%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 145%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 146%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 147%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 148%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 149%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_retVal1) <= 0 /\ -1 * (s IDuncompress_retVal1) <= 0)%Z
    | 150%positive => (-1 * (s IDuncompress_retVal1) <= 0 /\ 1 * (s IDuncompress_retVal1) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 151%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 152%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 153%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_unzFailsExist) + 1 <= 0)%Z
    | 154%positive => (-1 * (s IDuncompress_unzFailsExist) + 1 <= 0 /\ 1 * (s IDuncompress_unzFailsExist) + -1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 155%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 156%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0)%Z
    | 157%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 158%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0)%Z
    | 159%positive => (-1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) + 1 <= 0)%Z
    | 160%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 161%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 162%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0)%Z
    | 163%positive => (-1 * (s IDuncompress_keepInputFiles) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 164%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0)%Z
    | 165%positive => (-1 * (s IDuncompress_keepInputFiles) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 166%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0 /\ 1 * (s IDuncompress_retVal) <= 0 /\ -1 * (s IDuncompress_retVal) <= 0)%Z
    | 167%positive => (-1 * (s IDuncompress_retVal) <= 0 /\ 1 * (s IDuncompress_retVal) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 168%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0)%Z
    | 169%positive => (-1 * (s IDuncompress_keepInputFiles) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 170%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_keepInputFiles) <= 0 /\ -1 * (s IDuncompress_keepInputFiles) <= 0)%Z
    | 171%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 172%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_srcMode) + 3 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 173%positive => (1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 174%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) + -1 <= 0)%Z
    | 175%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 176%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 177%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0)%Z
    | 178%positive => (-1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 179%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_verbosity) <= 0)%Z
    | 180%positive => (1 * (s IDuncompress_verbosity) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 181%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_verbosity) + 1 <= 0)%Z
    | 182%positive => (-1 * (s IDuncompress_verbosity) + 1 <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 183%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_magicNumberOK) <= 0 /\ -1 * (s IDuncompress_magicNumberOK) <= 0)%Z
    | 184%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 185%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 186%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_verbosity) <= 0)%Z
    | 187%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_verbosity) + 1 <= 0)%Z
    | 188%positive => (-1 * (s IDuncompress_verbosity) + 1 <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 189%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 190%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 191%positive => (-1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 192%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0)%Z
    | 193%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_srcMode) + -3 <= 0 /\ -1 * (s IDuncompress_srcMode) + 2 <= 0)%Z
    | 194%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 195%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 196%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ 1 * (s IDuncompress_noisy) <= 0 /\ -1 * (s IDuncompress_noisy) <= 0)%Z
    | 197%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 198%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 199%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 200%positive => (1 * (s IDuncompress_cantGuess) + -1 <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | 201%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_cantGuess) <= 0 /\ 1 * (s IDuncompress_cantGuess) + -1 <= 0)%Z
    | 202%positive => (-1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_z) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0)%Z
    | 203%positive => (-1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ 1 * (s IDuncompress_deleteOutputOnInterrupt) <= 0 /\ -1 * (s IDuncompress_z) <= 0)%Z
    | _ => False
  end.

Definition uncompress_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 3%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 4%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 5%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 6%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 7%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 8%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 9%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                     + (s IDuncompress_z))%Q
    | 10%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 11%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 12%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 13%positive => (-(4 # 3) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 14%positive => (-(4 # 3) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 15%positive => (-(4 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 16%positive => (-(4 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 17%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 18%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 19%positive => (-(4 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 20%positive => ((1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(3 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 21%positive => ((1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(3 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 22%positive => ((1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(3 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 23%positive => (-(1 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 24%positive => (-(1 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 25%positive => (-(1 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 26%positive => (-(4 # 3) + (1 # 3) * (s IDuncompress_i)
                      + (s IDuncompress_z)
                      + (4 # 3) * max0(4 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 27%positive => ((1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(3 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 28%positive => ((1 # 3) * (s IDuncompress_i) + (s IDuncompress_z)
                      + (4 # 3) * max0(3 - (s IDuncompress_i))
                      + (4 # 1) * max0(-(s IDuncompress_cantGuess))
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 29%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 30%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 31%positive => ((4 # 1) - (4 # 1) * (s IDuncompress_cantGuess)
                      + (s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 32%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 33%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 34%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 35%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 36%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 37%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 38%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 39%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 40%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 41%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 42%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 43%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 44%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 45%positive => ((s IDuncompress_z)
                      + (4 # 1) * max0(-(s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 46%positive => ((s IDuncompress_z)
                      + (4 # 1) * max0(-(s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 47%positive => ((s IDuncompress_z)
                      + (4 # 1) * max0(-(s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 48%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 49%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 50%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 51%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 52%positive => ((s IDuncompress_z))%Q
    | 53%positive => ((s IDuncompress_z))%Q
    | 54%positive => ((s IDuncompress_z))%Q
    | 55%positive => (max0((s IDuncompress_z)))%Q
    | 56%positive => (max0((s IDuncompress_z)))%Q
    | 57%positive => (max0((s IDuncompress_z)))%Q
    | 58%positive => (max0((s IDuncompress_z)))%Q
    | 59%positive => (max0((s IDuncompress_z)))%Q
    | 60%positive => ((s IDuncompress_z))%Q
    | 61%positive => ((s IDuncompress_z))%Q
    | 62%positive => ((s IDuncompress_z))%Q
    | 63%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 64%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 65%positive => (max0((s IDuncompress_z)))%Q
    | 66%positive => (max0((s IDuncompress_z)))%Q
    | 67%positive => (max0((s IDuncompress_z)))%Q
    | 68%positive => (max0((s IDuncompress_z)))%Q
    | 69%positive => (max0((s IDuncompress_z)))%Q
    | 70%positive => (max0((s IDuncompress_z)))%Q
    | 71%positive => (max0((s IDuncompress_z)))%Q
    | 72%positive => (max0((s IDuncompress_z)))%Q
    | 73%positive => (max0((s IDuncompress_z)))%Q
    | 74%positive => (max0((s IDuncompress_z)))%Q
    | 75%positive => (max0((s IDuncompress_z)))%Q
    | 76%positive => (max0((s IDuncompress_z)))%Q
    | 77%positive => (max0((s IDuncompress_z)))%Q
    | 78%positive => (max0((s IDuncompress_z)))%Q
    | 79%positive => (max0((s IDuncompress_z)))%Q
    | 80%positive => (max0((s IDuncompress_z)))%Q
    | 81%positive => (max0((s IDuncompress_z)))%Q
    | 82%positive => (max0((s IDuncompress_z)))%Q
    | 83%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 84%positive => (max0((s IDuncompress_z)))%Q
    | 85%positive => (max0((s IDuncompress_z)))%Q
    | 86%positive => (max0((s IDuncompress_z)))%Q
    | 87%positive => (max0((s IDuncompress_z)))%Q
    | 88%positive => (max0((s IDuncompress_z)))%Q
    | 89%positive => (max0((s IDuncompress_z)))%Q
    | 90%positive => (max0((s IDuncompress_z)))%Q
    | 91%positive => ((s IDuncompress_z))%Q
    | 92%positive => ((s IDuncompress_z))%Q
    | 93%positive => ((s IDuncompress_z))%Q
    | 94%positive => ((s IDuncompress_z))%Q
    | 95%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 96%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 97%positive => ((s IDuncompress_z)
                      - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 98%positive => ((s IDuncompress_z))%Q
    | 99%positive => ((s IDuncompress_z))%Q
    | 100%positive => ((s IDuncompress_z))%Q
    | 101%positive => ((s IDuncompress_z))%Q
    | 102%positive => ((s IDuncompress_z))%Q
    | 103%positive => ((s IDuncompress_z))%Q
    | 104%positive => ((s IDuncompress_z))%Q
    | 105%positive => ((s IDuncompress_z))%Q
    | 106%positive => ((s IDuncompress_z))%Q
    | 107%positive => ((s IDuncompress_z))%Q
    | 108%positive => ((s IDuncompress_z))%Q
    | 109%positive => ((s IDuncompress_z))%Q
    | 110%positive => ((s IDuncompress_z))%Q
    | 111%positive => ((s IDuncompress_z))%Q
    | 112%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 113%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 114%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 115%positive => ((s IDuncompress_z))%Q
    | 116%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 117%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 118%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 119%positive => ((s IDuncompress_z))%Q
    | 120%positive => ((s IDuncompress_z))%Q
    | 121%positive => ((s IDuncompress_z))%Q
    | 122%positive => ((s IDuncompress_z))%Q
    | 123%positive => ((s IDuncompress_z))%Q
    | 124%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 125%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 126%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 127%positive => ((s IDuncompress_z))%Q
    | 128%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 129%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 130%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 131%positive => ((s IDuncompress_z))%Q
    | 132%positive => ((s IDuncompress_z))%Q
    | 133%positive => ((s IDuncompress_z))%Q
    | 134%positive => ((s IDuncompress_z))%Q
    | 135%positive => ((s IDuncompress_z))%Q
    | 136%positive => ((s IDuncompress_z))%Q
    | 137%positive => ((s IDuncompress_z))%Q
    | 138%positive => ((s IDuncompress_z))%Q
    | 139%positive => ((s IDuncompress_z))%Q
    | 140%positive => ((s IDuncompress_z))%Q
    | 141%positive => ((s IDuncompress_z))%Q
    | 142%positive => ((s IDuncompress_z))%Q
    | 143%positive => ((s IDuncompress_z))%Q
    | 144%positive => ((s IDuncompress_z))%Q
    | 145%positive => ((s IDuncompress_z))%Q
    | 146%positive => ((s IDuncompress_z))%Q
    | 147%positive => ((s IDuncompress_z))%Q
    | 148%positive => ((s IDuncompress_z))%Q
    | 149%positive => ((s IDuncompress_z))%Q
    | 150%positive => ((s IDuncompress_z))%Q
    | 151%positive => ((s IDuncompress_z))%Q
    | 152%positive => ((s IDuncompress_z))%Q
    | 153%positive => ((s IDuncompress_z))%Q
    | 154%positive => ((s IDuncompress_z))%Q
    | 155%positive => ((s IDuncompress_z))%Q
    | 156%positive => ((s IDuncompress_z))%Q
    | 157%positive => ((s IDuncompress_z))%Q
    | 158%positive => ((s IDuncompress_z))%Q
    | 159%positive => ((s IDuncompress_z))%Q
    | 160%positive => ((s IDuncompress_z))%Q
    | 161%positive => ((s IDuncompress_z))%Q
    | 162%positive => ((s IDuncompress_z))%Q
    | 163%positive => ((s IDuncompress_z))%Q
    | 164%positive => ((s IDuncompress_z))%Q
    | 165%positive => ((s IDuncompress_z))%Q
    | 166%positive => ((s IDuncompress_z))%Q
    | 167%positive => ((s IDuncompress_z))%Q
    | 168%positive => ((s IDuncompress_z))%Q
    | 169%positive => ((s IDuncompress_z))%Q
    | 170%positive => ((s IDuncompress_z))%Q
    | 171%positive => ((s IDuncompress_z))%Q
    | 172%positive => ((s IDuncompress_z))%Q
    | 173%positive => ((s IDuncompress_z))%Q
    | 174%positive => ((s IDuncompress_z))%Q
    | 175%positive => ((s IDuncompress_z))%Q
    | 176%positive => ((s IDuncompress_z))%Q
    | 177%positive => ((s IDuncompress_z))%Q
    | 178%positive => ((s IDuncompress_z))%Q
    | 179%positive => ((s IDuncompress_z))%Q
    | 180%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 181%positive => ((s IDuncompress_z))%Q
    | 182%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 183%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 184%positive => ((s IDuncompress_z))%Q
    | 185%positive => ((s IDuncompress_z))%Q
    | 186%positive => ((s IDuncompress_z))%Q
    | 187%positive => ((s IDuncompress_z))%Q
    | 188%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 189%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 190%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 191%positive => ((s IDuncompress_z))%Q
    | 192%positive => ((s IDuncompress_z))%Q
    | 193%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 194%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 195%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 196%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 197%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 198%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 199%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 200%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 201%positive => ((s IDuncompress_z)
                       - (4 # 1) * max0((s IDuncompress_deleteOutputOnInterrupt)))%Q
    | 202%positive => ((4 # 1) + (s IDuncompress_z))%Q
    | 203%positive => ((s IDuncompress_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition uncompress_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_cantGuess)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_cantGuess)))]
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDuncompress_i)) (3
                                                                    - (s IDuncompress_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDuncompress_i));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_cantGuess))) (F_check_ge (0) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDuncompress_i))) (F_check_ge (4
                                                                    - (s IDuncompress_i)) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*0 1.33333*) F_max0_pre_decrement (4
                                                          - (s IDuncompress_i)) (1)]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1.33333 0*) F_max0_ge_0 (3 - (s IDuncompress_i));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_i))) (F_check_ge (0) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_i)) (0))) (F_max0_ge_0 ((s IDuncompress_i)));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_cantGuess))) (F_check_ge (0) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDuncompress_cantGuess))) (F_check_ge (0) (0));
                      (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDuncompress_cantGuess)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDuncompress_cantGuess)))]
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
    | 44%positive => [(*-5.98333e-11 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt)));
                      (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)))]
    | 45%positive => []
    | 46%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 47%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt)));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)))]
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_z)) (0))) (F_max0_ge_0 ((s IDuncompress_z)))]
    | 54%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_z)) (0))) (F_max0_ge_0 ((s IDuncompress_z)))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDuncompress_z))) (F_check_ge ((s IDuncompress_z)) (0))]
    | 60%positive => []
    | 61%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 62%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
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
    | 82%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDuncompress_z))) (F_check_ge ((s IDuncompress_z)) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDuncompress_z))) (F_check_ge ((s IDuncompress_z)) (0))]
    | 90%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDuncompress_z))) (F_check_ge ((s IDuncompress_z)) (0))]
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 95%positive => []
    | 96%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt)));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)))]
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
    | 111%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 128%positive => []
    | 129%positive => []
    | 130%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt)));
                       (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0));
                       (*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)))]
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
    | 176%positive => []
    | 177%positive => []
    | 178%positive => []
    | 179%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 180%positive => []
    | 181%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 182%positive => []
    | 183%positive => []
    | 184%positive => []
    | 185%positive => []
    | 186%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 187%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0))]
    | 188%positive => []
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
    | 201%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 ((s IDuncompress_deleteOutputOnInterrupt)));
                       (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDuncompress_deleteOutputOnInterrupt))) (F_check_ge (0) (0));
                       (*-4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)) (0))) (F_max0_ge_0 (-
                                                                    (s IDuncompress_deleteOutputOnInterrupt)))]
    | 202%positive => [(*-4 0*) F_one]
    | 203%positive => []
    | _ => []
  end.


Theorem uncompress_ai_correct:
  forall s p' s', steps (g_start uncompress) s (g_edges uncompress) p' s' -> uncompress_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem uncompress_pot_correct:
  forall s p' s',
    steps (g_start uncompress) s (g_edges uncompress) p' s' ->
    (uncompress_pot (g_start uncompress) s >= uncompress_pot p' s')%Q.
Proof.
  check_lp uncompress_ai_correct uncompress_hints.
Qed.

