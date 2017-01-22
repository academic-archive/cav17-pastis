Require Import pasta.Pasta.

Notation IDzchar_set_cache_z := 1%positive.
Notation IDzchar_set_cache__tmp := 2%positive.
Notation IDzchar_set_cache_code := 3%positive.
Notation IDzchar_set_cache_code1 := 4%positive.
Notation IDzchar_set_cache_es_code_ := 5%positive.
Notation IDzchar_set_cache_have_cdevproc := 6%positive.
Notation IDzchar_set_cache_i := 7%positive.
Notation IDzchar_set_cache_metrics2 := 8%positive.
Notation IDzchar_set_cache_nparams := 9%positive.
Notation IDzchar_set_cache_pbfont_dref_off96 := 10%positive.
Notation IDzchar_set_cache_cont_fill := 11%positive.
Notation IDzchar_set_cache_cont_stroke := 12%positive.
Notation IDzchar_set_cache_op := 13%positive.
Notation IDzchar_set_cache_pbbox := 14%positive.
Notation IDzchar_set_cache_pbfont := 15%positive.
Notation IDzchar_set_cache_pcnref := 16%positive.
Notation IDzchar_set_cache_psb := 17%positive.
Notation IDzchar_set_cache_pwidth := 18%positive.
Definition zchar_set_cache : graph := {|
  g_start := 1%positive;
  g_end := 150%positive;
  g_edges := (1%positive,(AAssign IDzchar_set_cache_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDzchar_set_cache_metrics2
             (Some (ENum (0)))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_pbfont_dref_off96) s) =
             (eval (ENum (0)) s))%Z)),11%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_pbfont_dref_off96)
             s) <> (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,8%positive)::
             (6%positive,ANone,7%positive)::(7%positive,ANone,9%positive)::
             (8%positive,ANone,9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,14%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (14%positive,ANone,42%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (-20)))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,150%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,25%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (-7)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,150%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,41%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,32%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDzchar_set_cache__tmp None),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,150%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (33%positive,ANone,40%positive)::
             (34%positive,(AAssign IDzchar_set_cache_code None),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_code) s) <
             (eval (ENum (0)) s))%Z)),146%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_code) s) >=
             (eval (ENum (0)) s))%Z)),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDzchar_set_cache_metrics2
             (Some (ENum (1)))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDzchar_set_cache_have_cdevproc None),
             43%positive)::(43%positive,AWeaken,44%positive)::
             (44%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_have_cdevproc) s) <>
             (eval (ENum (0)) s))%Z)),75%positive)::
             (44%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_have_cdevproc) s) =
             (eval (ENum (0)) s))%Z)),45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,74%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) <>
             (eval (ENum (0)) s))%Z)),51%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) =
             (eval (ENum (0)) s))%Z)),49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,53%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDzchar_set_cache_code1 None),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_code1) s) <
             (eval (ENum (0)) s))%Z)),70%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_code1) s) >=
             (eval (ENum (0)) s))%Z)),56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (57%positive,ANone,64%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,67%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDzchar_set_cache__tmp None),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,150%positive)::
             (67%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (-16)))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,AWeaken,150%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,(AAssign IDzchar_set_cache__tmp
             (Some (EVar IDzchar_set_cache_code1))),72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,AWeaken,150%positive)::
             (74%positive,AWeaken,76%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_have_cdevproc) s) <>
             (eval (ENum (0)) s))%Z)),87%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_have_cdevproc) s) =
             (eval (ENum (0)) s))%Z)),77%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) <>
             (eval (ENum (0)) s))%Z)),82%positive)::
             (78%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) =
             (eval (ENum (0)) s))%Z)),79%positive)::
             (79%positive,AWeaken,80%positive)::
             (80%positive,(AAssign IDzchar_set_cache_nparams
             (Some (ENum (6)))),81%positive)::
             (81%positive,ANone,85%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AAssign IDzchar_set_cache_nparams
             (Some (ENum (10)))),84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,AWeaken,100%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,ANone,92%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,(AAssign IDzchar_set_cache__tmp None),90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,AWeaken,150%positive)::
             (92%positive,AWeaken,93%positive)::
             (93%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) <>
             (eval (ENum (0)) s))%Z)),96%positive)::
             (93%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_metrics2) s) =
             (eval (ENum (0)) s))%Z)),94%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,ANone,97%positive)::
             (96%positive,AWeaken,97%positive)::
             (97%positive,(AAssign IDzchar_set_cache_nparams
             (Some (ENum (10)))),98%positive)::
             (98%positive,ANone,99%positive)::
             (99%positive,AWeaken,100%positive)::
             (100%positive,ANone,102%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,AWeaken,108%positive)::
             (102%positive,(AAssign IDzchar_set_cache_es_code_ None),
             103%positive)::(103%positive,AWeaken,104%positive)::
             (104%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_es_code_) s) <
             (eval (ENum (0)) s))%Z)),142%positive)::
             (104%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_es_code_) s) >=
             (eval (ENum (0)) s))%Z)),105%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,ANone,118%positive)::
             (108%positive,ANone,109%positive)::
             (109%positive,ANone,110%positive)::
             (110%positive,AWeaken,111%positive)::
             (111%positive,ANone,115%positive)::
             (111%positive,ANone,112%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,ANone,114%positive)::
             (114%positive,ANone,124%positive)::
             (115%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (-16)))),116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,AWeaken,150%positive)::
             (118%positive,ANone,119%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,ANone,139%positive)::
             (120%positive,ANone,121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,(AAssign IDzchar_set_cache_i (Some (ENum (0)))),
             125%positive)::(125%positive,ANone,126%positive)::
             (126%positive,AWeaken,127%positive)::
             (127%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_i) s) <
             (eval (EVar IDzchar_set_cache_nparams) s))%Z)),132%positive)::
             (127%positive,(AGuard
             (fun s => ((eval (EVar IDzchar_set_cache_i) s) >=
             (eval (EVar IDzchar_set_cache_nparams) s))%Z)),128%positive)::
             (128%positive,AWeaken,129%positive)::
             (129%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (5)))),130%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,AWeaken,150%positive)::
             (132%positive,AWeaken,133%positive)::
             (133%positive,ANone,134%positive)::
             (134%positive,(AAssign IDzchar_set_cache_i
             (Some (EAdd (EVar IDzchar_set_cache_i) (ENum (1))))),
             135%positive)::(135%positive,ANone,136%positive)::
             (136%positive,ANone,137%positive)::
             (137%positive,(AAssign IDzchar_set_cache_z
             (Some (EAdd (ENum (1)) (EVar IDzchar_set_cache_z)))),
             138%positive)::(138%positive,AWeaken,127%positive)::
             (139%positive,(AAssign IDzchar_set_cache__tmp
             (Some (ENum (-16)))),140%positive)::
             (140%positive,ANone,141%positive)::
             (141%positive,AWeaken,150%positive)::
             (142%positive,AWeaken,143%positive)::
             (143%positive,(AAssign IDzchar_set_cache__tmp
             (Some (EVar IDzchar_set_cache_es_code_))),144%positive)::
             (144%positive,ANone,145%positive)::
             (145%positive,AWeaken,150%positive)::
             (146%positive,AWeaken,147%positive)::
             (147%positive,(AAssign IDzchar_set_cache__tmp
             (Some (EVar IDzchar_set_cache_code))),148%positive)::
             (148%positive,ANone,149%positive)::
             (149%positive,AWeaken,150%positive)::nil
|}.

Definition zchar_set_cache_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 4%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 6%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 8%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 9%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 10%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 11%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0 /\ -1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0)%Z
    | 12%positive => (-1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0 /\ 1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 13%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0 /\ -1 * (s IDzchar_set_cache_pbfont_dref_off96) <= 0)%Z
    | 14%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 15%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 16%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 17%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 18%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 20 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + -20 <= 0)%Z
    | 19%positive => (-1 * (s IDzchar_set_cache__tmp) + -20 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 20 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 20%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 21%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 22%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 23%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 7 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + -7 <= 0)%Z
    | 24%positive => (-1 * (s IDzchar_set_cache__tmp) + -7 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 7 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 25%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 26%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 27%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 28%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 29%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 30%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 31%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 32%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 33%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 34%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 35%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 36%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 37%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_code) <= 0)%Z
    | 38%positive => (-1 * (s IDzchar_set_cache_code) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 39%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_code) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 41%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 42%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 43%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 44%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 45%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0)%Z
    | 46%positive => (-1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 47%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0)%Z
    | 48%positive => (-1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 49%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 50%positive => (1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 51%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 52%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 53%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 54%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 55%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 56%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 57%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 58%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 59%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 60%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 61%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 62%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 63%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 64%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 65%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 66%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0)%Z
    | 67%positive => (-1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 68%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + -16 <= 0)%Z
    | 69%positive => (-1 * (s IDzchar_set_cache__tmp) + -16 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ -1 * (s IDzchar_set_cache_code1) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 70%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_code1) + 1 <= 0)%Z
    | 71%positive => (1 * (s IDzchar_set_cache_code1) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 72%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_code1) + 1 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 1 <= 0)%Z
    | 73%positive => (1 * (s IDzchar_set_cache__tmp) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_code1) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 74%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0)%Z
    | 75%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 76%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 77%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0)%Z
    | 78%positive => (-1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 79%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 80%positive => (1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 81%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -6 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 82%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 83%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 84%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 10 <= 0)%Z
    | 85%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 86%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ -1 * (s IDzchar_set_cache_have_cdevproc) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 87%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 88%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 89%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 90%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 91%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 92%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 93%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 94%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 95%positive => (1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 96%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0)%Z
    | 97%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 98%positive => (1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 10 <= 0)%Z
    | 99%positive => (-1 * (s IDzchar_set_cache_nparams) + 10 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 100%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 101%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 102%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 103%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 104%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 105%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_es_code_) <= 0)%Z
    | 106%positive => (-1 * (s IDzchar_set_cache_es_code_) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 107%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_es_code_) <= 0)%Z
    | 108%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 109%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 110%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 111%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 112%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 113%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 114%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 115%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 116%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + -16 <= 0)%Z
    | 117%positive => (-1 * (s IDzchar_set_cache__tmp) + -16 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 118%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 119%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 120%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 121%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 122%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 123%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 124%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 125%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0)%Z
    | 126%positive => (-1 * (s IDzchar_set_cache_i) <= 0 /\ 1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0)%Z
    | 127%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 128%positive => (1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i)+ 1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 129%positive => (-1 * (s IDzchar_set_cache_i)+ 1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 130%positive => (1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i)+ 1 * (s IDzchar_set_cache_nparams) <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + -5 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + 5 <= 0)%Z
    | 131%positive => (-1 * (s IDzchar_set_cache__tmp) + 5 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + -5 <= 0 /\ -1 * (s IDzchar_set_cache_i)+ 1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 132%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) + 1 <= 0)%Z
    | 133%positive => (1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 134%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_i) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) + 1 <= 0)%Z
    | 135%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ -1 * (s IDzchar_set_cache_i) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 136%positive => (1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_i) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | 137%positive => (-1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ -1 * (s IDzchar_set_cache_i) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0)%Z
    | 138%positive => (1 * (s IDzchar_set_cache_i)+ -1 * (s IDzchar_set_cache_nparams) <= 0 /\ -1 * (s IDzchar_set_cache_i) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_z) + 1 <= 0)%Z
    | 139%positive => (1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 140%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ -1 * (s IDzchar_set_cache__tmp) + -16 <= 0)%Z
    | 141%positive => (-1 * (s IDzchar_set_cache__tmp) + -16 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 16 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 142%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ 1 * (s IDzchar_set_cache_es_code_) + 1 <= 0)%Z
    | 143%positive => (1 * (s IDzchar_set_cache_es_code_) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 144%positive => (-1 * (s IDzchar_set_cache_nparams) + 6 <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ 1 * (s IDzchar_set_cache_es_code_) + 1 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 1 <= 0)%Z
    | 145%positive => (1 * (s IDzchar_set_cache__tmp) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_es_code_) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_nparams) + -10 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_nparams) + 6 <= 0)%Z
    | 146%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_code) + 1 <= 0)%Z
    | 147%positive => (1 * (s IDzchar_set_cache_code) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 148%positive => (-1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_code) + 1 <= 0 /\ 1 * (s IDzchar_set_cache__tmp) + 1 <= 0)%Z
    | 149%positive => (1 * (s IDzchar_set_cache__tmp) + 1 <= 0 /\ 1 * (s IDzchar_set_cache_code) + 1 <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_z) <= 0 /\ 1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0)%Z
    | 150%positive => (1 * (s IDzchar_set_cache_metrics2) + -1 <= 0 /\ -1 * (s IDzchar_set_cache_metrics2) <= 0 /\ -1 * (s IDzchar_set_cache_z) <= 0)%Z
    | _ => False
  end.

Definition zchar_set_cache_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((10 # 1))%Q
    | 2%positive => ((10 # 1))%Q
    | 3%positive => ((10 # 1))%Q
    | 4%positive => ((10 # 1))%Q
    | 5%positive => ((10 # 1))%Q
    | 6%positive => ((10 # 1))%Q
    | 7%positive => ((10 # 1))%Q
    | 8%positive => ((10 # 1))%Q
    | 9%positive => ((10 # 1))%Q
    | 10%positive => ((10 # 1))%Q
    | 11%positive => ((10 # 1))%Q
    | 12%positive => ((10 # 1))%Q
    | 13%positive => ((10 # 1))%Q
    | 14%positive => ((10 # 1))%Q
    | 15%positive => ((10 # 1))%Q
    | 16%positive => ((10 # 1))%Q
    | 17%positive => ((10 # 1))%Q
    | 18%positive => ((5 # 2) * max0(-16 - (s IDzchar_set_cache__tmp)))%Q
    | 19%positive => ((5 # 2) * max0(-16 - (s IDzchar_set_cache__tmp)))%Q
    | 20%positive => ((10 # 1))%Q
    | 21%positive => ((10 # 1))%Q
    | 22%positive => ((10 # 1))%Q
    | 23%positive => ((5 # 3) * max0(-1 - (s IDzchar_set_cache__tmp)))%Q
    | 24%positive => ((5 # 3) * max0(-1 - (s IDzchar_set_cache__tmp)))%Q
    | 25%positive => ((10 # 1))%Q
    | 26%positive => ((10 # 1))%Q
    | 27%positive => ((10 # 1))%Q
    | 28%positive => ((10 # 1))%Q
    | 29%positive => ((10 # 1))%Q
    | 30%positive => ((10 # 1))%Q
    | 31%positive => ((10 # 1))%Q
    | 32%positive => ((10 # 1))%Q
    | 33%positive => ((10 # 1))%Q
    | 34%positive => ((10 # 1))%Q
    | 35%positive => ((10 # 1))%Q
    | 36%positive => ((10 # 1))%Q
    | 37%positive => ((10 # 1))%Q
    | 38%positive => ((10 # 1))%Q
    | 39%positive => ((10 # 1))%Q
    | 40%positive => ((10 # 1))%Q
    | 41%positive => ((10 # 1))%Q
    | 42%positive => ((10 # 1))%Q
    | 43%positive => ((10 # 1))%Q
    | 44%positive => ((10 # 1))%Q
    | 45%positive => ((10 # 1))%Q
    | 46%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 47%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 48%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 49%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 50%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 51%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 52%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 53%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 54%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 55%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 56%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 57%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 58%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 59%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 60%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 61%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 62%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 63%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 64%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 65%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 66%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 67%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 68%positive => (-(10 # 1) + (10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 9) * max0(-7 - (s IDzchar_set_cache__tmp))
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 69%positive => (-(10 # 1) + (10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (s IDzchar_set_cache_z)
                      + (10 # 9) * max0(-7 - (s IDzchar_set_cache__tmp))
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 70%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 71%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 72%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 73%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 74%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 75%positive => ((10 # 1))%Q
    | 76%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 77%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 78%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 79%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 80%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 81%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 82%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 83%positive => ((4 # 1) + (10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 84%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 85%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 86%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 87%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                      + (10 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 88%positive => ((10 # 1))%Q
    | 89%positive => ((10 # 1))%Q
    | 90%positive => ((10 # 1))%Q
    | 91%positive => ((10 # 1))%Q
    | 92%positive => ((10 # 1))%Q
    | 93%positive => ((10 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 94%positive => ((10 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 95%positive => ((10 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 96%positive => ((10 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 97%positive => ((10 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 98%positive => ((6 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 99%positive => ((6 # 1) + (4 # 1) * (s IDzchar_set_cache_metrics2)
                      + max0(-6 + (s IDzchar_set_cache_nparams))
                      - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 100%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 101%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 102%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 103%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2)))%Q
    | 104%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 105%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 106%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 107%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 108%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + (2 # 1) * (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 109%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + (2 # 1) * (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 110%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + (2 # 1) * (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 111%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 112%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 113%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 114%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 115%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 116%positive => ((s IDzchar_set_cache_z)
                       + (2 # 3) * max0(-7 - (s IDzchar_set_cache__tmp))
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 117%positive => ((s IDzchar_set_cache_z)
                       + (2 # 3) * max0(-7 - (s IDzchar_set_cache__tmp))
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 118%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + (2 # 1) * (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 119%positive => ((10 # 1) * (s IDzchar_set_cache_metrics2)
                       + (2 # 1) * (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       - (4 # 1) * max0((s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 120%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 121%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 122%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 123%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 124%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 125%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 126%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 127%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 128%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 129%positive => ((6 # 1) - (s IDzchar_set_cache_nparams)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_i)
                              + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 130%positive => ((6 # 1) - (s IDzchar_set_cache_nparams)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_i)
                              + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 131%positive => ((6 # 1) - (s IDzchar_set_cache_nparams)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_i)
                              + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 132%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 133%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 134%positive => ((6 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 135%positive => ((7 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 136%positive => ((7 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 137%positive => ((7 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 138%positive => ((7 # 1) - (s IDzchar_set_cache_i)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-1 + (s IDzchar_set_cache_z)))%Q
    | 139%positive => ((6 # 1) + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 140%positive => ((s IDzchar_set_cache_z)
                       + (2 # 3) * max0(-7 - (s IDzchar_set_cache__tmp))
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 141%positive => ((s IDzchar_set_cache_z)
                       + (2 # 3) * max0(-7 - (s IDzchar_set_cache__tmp))
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + max0(-(s IDzchar_set_cache_z))
                       + max0((s IDzchar_set_cache_z)))%Q
    | 142%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2)))%Q
    | 143%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 144%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 145%positive => ((6 # 1) * (s IDzchar_set_cache_metrics2)
                       + (s IDzchar_set_cache_z)
                       + max0(-6 + (s IDzchar_set_cache_nparams))
                       + (6 # 1) * max0(1 - (s IDzchar_set_cache_metrics2))
                       + max0(-(s IDzchar_set_cache_z)))%Q
    | 146%positive => ((10 # 1))%Q
    | 147%positive => ((10 # 1))%Q
    | 148%positive => ((10 # 1))%Q
    | 149%positive => ((10 # 1))%Q
    | 150%positive => ((s IDzchar_set_cache_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zchar_set_cache_hints (p : node) (s : state) := 
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
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)));
                      (*-2.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                   - 
                                                                   (s IDzchar_set_cache__tmp))) (F_check_ge (0) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)));
                      (*-1.66667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDzchar_set_cache__tmp))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-10 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
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
    | 45%positive => [(*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDzchar_set_cache_metrics2)))]
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
    | 56%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-10 0*) F_one;
                      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))]
    | 67%positive => []
    | 68%positive => []
    | 69%positive => [(*-10 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0));
                      (*-1.11111 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - (s IDzchar_set_cache__tmp))) (F_check_ge (0) (0))]
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)));
                      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0));
                      (*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_metrics2)));
                      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                  - (s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0))]
    | 74%positive => []
    | 75%positive => [(*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDzchar_set_cache_metrics2)))]
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => [(*0 4*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0))]
    | 80%positive => []
    | 81%positive => []
    | 82%positive => [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzchar_set_cache_metrics2))) (F_check_ge ((s IDzchar_set_cache_metrics2)) (0));
                      (*0 4*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                  - (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))]
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => [(*0 10*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   - 
                                                                   (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))]
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => [(*-10 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
    | 92%positive => [(*0 4*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzchar_set_cache_metrics2))) (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => [(*-6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDzchar_set_cache_metrics2)))]
    | 100%positive => []
    | 101%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
    | 102%positive => []
    | 103%positive => [(*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_metrics2)))]
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)));
                       (*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzchar_set_cache_metrics2))) (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))]
    | 108%positive => []
    | 109%positive => []
    | 110%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_z)));
                       (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_metrics2)));
                       (*-6 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))]
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                  + (s IDzchar_set_cache_nparams))) (F_check_ge (0) (0));
                       (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - (s IDzchar_set_cache__tmp))) (F_check_ge (0) (0))]
    | 118%positive => []
    | 119%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_z)));
                       (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_metrics2)));
                       (*0 6*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   - 
                                                                   (s IDzchar_set_cache_metrics2))) (F_check_ge (1
                                                                    - (s IDzchar_set_cache_metrics2)) (0))]
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDzchar_set_cache_z))) (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))]
    | 127%positive => []
    | 128%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_i)
                                                                    + (s IDzchar_set_cache_nparams)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_i)
                                                                    + (s IDzchar_set_cache_nparams)))]
    | 129%positive => []
    | 130%positive => []
    | 131%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDzchar_set_cache_i)
                                                              + (s IDzchar_set_cache_nparams)) (-1
                                                                    - (s IDzchar_set_cache_i)
                                                                    + (s IDzchar_set_cache_nparams)));
                       (*-1 0*) F_max0_ge_0 (-1 - (s IDzchar_set_cache_i)
                                             + (s IDzchar_set_cache_nparams));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzchar_set_cache_z))) (F_check_ge ((s IDzchar_set_cache_z)) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + 
                                                                    (s IDzchar_set_cache_nparams))) (F_check_ge (-6
                                                                    + (s IDzchar_set_cache_nparams)) (0))]
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | 138%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_z)));
                       (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDzchar_set_cache_z))) (F_check_ge (-1
                                                                    + (s IDzchar_set_cache_z)) (0))]
    | 139%positive => []
    | 140%positive => []
    | 141%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                  + (s IDzchar_set_cache_nparams))) (F_check_ge (0) (0));
                       (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    - (s IDzchar_set_cache__tmp))) (F_check_ge (0) (0))]
    | 142%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
    | 143%positive => []
    | 144%positive => []
    | 145%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0));
                       (*-6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzchar_set_cache_metrics2)) (0))) (F_max0_ge_0 ((s IDzchar_set_cache_metrics2)));
                       (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                  - (s IDzchar_set_cache_metrics2))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                  + (s IDzchar_set_cache_nparams))) (F_check_ge (0) (0))]
    | 146%positive => []
    | 147%positive => []
    | 148%positive => []
    | 149%positive => [(*-10 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzchar_set_cache_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzchar_set_cache_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzchar_set_cache_z)))]
    | 150%positive => []
    | _ => []
  end.


Theorem zchar_set_cache_ai_correct:
  forall s p' s', steps (g_start zchar_set_cache) s (g_edges zchar_set_cache) p' s' -> zchar_set_cache_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zchar_set_cache_pot_correct:
  forall s p' s',
    steps (g_start zchar_set_cache) s (g_edges zchar_set_cache) p' s' ->
    (zchar_set_cache_pot (g_start zchar_set_cache) s >= zchar_set_cache_pot p' s')%Q.
Proof.
  check_lp zchar_set_cache_ai_correct zchar_set_cache_hints.
Qed.

