Require Import pasta.Pasta.

Notation IDscale_bitcount_z := 1%positive.
Notation IDscale_bitcount_cod_info_dref_off16 := 2%positive.
Notation IDscale_bitcount_cod_info_dref_off24 := 3%positive.
Notation IDscale_bitcount_cod_info_dref_off64 := 4%positive.
Notation IDscale_bitcount_cod_info_dref_off76 := 5%positive.
Notation IDscale_bitcount_ep := 6%positive.
Notation IDscale_bitcount_i := 7%positive.
Notation IDscale_bitcount_k := 8%positive.
Notation IDscale_bitcount_max_slen1 := 9%positive.
Notation IDscale_bitcount_max_slen2 := 10%positive.
Notation IDscale_bitcount_sfb := 11%positive.
Notation IDscale_bitcount_cod_info := 12%positive.
Notation IDscale_bitcount_scalefac := 13%positive.
Definition scale_bitcount : graph := {|
  g_start := 1%positive;
  g_end := 88%positive;
  g_edges := (1%positive,(AAssign IDscale_bitcount_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDscale_bitcount_max_slen1
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDscale_bitcount_max_slen2
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDscale_bitcount_ep (Some (ENum (2)))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_cod_info_dref_off24)
             s) = (eval (ENum (2)) s))%Z)),75%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_cod_info_dref_off24)
             s) <> (eval (ENum (2)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (11)) s))%Z)),65%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (11)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_cod_info_dref_off64)
             s) <> (eval (ENum (0)) s))%Z)),48%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_cod_info_dref_off64)
             s) = (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (11)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (21)) s))%Z)),20%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (21)) s))%Z)),19%positive)::
             (19%positive,AWeaken,30%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,28%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),27%positive)::
             (27%positive,AWeaken,18%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) =
             (eval (ENum (21)) s))%Z)),32%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <>
             (eval (ENum (21)) s))%Z)),31%positive)::
             (31%positive,AWeaken,40%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDscale_bitcount_cod_info_dref_off64
             (Some (ENum (1)))),34%positive)::
             (34%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (11)))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (21)) s))%Z)),41%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (21)) s))%Z)),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,49%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),47%positive)::
             (47%positive,AWeaken,37%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (11)))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (21)) s))%Z)),55%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (21)) s))%Z)),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,82%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,ANone,57%positive)::
             (56%positive,ANone,59%positive)::
             (57%positive,(AAssign IDscale_bitcount_max_slen2 None),
             58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),64%positive)::
             (64%positive,AWeaken,52%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,ANone,67%positive)::
             (66%positive,ANone,69%positive)::
             (67%positive,(AAssign IDscale_bitcount_max_slen1 None),
             68%positive)::(68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             71%positive)::(71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),74%positive)::
             (74%positive,AWeaken,11%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AAssign IDscale_bitcount_i (Some (ENum (0)))),
             77%positive)::(77%positive,ANone,78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard (fun s => ((eval (EVar IDscale_bitcount_i)
             s) < (eval (ENum (3)) s))%Z)),105%positive)::
             (79%positive,(AGuard (fun s => ((eval (EVar IDscale_bitcount_i)
             s) >= (eval (ENum (3)) s))%Z)),80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,ANone,82%positive)::
             (82%positive,(AAssign IDscale_bitcount_cod_info_dref_off76
             None),83%positive)::
             (83%positive,(AAssign IDscale_bitcount_k (Some (ENum (0)))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDscale_bitcount_k)
             s) < (eval (ENum (16)) s))%Z)),89%positive)::
             (86%positive,(AGuard (fun s => ((eval (EVar IDscale_bitcount_k)
             s) >= (eval (ENum (16)) s))%Z)),87%positive)::
             (87%positive,AWeaken,88%positive)::
             (89%positive,AWeaken,90%positive)::
             (90%positive,ANone,91%positive)::
             (90%positive,ANone,99%positive)::
             (91%positive,AWeaken,92%positive)::
             (92%positive,ANone,93%positive)::
             (92%positive,ANone,99%positive)::
             (93%positive,AWeaken,94%positive)::
             (94%positive,ANone,95%positive)::
             (94%positive,ANone,99%positive)::
             (95%positive,(AAssign IDscale_bitcount_cod_info_dref_off76
             None),96%positive)::
             (96%positive,(AAssign IDscale_bitcount_cod_info_dref_off16
             (Some (EVar IDscale_bitcount_k))),97%positive)::
             (97%positive,(AAssign IDscale_bitcount_ep (Some (ENum (0)))),
             98%positive)::(98%positive,ANone,99%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,(AAssign IDscale_bitcount_k
             (Some (EAdd (EVar IDscale_bitcount_k) (ENum (1))))),
             101%positive)::(101%positive,ANone,102%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),104%positive)::
             (104%positive,AWeaken,86%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (0)))),
             107%positive)::(107%positive,ANone,108%positive)::
             (108%positive,AWeaken,109%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (6)) s))%Z)),132%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (6)) s))%Z)),110%positive)::
             (110%positive,AWeaken,111%positive)::
             (111%positive,(AAssign IDscale_bitcount_sfb (Some (ENum (6)))),
             112%positive)::(112%positive,ANone,113%positive)::
             (113%positive,AWeaken,114%positive)::
             (114%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) <
             (eval (ENum (12)) s))%Z)),122%positive)::
             (114%positive,(AGuard
             (fun s => ((eval (EVar IDscale_bitcount_sfb) s) >=
             (eval (ENum (12)) s))%Z)),115%positive)::
             (115%positive,AWeaken,116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,(AAssign IDscale_bitcount_i
             (Some (EAdd (EVar IDscale_bitcount_i) (ENum (1))))),
             118%positive)::(118%positive,ANone,119%positive)::
             (119%positive,ANone,120%positive)::
             (120%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),121%positive)::
             (121%positive,AWeaken,79%positive)::
             (122%positive,AWeaken,123%positive)::
             (123%positive,ANone,124%positive)::
             (123%positive,ANone,126%positive)::
             (124%positive,(AAssign IDscale_bitcount_max_slen2 None),
             125%positive)::(125%positive,ANone,126%positive)::
             (126%positive,ANone,127%positive)::
             (127%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             128%positive)::(128%positive,ANone,129%positive)::
             (129%positive,ANone,130%positive)::
             (130%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),131%positive)::
             (131%positive,AWeaken,114%positive)::
             (132%positive,AWeaken,133%positive)::
             (133%positive,ANone,134%positive)::
             (133%positive,ANone,136%positive)::
             (134%positive,(AAssign IDscale_bitcount_max_slen1 None),
             135%positive)::(135%positive,ANone,136%positive)::
             (136%positive,ANone,137%positive)::
             (137%positive,(AAssign IDscale_bitcount_sfb
             (Some (EAdd (EVar IDscale_bitcount_sfb) (ENum (1))))),
             138%positive)::(138%positive,ANone,139%positive)::
             (139%positive,ANone,140%positive)::
             (140%positive,(AAssign IDscale_bitcount_z (Some (EAdd (ENum (1))
             (EVar IDscale_bitcount_z)))),141%positive)::
             (141%positive,AWeaken,109%positive)::nil
|}.

Definition scale_bitcount_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 3%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0)%Z
    | 4%positive => (-1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 5%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0)%Z
    | 6%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 7%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0)%Z
    | 8%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 9%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0)%Z
    | 10%positive => (-1 * (s IDscale_bitcount_sfb) <= 0 /\ 1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 11%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 12%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0)%Z
    | 13%positive => (-1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 14%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0)%Z
    | 15%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 16%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0)%Z
    | 17%positive => (-1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 18%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 19%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 21 <= 0)%Z
    | 20%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 21%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 22%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 23%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 24%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 25%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 26%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 27%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 29%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 30%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 31%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 32%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 21 <= 0)%Z
    | 33%positive => (-1 * (s IDscale_bitcount_sfb) + 21 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 34%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 21 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0)%Z
    | 36%positive => (-1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 38%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 21 <= 0)%Z
    | 39%positive => (-1 * (s IDscale_bitcount_sfb) + 21 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 40%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off64) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 41%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 42%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0)%Z
    | 43%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 44%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 45%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 46%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 47%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 48%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0)%Z
    | 49%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 50%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0)%Z
    | 51%positive => (-1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 52%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 53%positive => (1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 21 <= 0)%Z
    | 54%positive => (-1 * (s IDscale_bitcount_sfb) + 21 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0)%Z
    | 55%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 56%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 57%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 58%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 59%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -20 <= 0)%Z
    | 60%positive => (1 * (s IDscale_bitcount_sfb) + -20 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 11 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 61%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 62%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 63%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 64%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -21 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -10 <= 0)%Z
    | 66%positive => (1 * (s IDscale_bitcount_sfb) + -10 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 67%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -10 <= 0)%Z
    | 68%positive => (1 * (s IDscale_bitcount_sfb) + -10 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 69%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -10 <= 0)%Z
    | 70%positive => (1 * (s IDscale_bitcount_sfb) + -10 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 71%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDscale_bitcount_sfb) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 73%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDscale_bitcount_sfb) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0)%Z
    | 76%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 77%positive => (-1 * (s IDscale_bitcount_max_slen2) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0)%Z
    | 78%positive => (-1 * (s IDscale_bitcount_i) <= 0 /\ 1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_max_slen1) <= 0 /\ 1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_max_slen2) <= 0 /\ -1 * (s IDscale_bitcount_max_slen2) <= 0)%Z
    | 79%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0)%Z
    | 80%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) + 3 <= 0)%Z
    | 81%positive => (-1 * (s IDscale_bitcount_i) + 3 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0)%Z
    | 82%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0)%Z
    | 83%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0)%Z
    | 84%positive => (-1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0)%Z
    | 85%positive => (-1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0)%Z
    | 86%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0)%Z
    | 87%positive => (1 * (s IDscale_bitcount_k) + -16 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) + 16 <= 0)%Z
    | 88%positive => (-1 * (s IDscale_bitcount_k) + 16 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0)%Z
    | 89%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0)%Z
    | 90%positive => (1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 91%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0)%Z
    | 92%positive => (1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 93%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0)%Z
    | 94%positive => (1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 95%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0)%Z
    | 96%positive => (1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 97%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off16) + -15 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off16) <= 0)%Z
    | 98%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off16) <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off16) + -15 <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) <= 0 /\ -1 * (s IDscale_bitcount_ep) <= 0)%Z
    | 99%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -15 <= 0)%Z
    | 100%positive => (1 * (s IDscale_bitcount_k) + -15 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_k) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 101%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0 /\ -1 * (s IDscale_bitcount_k) + 1 <= 0)%Z
    | 102%positive => (-1 * (s IDscale_bitcount_k) + 1 <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 103%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0 /\ -1 * (s IDscale_bitcount_k) + 1 <= 0)%Z
    | 104%positive => (-1 * (s IDscale_bitcount_k) + 1 <= 0 /\ 1 * (s IDscale_bitcount_k) + -16 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_i) + -2 <= 0)%Z
    | 106%positive => (1 * (s IDscale_bitcount_i) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0)%Z
    | 107%positive => (-1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_i) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0)%Z
    | 108%positive => (-1 * (s IDscale_bitcount_sfb) <= 0 /\ 1 * (s IDscale_bitcount_sfb) <= 0 /\ 1 * (s IDscale_bitcount_i) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0)%Z
    | 109%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0)%Z
    | 110%positive => (1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0)%Z
    | 111%positive => (-1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0)%Z
    | 112%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0)%Z
    | 113%positive => (-1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 114%positive => (-1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0)%Z
    | 115%positive => (1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 116%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0)%Z
    | 117%positive => (1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 118%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_i) + 1 <= 0)%Z
    | 119%positive => (-1 * (s IDscale_bitcount_i) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0)%Z
    | 120%positive => (-1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_i) + 1 <= 0)%Z
    | 121%positive => (-1 * (s IDscale_bitcount_i) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 12 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 122%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 123%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 124%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 125%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 126%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -11 <= 0)%Z
    | 127%positive => (1 * (s IDscale_bitcount_sfb) + -11 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 6 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 128%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 7 <= 0)%Z
    | 129%positive => (-1 * (s IDscale_bitcount_sfb) + 7 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 130%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 7 <= 0)%Z
    | 131%positive => (-1 * (s IDscale_bitcount_sfb) + 7 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -12 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | 132%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -5 <= 0)%Z
    | 133%positive => (1 * (s IDscale_bitcount_sfb) + -5 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 134%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -5 <= 0)%Z
    | 135%positive => (1 * (s IDscale_bitcount_sfb) + -5 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 136%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -5 <= 0)%Z
    | 137%positive => (1 * (s IDscale_bitcount_sfb) + -5 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_sfb) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 138%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 1 <= 0)%Z
    | 139%positive => (-1 * (s IDscale_bitcount_sfb) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0)%Z
    | 140%positive => (1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_z) <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_sfb) + 1 <= 0)%Z
    | 141%positive => (-1 * (s IDscale_bitcount_sfb) + 1 <= 0 /\ 1 * (s IDscale_bitcount_sfb) + -6 <= 0 /\ -1 * (s IDscale_bitcount_i) <= 0 /\ -1 * (s IDscale_bitcount_cod_info_dref_off24) + 2 <= 0 /\ 1 * (s IDscale_bitcount_cod_info_dref_off24) + -2 <= 0 /\ -1 * (s IDscale_bitcount_ep) + 2 <= 0 /\ 1 * (s IDscale_bitcount_ep) + -2 <= 0 /\ -1 * (s IDscale_bitcount_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition scale_bitcount_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((55 # 1)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 2%positive => ((55 # 1) + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 3%positive => ((55 # 1) + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 4%positive => ((55 # 1) + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 5%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                     + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64))
                     + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 6%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                     + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64))
                     + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 7%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                     + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64))
                     + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 8%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                     + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64))
                     + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 9%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                     - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                     + (2 # 1) * max0(1
                                      - (s IDscale_bitcount_cod_info_dref_off64))
                     + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 10%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 11%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 12%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 13%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 14%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 15%positive => ((26 # 1)
                      + (2 # 1) * (s IDscale_bitcount_cod_info_dref_off64)
                      + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (2 # 1) * max0(-(s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 16%positive => ((37 # 1)
                      + (2 # 1) * (s IDscale_bitcount_cod_info_dref_off64)
                      + (9 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (2 # 1) * max0(-(s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 17%positive => ((37 # 1)
                      + (2 # 1) * (s IDscale_bitcount_cod_info_dref_off64)
                      + (9 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (2 # 1) * max0(-(s IDscale_bitcount_cod_info_dref_off64)))%Q
    | 18%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 19%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 20%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 21%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 22%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 23%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 24%positive => ((48 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 25%positive => ((48 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 26%positive => ((48 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 27%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 28%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 29%positive => ((47 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 30%positive => ((26 # 1) - (36 # 7) * (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0((s IDscale_bitcount_ep))
                      + (36 # 7) * max0((s IDscale_bitcount_sfb)))%Q
    | 31%positive => ((26 # 1) - (36 # 7) * (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0((s IDscale_bitcount_ep))
                      + (36 # 7) * max0((s IDscale_bitcount_sfb)))%Q
    | 32%positive => ((26 # 1) - (36 # 7) * (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0((s IDscale_bitcount_ep))
                      + (36 # 7) * max0((s IDscale_bitcount_sfb)))%Q
    | 33%positive => ((36 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 34%positive => ((36 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 35%positive => ((26 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + max0(21 - (s IDscale_bitcount_sfb))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 36%positive => ((26 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + max0(21 - (s IDscale_bitcount_sfb))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 37%positive => ((26 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + max0(21 - (s IDscale_bitcount_sfb))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 38%positive => ((26 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + max0(21 - (s IDscale_bitcount_sfb))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 39%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      - (9 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 40%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      - (9 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 41%positive => ((26 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + max0(21 - (s IDscale_bitcount_sfb))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 42%positive => ((47 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 43%positive => ((47 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 44%positive => ((48 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 45%positive => ((48 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 46%positive => ((48 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 47%positive => ((47 # 1) - (5 # 1) * (s IDscale_bitcount_ep)
                      - (s IDscale_bitcount_sfb) + (s IDscale_bitcount_z)
                      + (5 # 1) * max0(-2 + (s IDscale_bitcount_ep))
                      + (5 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 48%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 49%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      - (9 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 50%positive => ((16 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + max0(21 - (s IDscale_bitcount_sfb))
                      - (9 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 51%positive => ((16 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + max0(21 - (s IDscale_bitcount_sfb))
                      - (9 # 1) * max0((s IDscale_bitcount_ep)))%Q
    | 52%positive => ((16 # 1) + (s IDscale_bitcount_z)
                      + max0(21 - (s IDscale_bitcount_sfb)))%Q
    | 53%positive => ((16 # 1) + (s IDscale_bitcount_z)
                      + max0(21 - (s IDscale_bitcount_sfb)))%Q
    | 54%positive => ((16 # 1) + (s IDscale_bitcount_z))%Q
    | 55%positive => ((16 # 1) + (s IDscale_bitcount_z)
                      + max0(21 - (s IDscale_bitcount_sfb)))%Q
    | 56%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 57%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 58%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 59%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 60%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 61%positive => ((38 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 62%positive => ((38 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 63%positive => ((38 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 64%positive => ((37 # 1) - (s IDscale_bitcount_sfb)
                      + (s IDscale_bitcount_z))%Q
    | 65%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 66%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 67%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 68%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 69%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 70%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 71%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 72%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 73%positive => ((26 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 74%positive => ((25 # 1) + (9 # 1) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (9 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 75%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 76%positive => ((10 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (45 # 2) * max0(2 - (s IDscale_bitcount_ep)))%Q
    | 77%positive => (-(29 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (45 # 2) * max0(2 - (s IDscale_bitcount_ep))
                      + (13 # 1) * max0(3 - (s IDscale_bitcount_i)))%Q
    | 78%positive => (-(29 # 1) + (45 # 2) * (s IDscale_bitcount_ep)
                      + (s IDscale_bitcount_z)
                      + (2 # 1) * max0(1
                                       - (s IDscale_bitcount_cod_info_dref_off64))
                      + (45 # 2) * max0(2 - (s IDscale_bitcount_ep))
                      + (13 # 1) * max0(3 - (s IDscale_bitcount_i)))%Q
    | 79%positive => ((8 # 1) * (s IDscale_bitcount_ep)
                      + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                      + max0((s IDscale_bitcount_z)))%Q
    | 80%positive => ((8 # 1) * (s IDscale_bitcount_ep)
                      + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                      + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                      + max0((s IDscale_bitcount_z)))%Q
    | 81%positive => ((16 # 1) + (s IDscale_bitcount_z))%Q
    | 82%positive => ((16 # 1) + (s IDscale_bitcount_z))%Q
    | 83%positive => ((16 # 1) + (s IDscale_bitcount_z))%Q
    | 84%positive => ((s IDscale_bitcount_z)
                      + max0(16 - (s IDscale_bitcount_k)))%Q
    | 85%positive => ((s IDscale_bitcount_z)
                      + max0(16 - (s IDscale_bitcount_k)))%Q
    | 86%positive => ((s IDscale_bitcount_z)
                      + max0(16 - (s IDscale_bitcount_k)))%Q
    | 87%positive => ((s IDscale_bitcount_z)
                      + max0(16 - (s IDscale_bitcount_k)))%Q
    | 88%positive => ((s IDscale_bitcount_z))%Q
    | 89%positive => ((s IDscale_bitcount_z)
                      + max0(16 - (s IDscale_bitcount_k)))%Q
    | 90%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 91%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 92%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 93%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 94%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 95%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 96%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 97%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 98%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 99%positive => ((16 # 1) - (s IDscale_bitcount_k)
                      + (s IDscale_bitcount_z))%Q
    | 100%positive => ((16 # 1) - (s IDscale_bitcount_k)
                       + (s IDscale_bitcount_z))%Q
    | 101%positive => ((17 # 1) - (s IDscale_bitcount_k)
                       + (s IDscale_bitcount_z))%Q
    | 102%positive => ((17 # 1) - (s IDscale_bitcount_k)
                       + (s IDscale_bitcount_z))%Q
    | 103%positive => ((17 # 1) - (s IDscale_bitcount_k)
                       + (s IDscale_bitcount_z))%Q
    | 104%positive => ((16 # 1) - (s IDscale_bitcount_k)
                       + (s IDscale_bitcount_z))%Q
    | 105%positive => ((8 # 1) * (s IDscale_bitcount_ep)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0((s IDscale_bitcount_z)))%Q
    | 106%positive => ((8 # 1) * (s IDscale_bitcount_ep)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0((s IDscale_bitcount_z)))%Q
    | 107%positive => (-(12 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb))
                       + max0((s IDscale_bitcount_z)))%Q
    | 108%positive => (-(12 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb))
                       + max0((s IDscale_bitcount_z)))%Q
    | 109%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 110%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 111%positive => ((7 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i)))%Q
    | 112%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 113%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 114%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 115%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 116%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 117%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 118%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 119%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 120%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 121%positive => ((8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(3 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 122%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 123%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 124%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 125%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 126%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 127%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 128%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 129%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 130%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 131%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 132%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 133%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 134%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 135%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 136%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 137%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(11 - (s IDscale_bitcount_sfb)))%Q
    | 138%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 139%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 140%positive => ((2 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | 141%positive => ((1 # 1) + (8 # 1) * (s IDscale_bitcount_ep)
                       + (s IDscale_bitcount_z)
                       + (8 # 1) * max0(2 - (s IDscale_bitcount_ep))
                       + (13 # 1) * max0(2 - (s IDscale_bitcount_i))
                       + max0(12 - (s IDscale_bitcount_sfb)))%Q
    | _ => (0 # 1)%Q
  end.

Definition scale_bitcount_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                                    - (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (11
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-13.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                    - (s IDscale_bitcount_ep))) (F_check_ge (2
                                                                    - (s IDscale_bitcount_ep)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (11
                                                             - (s IDscale_bitcount_sfb)) (10
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-1 0*) F_max0_ge_0 (10 - (s IDscale_bitcount_sfb));
                      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDscale_bitcount_cod_info_dref_off64)) (0))) (F_max0_ge_0 (-
                                                                    (s IDscale_bitcount_cod_info_dref_off64)))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_ep)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_ep)));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDscale_bitcount_cod_info_dref_off64))) (F_check_ge (0) (0));
                      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                   - 
                                                                   (s IDscale_bitcount_ep))) (F_check_ge (2
                                                                    - (s IDscale_bitcount_ep)) (0));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   - 
                                                                   (s IDscale_bitcount_cod_info_dref_off64))) (F_check_ge (1
                                                                    - (s IDscale_bitcount_cod_info_dref_off64)) (0))]
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDscale_bitcount_sfb)) (20
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-1 0*) F_max0_ge_0 (20 - (s IDscale_bitcount_sfb));
                      (*-5.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_sfb)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                                    - (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (21
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                 - (s IDscale_bitcount_ep))) (F_check_ge (0) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1.05263 0*) F_max0_ge_0 (20
                                                  - (s IDscale_bitcount_sfb));
                      (*-5.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_sfb)));
                      (*0 1.05263*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                 - (s IDscale_bitcount_ep))) (F_check_ge (0) (0));
                      (*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDscale_bitcount_sfb))) (F_check_ge (0) (0));
                      (*0 0.0526316*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDscale_bitcount_sfb)))]
    | 30%positive => []
    | 31%positive => [(*-5.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_sfb))) (F_check_ge ((s IDscale_bitcount_sfb)) (0));
                      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDscale_bitcount_ep))) (F_check_ge (0) (0));
                      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_ep))) (F_check_ge ((s IDscale_bitcount_ep)) (0))]
    | 32%positive => [(*-5.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_sfb))) (F_check_ge ((s IDscale_bitcount_sfb)) (0));
                      (*0 5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDscale_bitcount_ep)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDscale_bitcount_ep)))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDscale_bitcount_sfb)) (20
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-1 0*) F_max0_ge_0 (20 - (s IDscale_bitcount_sfb));
                      (*-14 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_ep))) (F_check_ge ((s IDscale_bitcount_ep)) (0));
                      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                 + (s IDscale_bitcount_ep))) (F_check_ge (0) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (21
                                                                   - 
                                                                   (s IDscale_bitcount_sfb))) (F_check_ge (21
                                                                    - (s IDscale_bitcount_sfb)) (0))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                                    - (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (21
                                                                    - (s IDscale_bitcount_sfb)))]
    | 48%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (11
                                                             - (s IDscale_bitcount_sfb)) (10
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-1 0*) F_max0_ge_0 (10 - (s IDscale_bitcount_sfb));
                      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDscale_bitcount_ep))) (F_check_ge (0) (0));
                      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                 - (s IDscale_bitcount_ep))) (F_check_ge (0) (0));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDscale_bitcount_cod_info_dref_off64))) (F_check_ge (0) (0))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-9 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_ep)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_ep)))]
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDscale_bitcount_sfb)) (20
                                                                    - (s IDscale_bitcount_sfb)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                                 - (s IDscale_bitcount_sfb))) (F_check_ge (0) (0))]
    | 54%positive => []
    | 55%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (21
                                                                  - (s IDscale_bitcount_sfb))) (F_check_ge (21
                                                                    - (s IDscale_bitcount_sfb)) (0))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                                    - (s IDscale_bitcount_sfb)) (0))) (F_max0_ge_0 (21
                                                                    - (s IDscale_bitcount_sfb)))]
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*-1 0*) F_max0_pre_decrement (12
                                                     - (s IDscale_bitcount_sfb)) (1)]
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_z)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_z)));
                      (*-14.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                    - (s IDscale_bitcount_ep))) (F_check_ge (2
                                                                    - (s IDscale_bitcount_ep)) (0));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDscale_bitcount_cod_info_dref_off64))) (F_check_ge (0) (0))]
    | 79%positive => []
    | 80%positive => [(*-13 0*) F_max0_monotonic (F_check_ge (3
                                                              - (s IDscale_bitcount_i)) (2
                                                                    - (s IDscale_bitcount_i)));
                      (*-13 0*) F_max0_ge_0 (2 - (s IDscale_bitcount_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_z))) (F_check_ge ((s IDscale_bitcount_z)) (0));
                      (*0 8*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                  - (s IDscale_bitcount_ep))) (F_check_ge (2
                                                                    - (s IDscale_bitcount_ep)) (0))]
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDscale_bitcount_k)) (15
                                                                    - (s IDscale_bitcount_k)));
                      (*-1 0*) F_max0_ge_0 (15 - (s IDscale_bitcount_k))]
    | 88%positive => []
    | 89%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                   - 
                                                                   (s IDscale_bitcount_k))) (F_check_ge (16
                                                                    - (s IDscale_bitcount_k)) (0))]
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
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - (s IDscale_bitcount_k)) (0))) (F_max0_ge_0 (16
                                                                    - (s IDscale_bitcount_k)))]
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => [(*-13 0*) F_max0_pre_decrement (3
                                                       - (s IDscale_bitcount_i)) (1);
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDscale_bitcount_z))) (F_check_ge ((s IDscale_bitcount_z)) (0))]
    | 109%positive => []
    | 110%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (12
                                                              - (s IDscale_bitcount_sfb)) (6))]
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
    | 121%positive => [(*0 1*) F_max0_monotonic (F_check_ge (12
                                                             - (s IDscale_bitcount_sfb)) (11
                                                                    - (s IDscale_bitcount_sfb)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDscale_bitcount_z)) (0))) (F_max0_ge_0 ((s IDscale_bitcount_z)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (11
                                                                  - (s IDscale_bitcount_sfb))) (F_check_ge (0) (0))]
    | 122%positive => [(*0 1*) F_max0_pre_decrement (12
                                                     - (s IDscale_bitcount_sfb)) (1)]
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => []
    | 131%positive => []
    | 132%positive => [(*-1 2.15016e-12*) F_max0_pre_decrement (12
                                                                - (s IDscale_bitcount_sfb)) (1)]
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | 138%positive => []
    | 139%positive => []
    | 140%positive => []
    | 141%positive => []
    | _ => []
  end.


Theorem scale_bitcount_ai_correct:
  forall s p' s', steps (g_start scale_bitcount) s (g_edges scale_bitcount) p' s' -> scale_bitcount_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem scale_bitcount_pot_correct:
  forall s p' s',
    steps (g_start scale_bitcount) s (g_edges scale_bitcount) p' s' ->
    (scale_bitcount_pot (g_start scale_bitcount) s >= scale_bitcount_pot p' s')%Q.
Proof.
  check_lp scale_bitcount_ai_correct scale_bitcount_hints.
Qed.

