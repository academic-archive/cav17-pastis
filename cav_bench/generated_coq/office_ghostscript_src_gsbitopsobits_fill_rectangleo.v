Require Import pasta.Pasta.

Notation IDbits_fill_rectangle_z := 1%positive.
Notation IDbits_fill_rectangle__tmp := 2%positive.
Notation IDbits_fill_rectangle__tmp1 := 3%positive.
Notation IDbits_fill_rectangle__tmp2 := 4%positive.
Notation IDbits_fill_rectangle__tmp3 := 5%positive.
Notation IDbits_fill_rectangle__tmp4 := 6%positive.
Notation IDbits_fill_rectangle_bit := 7%positive.
Notation IDbits_fill_rectangle_byte_count := 8%positive.
Notation IDbits_fill_rectangle_last := 9%positive.
Notation IDbits_fill_rectangle_last_bit := 10%positive.
Notation IDbits_fill_rectangle_line_count := 11%positive.
Notation IDbits_fill_rectangle_line_count1 := 12%positive.
Notation IDbits_fill_rectangle_line_count11 := 13%positive.
Notation IDbits_fill_rectangle_line_count13 := 14%positive.
Notation IDbits_fill_rectangle_line_count15 := 15%positive.
Notation IDbits_fill_rectangle_line_count17 := 16%positive.
Notation IDbits_fill_rectangle_line_count19 := 17%positive.
Notation IDbits_fill_rectangle_line_count21 := 18%positive.
Notation IDbits_fill_rectangle_line_count3 := 19%positive.
Notation IDbits_fill_rectangle_line_count5 := 20%positive.
Notation IDbits_fill_rectangle_line_count7 := 21%positive.
Notation IDbits_fill_rectangle_line_count9 := 22%positive.
Notation IDbits_fill_rectangle_mask := 23%positive.
Notation IDbits_fill_rectangle_right_mask := 24%positive.
Notation IDbits_fill_rectangle_dest := 25%positive.
Notation IDbits_fill_rectangle_dest_bit := 26%positive.
Notation IDbits_fill_rectangle_draster := 27%positive.
Notation IDbits_fill_rectangle_height := 28%positive.
Notation IDbits_fill_rectangle_pattern := 29%positive.
Notation IDbits_fill_rectangle_width_bits := 30%positive.
Definition bits_fill_rectangle : graph := {|
  g_start := 1%positive;
  g_end := 164%positive;
  g_edges := (1%positive,(AAssign IDbits_fill_rectangle_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbits_fill_rectangle__tmp4
             (Some (EVar IDbits_fill_rectangle_dest_bit))),3%positive)::
             (3%positive,(AAssign IDbits_fill_rectangle__tmp3
             (Some (EVar IDbits_fill_rectangle_draster))),4%positive)::
             (4%positive,(AAssign IDbits_fill_rectangle__tmp2
             (Some (EVar IDbits_fill_rectangle_pattern))),5%positive)::
             (5%positive,(AAssign IDbits_fill_rectangle__tmp1
             (Some (EVar IDbits_fill_rectangle_width_bits))),6%positive)::
             (6%positive,(AAssign IDbits_fill_rectangle__tmp
             (Some (EVar IDbits_fill_rectangle_height))),7%positive)::
             (7%positive,(AAssign IDbits_fill_rectangle_bit None),8%positive)::
             (8%positive,(AAssign IDbits_fill_rectangle_last_bit
             (Some (ESub (EAdd (EVar IDbits_fill_rectangle__tmp1)
             (EVar IDbits_fill_rectangle_bit)) (ENum (33))))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDbits_fill_rectangle_last_bit) s) <
             (eval (ENum (0)) s))%Z)),128%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDbits_fill_rectangle_last_bit) s) >=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDbits_fill_rectangle_last None),
             13%positive)::
             (13%positive,(AAssign IDbits_fill_rectangle_mask None),
             14%positive)::
             (14%positive,(AAssign IDbits_fill_rectangle_right_mask None),
             15%positive)::(15%positive,AWeaken,16%positive)::
             (16%positive,ANone,89%positive)::
             (16%positive,ANone,53%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,41%positive)::
             (18%positive,ANone,30%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDbits_fill_rectangle_line_count13
             (Some (EVar IDbits_fill_rectangle__tmp))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDbits_fill_rectangle_line_count13
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count13)
             (ENum (-1))))),23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count13)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),27%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count13)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,49%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             21%positive)::
             (30%positive,(AAssign IDbits_fill_rectangle_line_count11
             (Some (EVar IDbits_fill_rectangle__tmp))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDbits_fill_rectangle_line_count11
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count11)
             (ENum (-1))))),34%positive)::(34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count11)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),38%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count11)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,49%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             32%positive)::
             (41%positive,(AAssign IDbits_fill_rectangle_line_count15
             (Some (EVar IDbits_fill_rectangle__tmp))),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDbits_fill_rectangle_line_count15
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count15)
             (ENum (-1))))),45%positive)::(45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count15)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),50%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count15)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,123%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             43%positive)::(53%positive,AWeaken,54%positive)::
             (54%positive,ANone,77%positive)::
             (54%positive,ANone,66%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDbits_fill_rectangle_line_count7
             (Some (EVar IDbits_fill_rectangle__tmp))),56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDbits_fill_rectangle_line_count7
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count7)
             (ENum (-1))))),59%positive)::(59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count7)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),63%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count7)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,85%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             57%positive)::
             (66%positive,(AAssign IDbits_fill_rectangle_line_count5
             (Some (EVar IDbits_fill_rectangle__tmp))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDbits_fill_rectangle_line_count5
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count5)
             (ENum (-1))))),70%positive)::(70%positive,AWeaken,71%positive)::
             (71%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count5)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),74%positive)::
             (71%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count5)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),72%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,85%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             68%positive)::
             (77%positive,(AAssign IDbits_fill_rectangle_line_count9
             (Some (EVar IDbits_fill_rectangle__tmp))),78%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,(AAssign IDbits_fill_rectangle_line_count9
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count9)
             (ENum (-1))))),81%positive)::(81%positive,AWeaken,82%positive)::
             (82%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count9)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),86%positive)::
             (82%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count9)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),83%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,ANone,123%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             79%positive)::
             (89%positive,(AAssign IDbits_fill_rectangle_byte_count None),
             90%positive)::(90%positive,AWeaken,91%positive)::
             (91%positive,ANone,114%positive)::
             (91%positive,ANone,103%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,(AAssign IDbits_fill_rectangle_line_count19
             (Some (EVar IDbits_fill_rectangle__tmp))),93%positive)::
             (93%positive,ANone,94%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,(AAssign IDbits_fill_rectangle_line_count19
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count19)
             (ENum (-1))))),96%positive)::(96%positive,AWeaken,97%positive)::
             (97%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count19)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),100%positive)::
             (97%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count19)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),98%positive)::
             (98%positive,AWeaken,99%positive)::
             (99%positive,ANone,122%positive)::
             (100%positive,AWeaken,101%positive)::
             (101%positive,ANone,102%positive)::
             (102%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             94%positive)::
             (103%positive,(AAssign IDbits_fill_rectangle_line_count17
             (Some (EVar IDbits_fill_rectangle__tmp))),104%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,ANone,106%positive)::
             (106%positive,(AAssign IDbits_fill_rectangle_line_count17
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count17)
             (ENum (-1))))),107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count17)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),111%positive)::
             (108%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count17)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),109%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,ANone,122%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             105%positive)::
             (114%positive,(AAssign IDbits_fill_rectangle_line_count21
             (Some (EVar IDbits_fill_rectangle__tmp))),115%positive)::
             (115%positive,ANone,116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,(AAssign IDbits_fill_rectangle_line_count21
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count21)
             (ENum (-1))))),118%positive)::
             (118%positive,AWeaken,119%positive)::
             (119%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count21)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),125%positive)::
             (119%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count21)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),120%positive)::
             (120%positive,AWeaken,121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,AWeaken,164%positive)::
             (125%positive,AWeaken,126%positive)::
             (126%positive,ANone,127%positive)::
             (127%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             116%positive)::(128%positive,AWeaken,129%positive)::
             (129%positive,(AAssign IDbits_fill_rectangle_right_mask None),
             130%positive)::(130%positive,AWeaken,131%positive)::
             (131%positive,ANone,154%positive)::
             (131%positive,ANone,143%positive)::
             (131%positive,ANone,132%positive)::
             (132%positive,(AAssign IDbits_fill_rectangle_line_count1
             (Some (EVar IDbits_fill_rectangle__tmp))),133%positive)::
             (133%positive,ANone,134%positive)::
             (134%positive,ANone,135%positive)::
             (135%positive,(AAssign IDbits_fill_rectangle_line_count1
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count1)
             (ENum (-1))))),136%positive)::
             (136%positive,AWeaken,137%positive)::
             (137%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count1)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),140%positive)::
             (137%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count1)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),138%positive)::
             (138%positive,AWeaken,139%positive)::
             (139%positive,ANone,162%positive)::
             (140%positive,AWeaken,141%positive)::
             (141%positive,ANone,142%positive)::
             (142%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             134%positive)::
             (143%positive,(AAssign IDbits_fill_rectangle_line_count
             (Some (EVar IDbits_fill_rectangle__tmp))),144%positive)::
             (144%positive,ANone,145%positive)::
             (145%positive,ANone,146%positive)::
             (146%positive,(AAssign IDbits_fill_rectangle_line_count
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count)
             (ENum (-1))))),147%positive)::
             (147%positive,AWeaken,148%positive)::
             (148%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),151%positive)::
             (148%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),149%positive)::
             (149%positive,AWeaken,150%positive)::
             (150%positive,ANone,162%positive)::
             (151%positive,AWeaken,152%positive)::
             (152%positive,ANone,153%positive)::
             (153%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             145%positive)::
             (154%positive,(AAssign IDbits_fill_rectangle_line_count3
             (Some (EVar IDbits_fill_rectangle__tmp))),155%positive)::
             (155%positive,ANone,156%positive)::
             (156%positive,ANone,157%positive)::
             (157%positive,(AAssign IDbits_fill_rectangle_line_count3
             (Some (EAdd (EVar IDbits_fill_rectangle_line_count3)
             (ENum (-1))))),158%positive)::
             (158%positive,AWeaken,159%positive)::
             (159%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count3)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),165%positive)::
             (159%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbits_fill_rectangle_line_count3)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),160%positive)::
             (160%positive,AWeaken,161%positive)::
             (161%positive,ANone,162%positive)::
             (162%positive,ANone,163%positive)::
             (163%positive,AWeaken,164%positive)::
             (165%positive,AWeaken,166%positive)::
             (166%positive,ANone,167%positive)::
             (167%positive,(AAssign IDbits_fill_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbits_fill_rectangle_z)))),
             156%positive)::nil
|}.

Definition bits_fill_rectangle_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 4%positive => (1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 6%positive => (1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 7%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 8%positive => (1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 9%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 10%positive => (1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 11%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 12%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 13%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 14%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 15%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 16%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 17%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 18%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 19%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 20%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 21%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 22%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 23%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 24%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 25%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count13) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count13) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDbits_fill_rectangle_line_count13) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count13) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 27%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 28%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 29%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 30%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 31%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 32%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 33%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 34%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 35%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 36%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count11) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count11) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDbits_fill_rectangle_line_count11) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count11) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 38%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 39%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 40%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 41%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 42%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 43%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 44%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 45%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 46%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 47%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count15) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count15) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDbits_fill_rectangle_line_count15) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count15) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 49%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 50%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 51%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 52%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 53%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 54%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 55%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 56%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 57%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 58%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 59%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 60%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 61%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count7) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count7) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDbits_fill_rectangle_line_count7) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count7) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 63%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 64%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 65%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 66%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 67%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 68%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 69%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 70%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 71%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 72%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count5) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count5) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDbits_fill_rectangle_line_count5) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count5) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 74%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 75%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 76%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 77%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 78%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 79%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 80%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 81%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 82%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 83%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count9) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count9) + 1 <= 0)%Z
    | 84%positive => (-1 * (s IDbits_fill_rectangle_line_count9) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count9) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 85%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 86%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 87%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 88%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 89%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 90%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 91%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 92%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 93%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 94%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 95%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 96%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 97%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 98%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count19) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count19) + 1 <= 0)%Z
    | 99%positive => (-1 * (s IDbits_fill_rectangle_line_count19) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count19) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 100%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 101%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 102%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 103%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 104%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 105%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 106%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 107%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 108%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 109%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count17) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count17) + 1 <= 0)%Z
    | 110%positive => (-1 * (s IDbits_fill_rectangle_line_count17) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count17) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 111%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 112%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 113%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 114%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 115%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 116%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 117%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 118%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 119%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 120%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count21) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count21) + 1 <= 0)%Z
    | 121%positive => (-1 * (s IDbits_fill_rectangle_line_count21) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count21) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 122%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 123%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 124%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 125%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 126%positive => (-1 * (s IDbits_fill_rectangle_last_bit) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 127%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_last_bit) <= 0)%Z
    | 128%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 129%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 130%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 131%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 132%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 133%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 134%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 135%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 136%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 137%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 138%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count1) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count1) + 1 <= 0)%Z
    | 139%positive => (-1 * (s IDbits_fill_rectangle_line_count1) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count1) + -1 <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 140%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 141%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 142%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 143%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 144%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 145%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 146%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 147%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 148%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 149%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count) + 1 <= 0)%Z
    | 150%positive => (-1 * (s IDbits_fill_rectangle_line_count) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count) + -1 <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 151%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 152%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 153%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 154%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 155%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_z) <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 156%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 157%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 158%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 159%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 160%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count3) + -1 <= 0 /\ -1 * (s IDbits_fill_rectangle_line_count3) + 1 <= 0)%Z
    | 161%positive => (-1 * (s IDbits_fill_rectangle_line_count3) + 1 <= 0 /\ 1 * (s IDbits_fill_rectangle_line_count3) + -1 <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 162%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 163%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 164%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 165%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | 166%positive => (1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0 /\ -1 * (s IDbits_fill_rectangle_z) <= 0)%Z
    | 167%positive => (-1 * (s IDbits_fill_rectangle_z) <= 0 /\ 1 * (s IDbits_fill_rectangle_last_bit) + 1 <= 0)%Z
    | _ => False
  end.

Definition bits_fill_rectangle_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDbits_fill_rectangle_height))%Q
    | 2%positive => ((s IDbits_fill_rectangle_height))%Q
    | 3%positive => ((s IDbits_fill_rectangle_height))%Q
    | 4%positive => ((s IDbits_fill_rectangle_height))%Q
    | 5%positive => ((s IDbits_fill_rectangle_height))%Q
    | 6%positive => ((s IDbits_fill_rectangle_height))%Q
    | 7%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 8%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 9%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 10%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 11%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 12%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 13%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 14%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 15%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 16%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 17%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 18%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 19%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 20%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 21%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 22%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 23%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 24%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 25%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 26%positive => ((s IDbits_fill_rectangle_z))%Q
    | 27%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 28%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 29%positive => ((s IDbits_fill_rectangle_line_count13)
                      + (s IDbits_fill_rectangle_z))%Q
    | 30%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 31%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 32%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 33%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 34%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 35%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 36%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 37%positive => ((s IDbits_fill_rectangle_z))%Q
    | 38%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 39%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 40%positive => ((s IDbits_fill_rectangle_line_count11)
                      + (s IDbits_fill_rectangle_z))%Q
    | 41%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 42%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 43%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 44%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 45%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 46%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 47%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 48%positive => ((s IDbits_fill_rectangle_z))%Q
    | 49%positive => ((s IDbits_fill_rectangle_z))%Q
    | 50%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 51%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 52%positive => ((s IDbits_fill_rectangle_line_count15)
                      + (s IDbits_fill_rectangle_z))%Q
    | 53%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 54%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 55%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 56%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 57%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 58%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 59%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 60%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 61%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 62%positive => ((s IDbits_fill_rectangle_z))%Q
    | 63%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 64%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 65%positive => ((s IDbits_fill_rectangle_line_count7)
                      + (s IDbits_fill_rectangle_z))%Q
    | 66%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 67%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 68%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 69%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 70%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 71%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 72%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 73%positive => ((s IDbits_fill_rectangle_z))%Q
    | 74%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 75%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 76%positive => ((s IDbits_fill_rectangle_line_count5)
                      + (s IDbits_fill_rectangle_z))%Q
    | 77%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 78%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 79%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 80%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 81%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 82%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 83%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 84%positive => ((s IDbits_fill_rectangle_z))%Q
    | 85%positive => ((s IDbits_fill_rectangle_z))%Q
    | 86%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 87%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 88%positive => ((s IDbits_fill_rectangle_line_count9)
                      + (s IDbits_fill_rectangle_z))%Q
    | 89%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 90%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 91%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 92%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                      + (s IDbits_fill_rectangle_z))%Q
    | 93%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 94%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 95%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 96%positive => ((s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 97%positive => ((s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 98%positive => ((s IDbits_fill_rectangle_line_count19)
                      + (s IDbits_fill_rectangle_z))%Q
    | 99%positive => ((s IDbits_fill_rectangle_z))%Q
    | 100%positive => ((s IDbits_fill_rectangle_line_count19)
                       + (s IDbits_fill_rectangle_z))%Q
    | 101%positive => ((s IDbits_fill_rectangle_line_count19)
                       + (s IDbits_fill_rectangle_z))%Q
    | 102%positive => ((s IDbits_fill_rectangle_line_count19)
                       + (s IDbits_fill_rectangle_z))%Q
    | 103%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 104%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 105%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 106%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 107%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 108%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 109%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 110%positive => ((s IDbits_fill_rectangle_z))%Q
    | 111%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 112%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 113%positive => ((s IDbits_fill_rectangle_line_count17)
                       + (s IDbits_fill_rectangle_z))%Q
    | 114%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 115%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 116%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 117%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 118%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 119%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 120%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 121%positive => ((s IDbits_fill_rectangle_z))%Q
    | 122%positive => ((s IDbits_fill_rectangle_z))%Q
    | 123%positive => ((s IDbits_fill_rectangle_z))%Q
    | 124%positive => ((s IDbits_fill_rectangle_z))%Q
    | 125%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 126%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 127%positive => ((s IDbits_fill_rectangle_line_count21)
                       + (s IDbits_fill_rectangle_z))%Q
    | 128%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 129%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 130%positive => ((s IDbits_fill_rectangle__tmp))%Q
    | 131%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 132%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 133%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 134%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 135%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 136%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 137%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 138%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 139%positive => ((s IDbits_fill_rectangle_z))%Q
    | 140%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 141%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 142%positive => ((s IDbits_fill_rectangle_line_count1)
                       + (s IDbits_fill_rectangle_z))%Q
    | 143%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 144%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 145%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 146%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 147%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 148%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 149%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 150%positive => ((s IDbits_fill_rectangle_z))%Q
    | 151%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 152%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 153%positive => ((s IDbits_fill_rectangle_line_count)
                       + (s IDbits_fill_rectangle_z))%Q
    | 154%positive => (-(1 # 1) + (s IDbits_fill_rectangle__tmp)
                       + (s IDbits_fill_rectangle_z))%Q
    | 155%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 156%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 157%positive => (-(1 # 1) + (s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 158%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 159%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 160%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 161%positive => ((s IDbits_fill_rectangle_z))%Q
    | 162%positive => ((s IDbits_fill_rectangle_z))%Q
    | 163%positive => ((s IDbits_fill_rectangle_z))%Q
    | 164%positive => ((s IDbits_fill_rectangle_z))%Q
    | 165%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 166%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | 167%positive => ((s IDbits_fill_rectangle_line_count3)
                       + (s IDbits_fill_rectangle_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition bits_fill_rectangle_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDbits_fill_rectangle_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDbits_fill_rectangle_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDbits_fill_rectangle_z)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count13))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count13)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count13)))]
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
    | 36%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count11))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count11)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count11)))]
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
    | 47%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count15))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count15)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count15)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDbits_fill_rectangle_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDbits_fill_rectangle_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDbits_fill_rectangle_z)))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count7))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count7)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count7)))]
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
    | 72%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count5))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count5)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count5)))]
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
    | 83%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count9))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count9)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count9)))]
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDbits_fill_rectangle_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDbits_fill_rectangle_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDbits_fill_rectangle_z)))]
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDbits_fill_rectangle_line_count19))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count19)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count19)))]
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
    | 109%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDbits_fill_rectangle_line_count17))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count17)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count17)))]
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
    | 120%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDbits_fill_rectangle_line_count21))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count21)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count21)))]
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDbits_fill_rectangle_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDbits_fill_rectangle_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDbits_fill_rectangle_z)))]
    | 131%positive => []
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | 138%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDbits_fill_rectangle_line_count1))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count1)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count1)))]
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
    | 149%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDbits_fill_rectangle_line_count))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count)))]
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
    | 160%positive => [(*-1 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDbits_fill_rectangle_line_count3))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbits_fill_rectangle_line_count3)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbits_fill_rectangle_line_count3)))]
    | 161%positive => []
    | 162%positive => []
    | 163%positive => []
    | 164%positive => []
    | 165%positive => []
    | 166%positive => []
    | 167%positive => []
    | _ => []
  end.


Theorem bits_fill_rectangle_ai_correct:
  forall s p' s', steps (g_start bits_fill_rectangle) s (g_edges bits_fill_rectangle) p' s' -> bits_fill_rectangle_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bits_fill_rectangle_pot_correct:
  forall s p' s',
    steps (g_start bits_fill_rectangle) s (g_edges bits_fill_rectangle) p' s' ->
    (bits_fill_rectangle_pot (g_start bits_fill_rectangle) s >= bits_fill_rectangle_pot p' s')%Q.
Proof.
  check_lp bits_fill_rectangle_ai_correct bits_fill_rectangle_hints.
Qed.

