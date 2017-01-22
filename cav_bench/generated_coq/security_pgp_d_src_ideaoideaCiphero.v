Require Import pasta.Pasta.

Notation IDideaCipher_z := 1%positive.
Notation IDideaCipher_r := 2%positive.
Notation IDideaCipher_s2 := 3%positive.
Notation IDideaCipher_s3 := 4%positive.
Notation IDideaCipher_t16 := 5%positive.
Notation IDideaCipher_t32 := 6%positive.
Notation IDideaCipher_x1 := 7%positive.
Notation IDideaCipher_x2 := 8%positive.
Notation IDideaCipher_x3 := 9%positive.
Notation IDideaCipher_x4 := 10%positive.
Notation IDideaCipher_inbuf := 11%positive.
Notation IDideaCipher_key := 12%positive.
Notation IDideaCipher_outbuf := 13%positive.
Definition ideaCipher : graph := {|
  g_start := 1%positive;
  g_end := 144%positive;
  g_edges := (1%positive,(AAssign IDideaCipher_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDideaCipher_r (Some (ENum (8)))),
             3%positive)::
             (3%positive,(AAssign IDideaCipher_x1 None),4%positive)::
             (4%positive,(AAssign IDideaCipher_x2 None),5%positive)::
             (5%positive,(AAssign IDideaCipher_x3 None),6%positive)::
             (6%positive,(AAssign IDideaCipher_x4 None),7%positive)::
             (7%positive,(AAssign IDideaCipher_x1 None),8%positive)::
             (8%positive,(AAssign IDideaCipher_x2 None),9%positive)::
             (9%positive,(AAssign IDideaCipher_x3 None),10%positive)::
             (10%positive,(AAssign IDideaCipher_x4 None),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDideaCipher_t16 None),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,17%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDideaCipher_x1 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x1)))),16%positive)::
             (16%positive,ANone,30%positive)::
             (17%positive,(AAssign IDideaCipher_x1
             (Some (EVar IDideaCipher_x1))),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x1)
             s) <> (eval (ENum (0)) s))%Z)),23%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x1)
             s) = (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDideaCipher_x1 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),22%positive)::
             (22%positive,ANone,29%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x1) (EVar IDideaCipher_t16)))),
             25%positive)::
             (25%positive,(AAssign IDideaCipher_x1
             (Some (EVar IDideaCipher_t32))),26%positive)::
             (26%positive,(AAssign IDideaCipher_t16 None),27%positive)::
             (27%positive,(AAssign IDideaCipher_x1 None),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDideaCipher_x2 None),31%positive)::
             (31%positive,(AAssign IDideaCipher_x3 None),32%positive)::
             (32%positive,(AAssign IDideaCipher_t16 None),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,37%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDideaCipher_x4 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x4)))),36%positive)::
             (36%positive,ANone,50%positive)::
             (37%positive,(AAssign IDideaCipher_x4
             (Some (EVar IDideaCipher_x4))),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x4)
             s) <> (eval (ENum (0)) s))%Z)),43%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x4)
             s) = (eval (ENum (0)) s))%Z)),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDideaCipher_x4 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),42%positive)::
             (42%positive,ANone,49%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x4) (EVar IDideaCipher_t16)))),
             45%positive)::
             (45%positive,(AAssign IDideaCipher_x4
             (Some (EVar IDideaCipher_t32))),46%positive)::
             (46%positive,(AAssign IDideaCipher_t16 None),47%positive)::
             (47%positive,(AAssign IDideaCipher_x4 None),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDideaCipher_s3
             (Some (EVar IDideaCipher_x3))),51%positive)::
             (51%positive,(AAssign IDideaCipher_x3 None),52%positive)::
             (52%positive,(AAssign IDideaCipher_t16 None),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,57%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDideaCipher_x3 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x3)))),56%positive)::
             (56%positive,ANone,70%positive)::
             (57%positive,(AAssign IDideaCipher_x3
             (Some (EVar IDideaCipher_x3))),58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x3)
             s) <> (eval (ENum (0)) s))%Z)),63%positive)::
             (59%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x3)
             s) = (eval (ENum (0)) s))%Z)),60%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AAssign IDideaCipher_x3 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),62%positive)::
             (62%positive,ANone,69%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x3) (EVar IDideaCipher_t16)))),
             65%positive)::
             (65%positive,(AAssign IDideaCipher_x3
             (Some (EVar IDideaCipher_t32))),66%positive)::
             (66%positive,(AAssign IDideaCipher_t16 None),67%positive)::
             (67%positive,(AAssign IDideaCipher_x3 None),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDideaCipher_s2
             (Some (EVar IDideaCipher_x2))),71%positive)::
             (71%positive,(AAssign IDideaCipher_x2 None),72%positive)::
             (72%positive,(AAssign IDideaCipher_x2
             (Some (EAdd (EVar IDideaCipher_x2) (EVar IDideaCipher_x3)))),
             73%positive)::
             (73%positive,(AAssign IDideaCipher_t16 None),74%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,ANone,78%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDideaCipher_x2 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x2)))),77%positive)::
             (77%positive,ANone,91%positive)::
             (78%positive,(AAssign IDideaCipher_x2
             (Some (EVar IDideaCipher_x2))),79%positive)::
             (79%positive,AWeaken,80%positive)::
             (80%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x2)
             s) <> (eval (ENum (0)) s))%Z)),84%positive)::
             (80%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x2)
             s) = (eval (ENum (0)) s))%Z)),81%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,(AAssign IDideaCipher_x2 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),83%positive)::
             (83%positive,ANone,90%positive)::
             (84%positive,AWeaken,85%positive)::
             (85%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x2) (EVar IDideaCipher_t16)))),
             86%positive)::
             (86%positive,(AAssign IDideaCipher_x2
             (Some (EVar IDideaCipher_t32))),87%positive)::
             (87%positive,(AAssign IDideaCipher_t16 None),88%positive)::
             (88%positive,(AAssign IDideaCipher_x2 None),89%positive)::
             (89%positive,ANone,90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,(AAssign IDideaCipher_x3
             (Some (EAdd (EVar IDideaCipher_x3) (EVar IDideaCipher_x2)))),
             92%positive)::
             (92%positive,(AAssign IDideaCipher_x1 None),93%positive)::
             (93%positive,(AAssign IDideaCipher_x4 None),94%positive)::
             (94%positive,(AAssign IDideaCipher_x2 None),95%positive)::
             (95%positive,(AAssign IDideaCipher_x3 None),96%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,(AAssign IDideaCipher_r
             (Some (EAdd (EVar IDideaCipher_r) (ENum (-1))))),98%positive)::
             (98%positive,AWeaken,99%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCipher_r) (ENum (-1))) s) <>
             (eval (ENum (0)) s))%Z)),145%positive)::
             (99%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCipher_r) (ENum (-1))) s) =
             (eval (ENum (0)) s))%Z)),100%positive)::
             (100%positive,AWeaken,101%positive)::
             (101%positive,(AAssign IDideaCipher_t16 None),102%positive)::
             (102%positive,AWeaken,103%positive)::
             (103%positive,ANone,106%positive)::
             (103%positive,ANone,104%positive)::
             (104%positive,(AAssign IDideaCipher_x1 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x1)))),105%positive)::
             (105%positive,ANone,119%positive)::
             (106%positive,(AAssign IDideaCipher_x1
             (Some (EVar IDideaCipher_x1))),107%positive)::
             (107%positive,AWeaken,108%positive)::
             (108%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x1)
             s) <> (eval (ENum (0)) s))%Z)),112%positive)::
             (108%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x1)
             s) = (eval (ENum (0)) s))%Z)),109%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,(AAssign IDideaCipher_x1 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),111%positive)::
             (111%positive,ANone,118%positive)::
             (112%positive,AWeaken,113%positive)::
             (113%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x1) (EVar IDideaCipher_t16)))),
             114%positive)::
             (114%positive,(AAssign IDideaCipher_x1
             (Some (EVar IDideaCipher_t32))),115%positive)::
             (115%positive,(AAssign IDideaCipher_t16 None),116%positive)::
             (116%positive,(AAssign IDideaCipher_x1 None),117%positive)::
             (117%positive,ANone,118%positive)::
             (118%positive,ANone,119%positive)::
             (119%positive,(AAssign IDideaCipher_x3 None),120%positive)::
             (120%positive,(AAssign IDideaCipher_x2 None),121%positive)::
             (121%positive,(AAssign IDideaCipher_t16 None),122%positive)::
             (122%positive,AWeaken,123%positive)::
             (123%positive,ANone,126%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,(AAssign IDideaCipher_x4 (Some (ESub (ENum (1))
             (EVar IDideaCipher_x4)))),125%positive)::
             (125%positive,ANone,139%positive)::
             (126%positive,(AAssign IDideaCipher_x4
             (Some (EVar IDideaCipher_x4))),127%positive)::
             (127%positive,AWeaken,128%positive)::
             (128%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x4)
             s) <> (eval (ENum (0)) s))%Z)),132%positive)::
             (128%positive,(AGuard (fun s => ((eval (EVar IDideaCipher_x4)
             s) = (eval (ENum (0)) s))%Z)),129%positive)::
             (129%positive,AWeaken,130%positive)::
             (130%positive,(AAssign IDideaCipher_x4 (Some (ESub (ENum (1))
             (EVar IDideaCipher_t16)))),131%positive)::
             (131%positive,ANone,138%positive)::
             (132%positive,AWeaken,133%positive)::
             (133%positive,(AAssign IDideaCipher_t32
             (Some (EMul (EVar IDideaCipher_x4) (EVar IDideaCipher_t16)))),
             134%positive)::
             (134%positive,(AAssign IDideaCipher_x4
             (Some (EVar IDideaCipher_t32))),135%positive)::
             (135%positive,(AAssign IDideaCipher_t16 None),136%positive)::
             (136%positive,(AAssign IDideaCipher_x4 None),137%positive)::
             (137%positive,ANone,138%positive)::
             (138%positive,ANone,139%positive)::
             (139%positive,(AAssign IDideaCipher_x1
             (Some (EVar IDideaCipher_x1))),140%positive)::
             (140%positive,(AAssign IDideaCipher_x2
             (Some (EVar IDideaCipher_x2))),141%positive)::
             (141%positive,(AAssign IDideaCipher_x3
             (Some (EVar IDideaCipher_x3))),142%positive)::
             (142%positive,(AAssign IDideaCipher_x4
             (Some (EVar IDideaCipher_x4))),143%positive)::
             (143%positive,AWeaken,144%positive)::
             (145%positive,AWeaken,146%positive)::
             (146%positive,ANone,147%positive)::
             (147%positive,(AAssign IDideaCipher_z (Some (EAdd (ENum (1))
             (EVar IDideaCipher_z)))),12%positive)::nil
|}.

Definition ideaCipher_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaCipher_z) <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_r) + 8 <= 0)%Z
    | 4%positive => (-1 * (s IDideaCipher_r) + 8 <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_r) + 8 <= 0)%Z
    | 6%positive => (-1 * (s IDideaCipher_r) + 8 <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 7%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_r) + 8 <= 0)%Z
    | 8%positive => (-1 * (s IDideaCipher_r) + 8 <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 9%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_r) + 8 <= 0)%Z
    | 10%positive => (-1 * (s IDideaCipher_r) + 8 <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 11%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_r) + 8 <= 0)%Z
    | 12%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 13%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 14%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 15%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 16%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 17%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 18%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 19%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 20%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_x1) <= 0 /\ -1 * (s IDideaCipher_x1) <= 0)%Z
    | 21%positive => (-1 * (s IDideaCipher_x1) <= 0 /\ 1 * (s IDideaCipher_x1) <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 22%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 23%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 24%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 25%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 26%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 27%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 28%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 29%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 30%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 32%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 33%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 34%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 35%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 36%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 37%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 38%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 39%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 40%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ 1 * (s IDideaCipher_x4) <= 0 /\ -1 * (s IDideaCipher_x4) <= 0)%Z
    | 41%positive => (-1 * (s IDideaCipher_x4) <= 0 /\ 1 * (s IDideaCipher_x4) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 42%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 43%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 44%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 45%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 46%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 47%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 48%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 49%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 50%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 51%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 52%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 53%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 54%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 55%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 56%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 57%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 58%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 59%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 60%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_x3) <= 0 /\ -1 * (s IDideaCipher_x3) <= 0)%Z
    | 61%positive => (-1 * (s IDideaCipher_x3) <= 0 /\ 1 * (s IDideaCipher_x3) <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 62%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 63%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 64%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 65%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 66%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 67%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 68%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 69%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 70%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 71%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 72%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 73%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 74%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 75%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 76%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 77%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 78%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 79%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 80%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 81%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_x2) <= 0 /\ -1 * (s IDideaCipher_x2) <= 0)%Z
    | 82%positive => (-1 * (s IDideaCipher_x2) <= 0 /\ 1 * (s IDideaCipher_x2) <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 83%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 84%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 85%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 86%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 87%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 88%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 89%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 90%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 91%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 92%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 93%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 94%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 95%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 96%positive => (1 * (s IDideaCipher_r) + -8 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 97%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -8 <= 0)%Z
    | 98%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -7 <= 0)%Z
    | 99%positive => (1 * (s IDideaCipher_r) + -7 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 100%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 101%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 102%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 103%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 104%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 106%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 107%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 108%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 109%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_x1) <= 0 /\ -1 * (s IDideaCipher_x1) <= 0)%Z
    | 110%positive => (-1 * (s IDideaCipher_x1) <= 0 /\ 1 * (s IDideaCipher_x1) <= 0 /\ -1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 111%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 112%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 113%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 114%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 115%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 116%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 117%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 118%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 119%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 120%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 121%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 122%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 123%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 124%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 125%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 126%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 127%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 128%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 129%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_x4) <= 0 /\ -1 * (s IDideaCipher_x4) <= 0)%Z
    | 130%positive => (-1 * (s IDideaCipher_x4) <= 0 /\ 1 * (s IDideaCipher_x4) <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 131%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 132%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 133%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 134%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 135%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 136%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 137%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 138%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 139%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 140%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 141%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 142%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 143%positive => (-1 * (s IDideaCipher_r) + 1 <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 144%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -1 <= 0 /\ -1 * (s IDideaCipher_r) + 1 <= 0)%Z
    | 145%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -7 <= 0)%Z
    | 146%positive => (1 * (s IDideaCipher_r) + -7 <= 0 /\ -1 * (s IDideaCipher_z) <= 0)%Z
    | 147%positive => (-1 * (s IDideaCipher_z) <= 0 /\ 1 * (s IDideaCipher_r) + -7 <= 0)%Z
    | _ => False
  end.

Definition ideaCipher_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((6 # 1))%Q
    | 2%positive => ((6 # 1) + (s IDideaCipher_z))%Q
    | 3%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 4%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 5%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 6%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 7%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 8%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 9%positive => ((6 # 1) + (s IDideaCipher_z)
                     - max0(8 - (s IDideaCipher_r)))%Q
    | 10%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 11%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 12%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 13%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 14%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 15%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 16%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 17%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 18%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 19%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 20%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 21%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 22%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 23%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 24%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 25%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 26%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 27%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 28%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 29%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 30%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 31%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 32%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 33%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 34%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 35%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 36%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 37%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 38%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 39%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 40%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 41%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 42%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 43%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 44%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 45%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 46%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 47%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 48%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 49%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 50%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 51%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 52%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 53%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 54%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 55%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 56%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 57%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 58%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 59%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 60%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 61%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 62%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 63%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 64%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 65%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 66%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 67%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 68%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 69%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 70%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 71%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 72%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 73%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 74%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 75%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 76%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 77%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 78%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 79%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 80%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 81%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 82%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 83%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 84%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 85%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 86%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 87%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 88%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 89%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 90%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 91%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 92%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 93%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 94%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 95%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 96%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 97%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(8 - (s IDideaCipher_r)))%Q
    | 98%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(7 - (s IDideaCipher_r)))%Q
    | 99%positive => ((6 # 1) + (s IDideaCipher_z)
                      - max0(7 - (s IDideaCipher_r)))%Q
    | 100%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 101%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 102%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 103%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 104%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 105%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 106%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 107%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 108%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 109%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 110%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 111%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 112%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 113%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 114%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 115%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 116%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 117%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 118%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 119%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 120%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 121%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 122%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 123%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 124%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 125%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 126%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 127%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 128%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 129%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 130%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 131%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 132%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 133%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 134%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 135%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 136%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 137%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 138%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 139%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 140%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 141%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 142%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 143%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 144%positive => ((s IDideaCipher_z))%Q
    | 145%positive => ((6 # 1) + (s IDideaCipher_z)
                       - max0(7 - (s IDideaCipher_r)))%Q
    | 146%positive => ((7 # 1) + (s IDideaCipher_z)
                       - max0(8 - (s IDideaCipher_r)))%Q
    | 147%positive => ((7 # 1) + (s IDideaCipher_z)
                       - max0(8 - (s IDideaCipher_r)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaCipher_hints (p : node) (s : state) := 
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
    | 143%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaCipher_r)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaCipher_r)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDideaCipher_r))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDideaCipher_r)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDideaCipher_r)))]
    | 144%positive => []
    | 145%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - 
                                                                    (s IDideaCipher_r))) (F_check_ge (8
                                                                    - (s IDideaCipher_r)) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaCipher_r)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaCipher_r)))]
    | 146%positive => []
    | 147%positive => []
    | _ => []
  end.


Theorem ideaCipher_ai_correct:
  forall s p' s', steps (g_start ideaCipher) s (g_edges ideaCipher) p' s' -> ideaCipher_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaCipher_pot_correct:
  forall s p' s',
    steps (g_start ideaCipher) s (g_edges ideaCipher) p' s' ->
    (ideaCipher_pot (g_start ideaCipher) s >= ideaCipher_pot p' s')%Q.
Proof.
  check_lp ideaCipher_ai_correct ideaCipher_hints.
Qed.

