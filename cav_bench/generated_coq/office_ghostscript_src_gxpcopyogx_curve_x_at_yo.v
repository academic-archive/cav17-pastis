Require Import pasta.Pasta.

Notation IDgx_curve_x_at_y_z := 1%positive.
Notation IDgx_curve_x_at_y__tmp := 2%positive.
Notation IDgx_curve_x_at_y__tmp1 := 3%positive.
Notation IDgx_curve_x_at_y_a := 4%positive.
Notation IDgx_curve_x_at_y_b := 5%positive.
Notation IDgx_curve_x_at_y_c := 6%positive.
Notation IDgx_curve_x_at_y_cx0 := 7%positive.
Notation IDgx_curve_x_at_y_cy0 := 8%positive.
Notation IDgx_curve_x_at_y_cy1 := 9%positive.
Notation IDgx_curve_x_at_y_cy2 := 10%positive.
Notation IDgx_curve_x_at_y_cy3 := 11%positive.
Notation IDgx_curve_x_at_y_i := 12%positive.
Notation IDgx_curve_x_at_y_k := 13%positive.
Notation IDgx_curve_x_at_y_prc_dref_off0 := 14%positive.
Notation IDgx_curve_x_at_y_prc_dref_off32 := 15%positive.
Notation IDgx_curve_x_at_y_prc_dref_off40 := 16%positive.
Notation IDgx_curve_x_at_y_prc_dref_off48 := 17%positive.
Notation IDgx_curve_x_at_y_prc_dref_off80 := 18%positive.
Notation IDgx_curve_x_at_y_prc_dref_off84 := 19%positive.
Notation IDgx_curve_x_at_y_prc_dref_off88_off0 := 20%positive.
Notation IDgx_curve_x_at_y_prc_dref_off88_off16 := 21%positive.
Notation IDgx_curve_x_at_y_prc_dref_off88_off24 := 22%positive.
Notation IDgx_curve_x_at_y_prc_dref_off88_off8 := 23%positive.
Notation IDgx_curve_x_at_y_prc_dref_off8_off0 := 24%positive.
Notation IDgx_curve_x_at_y_prc_dref_off8_off8 := 25%positive.
Notation IDgx_curve_x_at_y_t := 26%positive.
Notation IDgx_curve_x_at_y_t2 := 27%positive.
Notation IDgx_curve_x_at_y_t21 := 28%positive.
Notation IDgx_curve_x_at_y_t2d := 29%positive.
Notation IDgx_curve_x_at_y_t2d4 := 30%positive.
Notation IDgx_curve_x_at_y_t3 := 31%positive.
Notation IDgx_curve_x_at_y_t32 := 32%positive.
Notation IDgx_curve_x_at_y_t3d := 33%positive.
Notation IDgx_curve_x_at_y_t3d3 := 34%positive.
Notation IDgx_curve_x_at_y_xd := 35%positive.
Notation IDgx_curve_x_at_y_xl := 36%positive.
Notation IDgx_curve_x_at_y_yd := 37%positive.
Notation IDgx_curve_x_at_y_ym := 38%positive.
Notation IDgx_curve_x_at_y_yn := 39%positive.
Notation IDgx_curve_x_at_y_yrel := 40%positive.
Notation IDgx_curve_x_at_y_prc := 41%positive.
Notation IDgx_curve_x_at_y_y := 42%positive.
Definition gx_curve_x_at_y : graph := {|
  g_start := 1%positive;
  g_end := 134%positive;
  g_edges := (1%positive,(AAssign IDgx_curve_x_at_y_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_k)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDgx_curve_x_at_y__tmp
             (Some (EVar IDgx_curve_x_at_y_y))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) >=
             (eval (EVar IDgx_curve_x_at_y_prc_dref_off88_off0) s))%Z)),
             8%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) <
             (eval (EVar IDgx_curve_x_at_y_prc_dref_off88_off0) s))%Z)),
             7%positive)::(7%positive,AWeaken,11%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) <=
             (eval (EVar IDgx_curve_x_at_y_prc_dref_off88_off8) s))%Z)),
             102%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) >
             (eval (EVar IDgx_curve_x_at_y_prc_dref_off88_off8) s))%Z)),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDgx_curve_x_at_y_cy0
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off8_off8))),12%positive)::
             (12%positive,(AAssign IDgx_curve_x_at_y_cy3 None),13%positive)::
             (13%positive,(AAssign IDgx_curve_x_at_y_t (Some (ENum (0)))),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_cy0) s) >
             (eval (EVar IDgx_curve_x_at_y_cy3) s))%Z)),21%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_cy0) s) <=
             (eval (EVar IDgx_curve_x_at_y_cy3) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDgx_curve_x_at_y_cx0
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off8_off0))),18%positive)::
             (18%positive,(AAssign IDgx_curve_x_at_y_cy1 None),19%positive)::
             (19%positive,(AAssign IDgx_curve_x_at_y_cy2 None),20%positive)::
             (20%positive,ANone,28%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDgx_curve_x_at_y_cx0 None),23%positive)::
             (23%positive,(AAssign IDgx_curve_x_at_y_cy0 None),24%positive)::
             (24%positive,(AAssign IDgx_curve_x_at_y_cy1 None),25%positive)::
             (25%positive,(AAssign IDgx_curve_x_at_y_cy2 None),26%positive)::
             (26%positive,(AAssign IDgx_curve_x_at_y_cy3
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off8_off8))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDgx_curve_x_at_y_k
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off0))),29%positive)::
             (29%positive,(AAssign IDgx_curve_x_at_y_i
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off0))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_i)
             s) > (eval (ENum (0)) s))%Z)),79%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_i)
             s) <= (eval (ENum (0)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDgx_curve_x_at_y_a
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off32))),35%positive)::
             (35%positive,(AAssign IDgx_curve_x_at_y_b
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off40))),36%positive)::
             (36%positive,(AAssign IDgx_curve_x_at_y_c
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off48))),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_t)
             s) <= (eval (EVar IDgx_curve_x_at_y_prc_dref_off84) s))%Z)),
             63%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_t)
             s) > (eval (EVar IDgx_curve_x_at_y_prc_dref_off84) s))%Z)),
             39%positive)::(39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_prc_dref_off80) s) <>
             (eval (ENum (0)) s))%Z)),50%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_prc_dref_off80) s) =
             (eval (ENum (0)) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_k)
             s) >= (eval (ENum (11)) s))%Z)),45%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDgx_curve_x_at_y_k)
             s) < (eval (ENum (11)) s))%Z)),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,47%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDgx_curve_x_at_y_prc_dref_off80
             (Some (ENum (1)))),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,51%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,55%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDgx_curve_x_at_y_xl None),53%positive)::
             (53%positive,(AAssign IDgx_curve_x_at_y_xd None),54%positive)::
             (54%positive,ANone,62%positive)::
             (55%positive,(AAssign IDgx_curve_x_at_y_t21
             (Some (EMul (EVar IDgx_curve_x_at_y_t)
             (EVar IDgx_curve_x_at_y_t)))),56%positive)::
             (56%positive,(AAssign IDgx_curve_x_at_y_t32
             (Some (EMul (EVar IDgx_curve_x_at_y_t21)
             (EVar IDgx_curve_x_at_y_t)))),57%positive)::
             (57%positive,(AAssign IDgx_curve_x_at_y_t3d3
             (Some (EAdd (EMul (EAdd (EVar IDgx_curve_x_at_y_t21)
             (EVar IDgx_curve_x_at_y_t)) (ENum (3))) (ENum (1))))),
             58%positive)::
             (58%positive,(AAssign IDgx_curve_x_at_y_t2d4
             (Some (EAdd (EAdd (EVar IDgx_curve_x_at_y_t)
             (EVar IDgx_curve_x_at_y_t)) (ENum (1))))),59%positive)::
             (59%positive,(AAssign IDgx_curve_x_at_y_xl None),60%positive)::
             (60%positive,(AAssign IDgx_curve_x_at_y_xd None),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,ANone,71%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDgx_curve_x_at_y_t2
             (Some (EMul (EVar IDgx_curve_x_at_y_t)
             (EVar IDgx_curve_x_at_y_t)))),65%positive)::
             (65%positive,(AAssign IDgx_curve_x_at_y_t3
             (Some (EMul (EVar IDgx_curve_x_at_y_t2)
             (EVar IDgx_curve_x_at_y_t)))),66%positive)::
             (66%positive,(AAssign IDgx_curve_x_at_y_t3d
             (Some (EAdd (EMul (EAdd (EVar IDgx_curve_x_at_y_t2)
             (EVar IDgx_curve_x_at_y_t)) (ENum (3))) (ENum (1))))),
             67%positive)::
             (67%positive,(AAssign IDgx_curve_x_at_y_t2d
             (Some (EAdd (EAdd (EVar IDgx_curve_x_at_y_t)
             (EVar IDgx_curve_x_at_y_t)) (ENum (1))))),68%positive)::
             (68%positive,(AAssign IDgx_curve_x_at_y_xl None),69%positive)::
             (69%positive,(AAssign IDgx_curve_x_at_y_xd None),70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,(AAssign IDgx_curve_x_at_y_prc_dref_off88_off0
             (Some (EVar IDgx_curve_x_at_y_cy0))),72%positive)::
             (72%positive,(AAssign IDgx_curve_x_at_y_prc_dref_off88_off8
             (Some (EVar IDgx_curve_x_at_y_cy3))),73%positive)::
             (73%positive,(AAssign IDgx_curve_x_at_y_prc_dref_off88_off16
             (Some (EVar IDgx_curve_x_at_y_xl))),74%positive)::
             (74%positive,(AAssign IDgx_curve_x_at_y_prc_dref_off88_off24
             (Some (EVar IDgx_curve_x_at_y_xd))),75%positive)::
             (75%positive,(AAssign IDgx_curve_x_at_y_yd
             (Some (ESub (EVar IDgx_curve_x_at_y_cy3)
             (EVar IDgx_curve_x_at_y_cy0)))),76%positive)::
             (76%positive,(AAssign IDgx_curve_x_at_y_yrel
             (Some (ESub (EVar IDgx_curve_x_at_y__tmp)
             (EVar IDgx_curve_x_at_y_cy0)))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,AWeaken,109%positive)::
             (79%positive,AWeaken,80%positive)::
             (80%positive,(AAssign IDgx_curve_x_at_y_ym None),81%positive)::
             (81%positive,(AAssign IDgx_curve_x_at_y_yn None),82%positive)::
             (82%positive,(AAssign IDgx_curve_x_at_y_t None),83%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) <
             (eval (EVar IDgx_curve_x_at_y_yn) s))%Z)),91%positive)::
             (84%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y__tmp) s) >=
             (eval (EVar IDgx_curve_x_at_y_yn) s))%Z)),85%positive)::
             (85%positive,AWeaken,86%positive)::
             (86%positive,(AAssign IDgx_curve_x_at_y_t
             (Some (EAdd (EVar IDgx_curve_x_at_y_t) (ENum (1))))),
             87%positive)::
             (87%positive,(AAssign IDgx_curve_x_at_y_cy2 None),88%positive)::
             (88%positive,(AAssign IDgx_curve_x_at_y_cy1 None),89%positive)::
             (89%positive,(AAssign IDgx_curve_x_at_y_cy0
             (Some (EVar IDgx_curve_x_at_y_yn))),90%positive)::
             (90%positive,ANone,96%positive)::
             (91%positive,AWeaken,92%positive)::
             (92%positive,(AAssign IDgx_curve_x_at_y_cy1 None),93%positive)::
             (93%positive,(AAssign IDgx_curve_x_at_y_cy2 None),94%positive)::
             (94%positive,(AAssign IDgx_curve_x_at_y_cy3
             (Some (EVar IDgx_curve_x_at_y_yn))),95%positive)::
             (95%positive,ANone,96%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,(AAssign IDgx_curve_x_at_y_i
             (Some (EAdd (EVar IDgx_curve_x_at_y_i) (ENum (-1))))),
             98%positive)::(98%positive,ANone,99%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,(AAssign IDgx_curve_x_at_y_z
             (Some (EAdd (ENum (1)) (EVar IDgx_curve_x_at_y_z)))),
             101%positive)::(101%positive,AWeaken,32%positive)::
             (102%positive,AWeaken,103%positive)::
             (103%positive,(AAssign IDgx_curve_x_at_y_yd
             (Some (ESub (EVar IDgx_curve_x_at_y_prc_dref_off88_off8)
             (EVar IDgx_curve_x_at_y_prc_dref_off88_off0)))),104%positive)::
             (104%positive,(AAssign IDgx_curve_x_at_y_yrel
             (Some (ESub (EVar IDgx_curve_x_at_y__tmp)
             (EVar IDgx_curve_x_at_y_prc_dref_off88_off0)))),105%positive)::
             (105%positive,(AAssign IDgx_curve_x_at_y_xl
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off88_off16))),
             106%positive)::
             (106%positive,(AAssign IDgx_curve_x_at_y_xd
             (Some (EVar IDgx_curve_x_at_y_prc_dref_off88_off24))),
             107%positive)::(107%positive,ANone,108%positive)::
             (108%positive,AWeaken,109%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_yrel) s) =
             (eval (ENum (0)) s))%Z)),130%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_yrel) s) <>
             (eval (ENum (0)) s))%Z)),110%positive)::
             (110%positive,AWeaken,111%positive)::
             (111%positive,ANone,112%positive)::
             (111%positive,ANone,124%positive)::
             (112%positive,AWeaken,113%positive)::
             (113%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_xd) s) >=
             (eval (ENum (0)) s))%Z)),120%positive)::
             (113%positive,(AGuard
             (fun s => ((eval (EVar IDgx_curve_x_at_y_xd) s) <
             (eval (ENum (0)) s))%Z)),114%positive)::
             (114%positive,AWeaken,115%positive)::
             (115%positive,ANone,117%positive)::
             (115%positive,ANone,116%positive)::
             (116%positive,ANone,123%positive)::
             (117%positive,(AAssign IDgx_curve_x_at_y__tmp1 None),
             118%positive)::(118%positive,ANone,119%positive)::
             (119%positive,AWeaken,134%positive)::
             (120%positive,AWeaken,121%positive)::
             (121%positive,ANone,127%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,ANone,123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,(AAssign IDgx_curve_x_at_y__tmp1 None),
             125%positive)::(125%positive,ANone,126%positive)::
             (126%positive,AWeaken,134%positive)::
             (127%positive,(AAssign IDgx_curve_x_at_y__tmp1 None),
             128%positive)::(128%positive,ANone,129%positive)::
             (129%positive,AWeaken,134%positive)::
             (130%positive,AWeaken,131%positive)::
             (131%positive,(AAssign IDgx_curve_x_at_y__tmp1
             (Some (EVar IDgx_curve_x_at_y_xl))),132%positive)::
             (132%positive,ANone,133%positive)::
             (133%positive,AWeaken,134%positive)::nil
|}.

Definition gx_curve_x_at_y_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0)%Z
    | 4%positive => (-1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0)%Z
    | 9%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 12%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0)%Z
    | 13%positive => (-1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 14%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 15%positive => (-1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 16%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_cy0)+ -1 * (s IDgx_curve_x_at_y_cy3) <= 0)%Z
    | 17%positive => (1 * (s IDgx_curve_x_at_y_cy0)+ -1 * (s IDgx_curve_x_at_y_cy3) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 18%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_cy0)+ -1 * (s IDgx_curve_x_at_y_cy3) <= 0)%Z
    | 19%positive => (1 * (s IDgx_curve_x_at_y_cy0)+ -1 * (s IDgx_curve_x_at_y_cy3) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 20%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_cy0)+ -1 * (s IDgx_curve_x_at_y_cy3) <= 0)%Z
    | 21%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_cy0)+ 1 * (s IDgx_curve_x_at_y_cy3) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDgx_curve_x_at_y_cy0)+ 1 * (s IDgx_curve_x_at_y_cy3) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_cy0)+ 1 * (s IDgx_curve_x_at_y_cy3) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 25%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 26%positive => (-1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 27%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 28%positive => (-1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 29%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 30%positive => (-1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 31%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ -1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 32%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 33%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 34%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 35%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 36%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 37%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 38%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 39%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 41%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0)%Z
    | 42%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 43%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_k) + -10 <= 0)%Z
    | 44%positive => (1 * (s IDgx_curve_x_at_y_k) + -10 <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 45%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) + 11 <= 0)%Z
    | 46%positive => (-1 * (s IDgx_curve_x_at_y_k) + 11 <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 47%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) <= 0)%Z
    | 48%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) + -1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off80) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off80) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off80) + -1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 52%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 54%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 56%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 57%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0)%Z
    | 58%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 59%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t2d4) + 3 <= 0)%Z
    | 60%positive => (2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t2d4) + 3 <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 61%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ 1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t2d4) + 3 <= 0)%Z
    | 62%positive => (1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ -1 * (s IDgx_curve_x_at_y_t) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 63%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 64%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 65%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 66%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 67%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 68%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t2d) + -1 <= 0)%Z
    | 69%positive => (-2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t2d) + -1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0)%Z
    | 70%positive => (-1 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -2 * (s IDgx_curve_x_at_y_prc_dref_off84)+ 1 * (s IDgx_curve_x_at_y_t2d) + -1 <= 0)%Z
    | 71%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 72%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 73%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 74%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 75%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 76%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 77%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 78%positive => (1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 79%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0)%Z
    | 80%positive => (-1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 81%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0)%Z
    | 82%positive => (-1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 83%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0)%Z
    | 84%positive => (-1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 85%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0)%Z
    | 86%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 87%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0)%Z
    | 88%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 89%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0)%Z
    | 90%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_yn) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_cy0) <= 0)%Z
    | 91%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yn) + 1 <= 0)%Z
    | 92%positive => (1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yn) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 93%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yn) + 1 <= 0)%Z
    | 94%positive => (1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yn) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 95%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yn) + 1 <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_cy3) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDgx_curve_x_at_y_i) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 97%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) + 1 <= 0)%Z
    | 98%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 99%positive => (-1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 100%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_i) <= 0)%Z
    | 101%positive => (-1 * (s IDgx_curve_x_at_y_i) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) + 1 <= 0)%Z
    | 102%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0)%Z
    | 103%positive => (1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 104%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y_yd) <= 0)%Z
    | 105%positive => (-1 * (s IDgx_curve_x_at_y_yd) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yrel) <= 0)%Z
    | 106%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y_yd) <= 0)%Z
    | 107%positive => (-1 * (s IDgx_curve_x_at_y_yd) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yrel) <= 0)%Z
    | 108%positive => (-1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_k) <= 0 /\ -1 * (s IDgx_curve_x_at_y__tmp)+ 1 * (s IDgx_curve_x_at_y_prc_dref_off88_off0) <= 0 /\ 1 * (s IDgx_curve_x_at_y__tmp)+ -1 * (s IDgx_curve_x_at_y_prc_dref_off88_off8) <= 0 /\ -1 * (s IDgx_curve_x_at_y_yd) <= 0)%Z
    | 109%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 110%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 111%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 112%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 113%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 114%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0)%Z
    | 115%positive => (1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 116%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0)%Z
    | 117%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0)%Z
    | 118%positive => (1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 119%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_xd) + 1 <= 0)%Z
    | 120%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_xd) <= 0)%Z
    | 121%positive => (-1 * (s IDgx_curve_x_at_y_xd) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 122%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_xd) <= 0)%Z
    | 123%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 124%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 125%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 126%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 127%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_xd) <= 0)%Z
    | 128%positive => (-1 * (s IDgx_curve_x_at_y_xd) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 129%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ -1 * (s IDgx_curve_x_at_y_xd) <= 0)%Z
    | 130%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_yrel) <= 0)%Z
    | 131%positive => (-1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ 1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 132%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0 /\ 1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_yrel) <= 0)%Z
    | 133%positive => (-1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ 1 * (s IDgx_curve_x_at_y_yrel) <= 0 /\ -1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | 134%positive => (-1 * (s IDgx_curve_x_at_y_z) <= 0)%Z
    | _ => False
  end.

Definition gx_curve_x_at_y_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 2%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 3%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 4%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 5%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 6%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 7%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 8%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 9%positive => ((s IDgx_curve_x_at_y_z)
                     + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 10%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 11%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 12%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 13%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 14%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 15%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 16%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 17%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 18%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 19%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 20%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 21%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 22%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 23%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 24%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 25%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 26%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 27%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 28%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 29%positive => ((s IDgx_curve_x_at_y_z)
                      + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 30%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 31%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 32%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 33%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 34%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 35%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 36%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 37%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 38%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 39%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 40%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 41%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 42%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 43%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 44%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 45%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 46%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 47%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 48%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 49%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 50%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 51%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 52%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 53%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 54%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 55%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 56%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 57%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 58%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 59%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 60%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 61%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 62%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 63%positive => ((s IDgx_curve_x_at_y_z)
                      - max0(-1 + (s IDgx_curve_x_at_y_i))
                      + max0((s IDgx_curve_x_at_y_i)))%Q
    | 64%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 65%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 66%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 67%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 68%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 69%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 70%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 71%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 72%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 73%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 74%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 75%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 76%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 77%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 78%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 79%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 80%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 81%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 82%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 83%positive => ((s IDgx_curve_x_at_y_z) + max0((s IDgx_curve_x_at_y_i)))%Q
    | 84%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 85%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 86%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 87%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 88%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 89%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 90%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 91%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 92%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 93%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 94%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 95%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 96%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 97%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 98%positive => ((1 # 1) + (s IDgx_curve_x_at_y_i)
                      + (s IDgx_curve_x_at_y_z))%Q
    | 99%positive => ((1 # 1) + (s IDgx_curve_x_at_y_i)
                      + (s IDgx_curve_x_at_y_z))%Q
    | 100%positive => ((1 # 1) + (s IDgx_curve_x_at_y_i)
                       + (s IDgx_curve_x_at_y_z))%Q
    | 101%positive => ((s IDgx_curve_x_at_y_i) + (s IDgx_curve_x_at_y_z))%Q
    | 102%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 103%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 104%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 105%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 106%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 107%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 108%positive => ((s IDgx_curve_x_at_y_z)
                       + max0((s IDgx_curve_x_at_y_prc_dref_off0)))%Q
    | 109%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 110%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 111%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 112%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 113%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 114%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 115%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 116%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 117%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 118%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 119%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 120%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 121%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 122%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 123%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 124%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 125%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 126%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 127%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 128%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 129%positive => (max0((s IDgx_curve_x_at_y_z)))%Q
    | 130%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 131%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 132%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 133%positive => ((s IDgx_curve_x_at_y_z))%Q
    | 134%positive => ((s IDgx_curve_x_at_y_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_curve_x_at_y_hints (p : node) (s : state) := 
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
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgx_curve_x_at_y_i))) (F_check_ge (0) (0))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDgx_curve_x_at_y_i)) (-1
                                                                    + (s IDgx_curve_x_at_y_i)))]
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
    | 63%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_curve_x_at_y_i)) (-1
                                                                    + (s IDgx_curve_x_at_y_i)))]
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
    | 83%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_curve_x_at_y_i))) (F_check_ge ((s IDgx_curve_x_at_y_i)) (0))]
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
    | 101%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_curve_x_at_y_i)) (0))) (F_max0_ge_0 ((s IDgx_curve_x_at_y_i)))]
    | 102%positive => []
    | 103%positive => []
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => [(*-1 0*) F_max0_ge_0 ((s IDgx_curve_x_at_y_prc_dref_off0))]
    | 109%positive => []
    | 110%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_curve_x_at_y_z)) (0))) (F_max0_ge_0 ((s IDgx_curve_x_at_y_z)))]
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_curve_x_at_y_z))) (F_check_ge ((s IDgx_curve_x_at_y_z)) (0))]
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_curve_x_at_y_z))) (F_check_ge ((s IDgx_curve_x_at_y_z)) (0))]
    | 127%positive => []
    | 128%positive => []
    | 129%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_curve_x_at_y_z))) (F_check_ge ((s IDgx_curve_x_at_y_z)) (0))]
    | 130%positive => []
    | 131%positive => []
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | _ => []
  end.


Theorem gx_curve_x_at_y_ai_correct:
  forall s p' s', steps (g_start gx_curve_x_at_y) s (g_edges gx_curve_x_at_y) p' s' -> gx_curve_x_at_y_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_curve_x_at_y_pot_correct:
  forall s p' s',
    steps (g_start gx_curve_x_at_y) s (g_edges gx_curve_x_at_y) p' s' ->
    (gx_curve_x_at_y_pot (g_start gx_curve_x_at_y) s >= gx_curve_x_at_y_pot p' s')%Q.
Proof.
  check_lp gx_curve_x_at_y_ai_correct gx_curve_x_at_y_hints.
Qed.

