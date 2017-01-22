Require Import pasta.Pasta.

Notation IDRPE_grid_selection_z := 1%positive.
Notation IDRPE_grid_selection_EM := 2%positive.
Notation IDRPE_grid_selection_L_common_0_3 := 3%positive.
Notation IDRPE_grid_selection_L_result := 4%positive.
Notation IDRPE_grid_selection_L_temp := 5%positive.
Notation IDRPE_grid_selection_Mc := 6%positive.
Notation IDRPE_grid_selection_Mc_out_dref := 7%positive.
Notation IDRPE_grid_selection_i := 8%positive.
Notation IDRPE_grid_selection_Mc_out := 9%positive.
Notation IDRPE_grid_selection_x := 10%positive.
Notation IDRPE_grid_selection_xM := 11%positive.
Definition RPE_grid_selection : graph := {|
  g_start := 1%positive;
  g_end := 122%positive;
  g_edges := (1%positive,(AAssign IDRPE_grid_selection_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDRPE_grid_selection_EM (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDRPE_grid_selection_Mc (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (ENum (0)))),5%positive)::
             (5%positive,(AAssign IDRPE_grid_selection_L_temp None),
             6%positive)::
             (6%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),7%positive)::
             (7%positive,(AAssign IDRPE_grid_selection_L_temp None),
             8%positive)::
             (8%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),9%positive)::
             (9%positive,(AAssign IDRPE_grid_selection_L_temp None),
             10%positive)::
             (10%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),11%positive)::
             (11%positive,(AAssign IDRPE_grid_selection_L_temp None),
             12%positive)::
             (12%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),13%positive)::
             (13%positive,(AAssign IDRPE_grid_selection_L_temp None),
             14%positive)::
             (14%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),15%positive)::
             (15%positive,(AAssign IDRPE_grid_selection_L_temp None),
             16%positive)::
             (16%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),17%positive)::
             (17%positive,(AAssign IDRPE_grid_selection_L_temp None),
             18%positive)::
             (18%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),19%positive)::
             (19%positive,(AAssign IDRPE_grid_selection_L_temp None),
             20%positive)::
             (20%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),21%positive)::
             (21%positive,(AAssign IDRPE_grid_selection_L_temp None),
             22%positive)::
             (22%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),23%positive)::
             (23%positive,(AAssign IDRPE_grid_selection_L_temp None),
             24%positive)::
             (24%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),25%positive)::
             (25%positive,(AAssign IDRPE_grid_selection_L_temp None),
             26%positive)::
             (26%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),27%positive)::
             (27%positive,(AAssign IDRPE_grid_selection_L_temp None),
             28%positive)::
             (28%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),29%positive)::
             (29%positive,(AAssign IDRPE_grid_selection_L_common_0_3
             (Some (EVar IDRPE_grid_selection_L_result))),30%positive)::
             (30%positive,(AAssign IDRPE_grid_selection_L_temp None),
             31%positive)::
             (31%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),32%positive)::
             (32%positive,(AAssign IDRPE_grid_selection_L_result None),
             33%positive)::
             (33%positive,(AAssign IDRPE_grid_selection_EM
             (Some (EVar IDRPE_grid_selection_L_result))),34%positive)::
             (34%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (ENum (0)))),35%positive)::
             (35%positive,(AAssign IDRPE_grid_selection_L_temp None),
             36%positive)::
             (36%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),37%positive)::
             (37%positive,(AAssign IDRPE_grid_selection_L_temp None),
             38%positive)::
             (38%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),39%positive)::
             (39%positive,(AAssign IDRPE_grid_selection_L_temp None),
             40%positive)::
             (40%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),41%positive)::
             (41%positive,(AAssign IDRPE_grid_selection_L_temp None),
             42%positive)::
             (42%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),43%positive)::
             (43%positive,(AAssign IDRPE_grid_selection_L_temp None),
             44%positive)::
             (44%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),45%positive)::
             (45%positive,(AAssign IDRPE_grid_selection_L_temp None),
             46%positive)::
             (46%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),47%positive)::
             (47%positive,(AAssign IDRPE_grid_selection_L_temp None),
             48%positive)::
             (48%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),49%positive)::
             (49%positive,(AAssign IDRPE_grid_selection_L_temp None),
             50%positive)::
             (50%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),51%positive)::
             (51%positive,(AAssign IDRPE_grid_selection_L_temp None),
             52%positive)::
             (52%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),53%positive)::
             (53%positive,(AAssign IDRPE_grid_selection_L_temp None),
             54%positive)::
             (54%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),55%positive)::
             (55%positive,(AAssign IDRPE_grid_selection_L_temp None),
             56%positive)::
             (56%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),57%positive)::
             (57%positive,(AAssign IDRPE_grid_selection_L_temp None),
             58%positive)::
             (58%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),59%positive)::
             (59%positive,(AAssign IDRPE_grid_selection_L_temp None),
             60%positive)::
             (60%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),61%positive)::
             (61%positive,(AAssign IDRPE_grid_selection_L_result None),
             62%positive)::(62%positive,AWeaken,63%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) >
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),65%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) <=
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),64%positive)::
             (64%positive,AWeaken,69%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AAssign IDRPE_grid_selection_Mc
             (Some (ENum (1)))),67%positive)::
             (67%positive,(AAssign IDRPE_grid_selection_EM
             (Some (EVar IDRPE_grid_selection_L_result))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (ENum (0)))),70%positive)::
             (70%positive,(AAssign IDRPE_grid_selection_L_temp None),
             71%positive)::
             (71%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),72%positive)::
             (72%positive,(AAssign IDRPE_grid_selection_L_temp None),
             73%positive)::
             (73%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),74%positive)::
             (74%positive,(AAssign IDRPE_grid_selection_L_temp None),
             75%positive)::
             (75%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),76%positive)::
             (76%positive,(AAssign IDRPE_grid_selection_L_temp None),
             77%positive)::
             (77%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),78%positive)::
             (78%positive,(AAssign IDRPE_grid_selection_L_temp None),
             79%positive)::
             (79%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),80%positive)::
             (80%positive,(AAssign IDRPE_grid_selection_L_temp None),
             81%positive)::
             (81%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),82%positive)::
             (82%positive,(AAssign IDRPE_grid_selection_L_temp None),
             83%positive)::
             (83%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),84%positive)::
             (84%positive,(AAssign IDRPE_grid_selection_L_temp None),
             85%positive)::
             (85%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),86%positive)::
             (86%positive,(AAssign IDRPE_grid_selection_L_temp None),
             87%positive)::
             (87%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),88%positive)::
             (88%positive,(AAssign IDRPE_grid_selection_L_temp None),
             89%positive)::
             (89%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),90%positive)::
             (90%positive,(AAssign IDRPE_grid_selection_L_temp None),
             91%positive)::
             (91%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),92%positive)::
             (92%positive,(AAssign IDRPE_grid_selection_L_temp None),
             93%positive)::
             (93%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),94%positive)::
             (94%positive,(AAssign IDRPE_grid_selection_L_temp None),
             95%positive)::
             (95%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),96%positive)::
             (96%positive,(AAssign IDRPE_grid_selection_L_result None),
             97%positive)::(97%positive,AWeaken,98%positive)::
             (98%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) >
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),100%positive)::
             (98%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) <=
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),99%positive)::
             (99%positive,AWeaken,104%positive)::
             (100%positive,AWeaken,101%positive)::
             (101%positive,(AAssign IDRPE_grid_selection_Mc
             (Some (ENum (2)))),102%positive)::
             (102%positive,(AAssign IDRPE_grid_selection_EM
             (Some (EVar IDRPE_grid_selection_L_result))),103%positive)::
             (103%positive,ANone,104%positive)::
             (104%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EVar IDRPE_grid_selection_L_common_0_3))),105%positive)::
             (105%positive,(AAssign IDRPE_grid_selection_L_temp None),
             106%positive)::
             (106%positive,(AAssign IDRPE_grid_selection_L_result
             (Some (EAdd (EVar IDRPE_grid_selection_L_result)
             (EMul (EVar IDRPE_grid_selection_L_temp)
             (EVar IDRPE_grid_selection_L_temp))))),107%positive)::
             (107%positive,(AAssign IDRPE_grid_selection_L_result None),
             108%positive)::(108%positive,AWeaken,109%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) >
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),111%positive)::
             (109%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_L_result) s) <=
             (eval (EVar IDRPE_grid_selection_EM) s))%Z)),110%positive)::
             (110%positive,AWeaken,115%positive)::
             (111%positive,AWeaken,112%positive)::
             (112%positive,(AAssign IDRPE_grid_selection_Mc
             (Some (ENum (3)))),113%positive)::
             (113%positive,(AAssign IDRPE_grid_selection_EM
             (Some (EVar IDRPE_grid_selection_L_result))),114%positive)::
             (114%positive,ANone,115%positive)::
             (115%positive,(AAssign IDRPE_grid_selection_i
             (Some (ENum (0)))),116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,AWeaken,118%positive)::
             (118%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_i) s) <=
             (eval (ENum (12)) s))%Z)),123%positive)::
             (118%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_selection_i) s) >
             (eval (ENum (12)) s))%Z)),119%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,(AAssign IDRPE_grid_selection_Mc_out_dref
             (Some (EVar IDRPE_grid_selection_Mc))),121%positive)::
             (121%positive,AWeaken,122%positive)::
             (123%positive,AWeaken,124%positive)::
             (124%positive,ANone,125%positive)::
             (125%positive,(AAssign IDRPE_grid_selection_i
             (Some (EAdd (EVar IDRPE_grid_selection_i) (ENum (1))))),
             126%positive)::(126%positive,ANone,127%positive)::
             (127%positive,ANone,128%positive)::
             (128%positive,(AAssign IDRPE_grid_selection_z
             (Some (EAdd (ENum (1)) (EVar IDRPE_grid_selection_z)))),
             129%positive)::(129%positive,AWeaken,118%positive)::nil
|}.

Definition RPE_grid_selection_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 3%positive => (-1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 4%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 5%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ -1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 6%positive => (-1 * (s IDRPE_grid_selection_L_result) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 7%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 8%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 9%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 10%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 11%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 12%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 13%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 14%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 15%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 16%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 17%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 18%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 19%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 20%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 21%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 22%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 23%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 24%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 25%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 26%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 27%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 28%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 29%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 30%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 31%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 32%positive => (-1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 33%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM) <= 0 /\ -1 * (s IDRPE_grid_selection_EM) <= 0)%Z
    | 34%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 35%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ -1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 36%positive => (-1 * (s IDRPE_grid_selection_L_result) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 37%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 38%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 39%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 40%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 41%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 42%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 43%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 44%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 45%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 46%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 47%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 48%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 49%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 50%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 51%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 52%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 53%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 54%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 55%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 56%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 57%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 58%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 59%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 60%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 61%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 62%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 63%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 64%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_EM)+ 1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 65%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 67%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDRPE_grid_selection_Mc) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 69%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 70%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ -1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 71%positive => (-1 * (s IDRPE_grid_selection_L_result) <= 0 /\ 1 * (s IDRPE_grid_selection_L_result) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 72%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 73%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 74%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 75%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 76%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 77%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 78%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 79%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 80%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 81%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 82%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 83%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 84%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 85%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 86%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 87%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 88%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 89%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 90%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 91%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 92%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 93%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 94%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 95%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 96%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 97%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0)%Z
    | 98%positive => (1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 99%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_EM)+ 1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 100%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0)%Z
    | 101%positive => (1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -1 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 102%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) + 2 <= 0)%Z
    | 103%positive => (-1 * (s IDRPE_grid_selection_Mc) + 2 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 104%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0)%Z
    | 105%positive => (1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 106%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0)%Z
    | 107%positive => (1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 108%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0)%Z
    | 109%positive => (1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 110%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_EM)+ 1 * (s IDRPE_grid_selection_L_result) <= 0)%Z
    | 111%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0)%Z
    | 112%positive => (1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -2 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 113%positive => (1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_EM)+ -1 * (s IDRPE_grid_selection_L_result) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) + 3 <= 0)%Z
    | 114%positive => (-1 * (s IDRPE_grid_selection_Mc) + 3 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 115%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0)%Z
    | 116%positive => (1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_i) <= 0 /\ -1 * (s IDRPE_grid_selection_i) <= 0)%Z
    | 117%positive => (-1 * (s IDRPE_grid_selection_i) <= 0 /\ 1 * (s IDRPE_grid_selection_i) <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0)%Z
    | 118%positive => (-1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_i) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -13 <= 0)%Z
    | 119%positive => (1 * (s IDRPE_grid_selection_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 13 <= 0)%Z
    | 120%positive => (-1 * (s IDRPE_grid_selection_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -13 <= 0)%Z
    | 121%positive => (1 * (s IDRPE_grid_selection_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_selection_Mc_out_dref) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc_out_dref) <= 0)%Z
    | 122%positive => (-1 * (s IDRPE_grid_selection_Mc_out_dref) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc_out_dref) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -13 <= 0)%Z
    | 123%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_i) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -12 <= 0)%Z
    | 124%positive => (1 * (s IDRPE_grid_selection_i) + -12 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ -1 * (s IDRPE_grid_selection_i) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0)%Z
    | 125%positive => (-1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_i) <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -12 <= 0)%Z
    | 126%positive => (-1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -13 <= 0)%Z
    | 127%positive => (1 * (s IDRPE_grid_selection_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 1 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) <= 0)%Z
    | 128%positive => (-1 * (s IDRPE_grid_selection_z) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_selection_i) + -13 <= 0)%Z
    | 129%positive => (1 * (s IDRPE_grid_selection_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_selection_i) + 1 <= 0 /\ -1 * (s IDRPE_grid_selection_Mc) <= 0 /\ 1 * (s IDRPE_grid_selection_Mc) + -3 <= 0 /\ -1 * (s IDRPE_grid_selection_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition RPE_grid_selection_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((13 # 1))%Q
    | 2%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 3%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 4%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 5%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 6%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 7%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 8%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 9%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 10%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 11%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 12%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 13%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 14%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 15%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 16%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 17%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 18%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 19%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 20%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 21%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 22%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 23%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 24%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 25%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 26%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 27%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 28%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 29%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 30%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 31%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 32%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 33%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 34%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 35%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 36%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 37%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 38%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 39%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 40%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 41%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 42%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 43%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 44%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 45%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 46%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 47%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 48%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 49%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 50%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 51%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 52%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 53%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 54%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 55%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 56%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 57%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 58%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 59%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 60%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 61%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 62%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 63%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 64%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 65%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 66%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 67%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 68%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 69%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 70%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 71%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 72%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 73%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 74%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 75%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 76%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 77%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 78%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 79%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 80%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 81%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 82%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 83%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 84%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 85%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 86%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 87%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 88%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 89%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 90%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 91%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 92%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 93%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 94%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 95%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 96%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 97%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 98%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 99%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 100%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 101%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 102%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 103%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 104%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 105%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 106%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 107%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 108%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 109%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 110%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 111%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 112%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 113%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 114%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 115%positive => ((13 # 1) + (s IDRPE_grid_selection_z))%Q
    | 116%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 117%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 118%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 119%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 120%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 121%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 122%positive => ((s IDRPE_grid_selection_z))%Q
    | 123%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 124%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 125%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 126%positive => ((14 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 127%positive => ((14 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 128%positive => ((14 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | 129%positive => ((13 # 1) - (s IDRPE_grid_selection_i)
                       + (s IDRPE_grid_selection_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition RPE_grid_selection_hints (p : node) (s : state) := 
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
    | 121%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (13
                                                              - (s IDRPE_grid_selection_i)) (12
                                                                    - (s IDRPE_grid_selection_i)));
                       (*-1 0*) F_max0_ge_0 (12 - (s IDRPE_grid_selection_i));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDRPE_grid_selection_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDRPE_grid_selection_i)))]
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | _ => []
  end.


Theorem RPE_grid_selection_ai_correct:
  forall s p' s', steps (g_start RPE_grid_selection) s (g_edges RPE_grid_selection) p' s' -> RPE_grid_selection_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem RPE_grid_selection_pot_correct:
  forall s p' s',
    steps (g_start RPE_grid_selection) s (g_edges RPE_grid_selection) p' s' ->
    (RPE_grid_selection_pot (g_start RPE_grid_selection) s >= RPE_grid_selection_pot p' s')%Q.
Proof.
  check_lp RPE_grid_selection_ai_correct RPE_grid_selection_hints.
Qed.

