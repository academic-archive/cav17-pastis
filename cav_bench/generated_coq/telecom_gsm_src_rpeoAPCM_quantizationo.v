Require Import pasta.Pasta.

Notation IDAPCM_quantization_z := 1%positive.
Notation IDAPCM_quantization_exp_out_dref := 2%positive.
Notation IDAPCM_quantization_i := 3%positive.
Notation IDAPCM_quantization_itest := 4%positive.
Notation IDAPCM_quantization_mant_out_dref := 5%positive.
Notation IDAPCM_quantization_temp := 6%positive.
Notation IDAPCM_quantization_temp1 := 7%positive.
Notation IDAPCM_quantization_temp2 := 8%positive.
Notation IDAPCM_quantization_xmax := 9%positive.
Notation IDAPCM_quantization_xmaxc := 10%positive.
Notation IDAPCM_quantization_xmaxc_out_dref := 11%positive.
Notation IDAPCM_quantization_exp_out := 12%positive.
Notation IDAPCM_quantization_mant_out := 13%positive.
Notation IDAPCM_quantization_xM := 14%positive.
Notation IDAPCM_quantization_xMc := 15%positive.
Notation IDAPCM_quantization_xmaxc_out := 16%positive.
Definition APCM_quantization : graph := {|
  g_start := 1%positive;
  g_end := 84%positive;
  g_edges := (1%positive,(AAssign IDAPCM_quantization_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDAPCM_quantization_xmax
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDAPCM_quantization_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) <=
             (eval (ENum (12)) s))%Z)),97%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) >
             (eval (ENum (12)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDAPCM_quantization_temp None),9%positive)::
             (9%positive,(AAssign IDAPCM_quantization_itest
             (Some (ENum (0)))),10%positive)::
             (10%positive,(AAssign IDAPCM_quantization_i (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) <=
             (eval (ENum (5)) s))%Z)),78%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) >
             (eval (ENum (5)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (15%positive,ANone,18%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,20%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,84%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDAPCM_quantization_temp None),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) <=
             (eval (ENum (11)) s))%Z)),25%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) >
             (eval (ENum (11)) s))%Z)),24%positive)::
             (24%positive,AWeaken,28%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) >=
             (eval (ENum (0)) s))%Z)),30%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) <
             (eval (ENum (0)) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,84%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDAPCM_quantization_xmaxc None),
             33%positive)::(33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (34%positive,ANone,37%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,39%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,84%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (41%positive,ANone,44%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,46%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,84%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDAPCM_quantization_temp1 None),
             48%positive)::
             (48%positive,(AAssign IDAPCM_quantization_temp2 None),
             49%positive)::
             (49%positive,(AAssign IDAPCM_quantization_i (Some (ENum (0)))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) <=
             (eval (ENum (12)) s))%Z)),59%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_i) s) >
             (eval (ENum (12)) s))%Z)),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AAssign IDAPCM_quantization_mant_out_dref None),
             55%positive)::
             (55%positive,(AAssign IDAPCM_quantization_exp_out_dref None),
             56%positive)::
             (56%positive,(AAssign IDAPCM_quantization_xmaxc_out_dref
             (Some (EVar IDAPCM_quantization_xmaxc))),57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,AWeaken,84%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp1) s) >=
             (eval (ENum (0)) s))%Z)),62%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp1) s) <
             (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,65%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp1) s) <
             (eval (ENum (16)) s))%Z)),67%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp1) s) >=
             (eval (ENum (16)) s))%Z)),64%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,84%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDAPCM_quantization_temp None),
             70%positive)::
             (70%positive,(AAssign IDAPCM_quantization_temp None),
             71%positive)::
             (71%positive,(AAssign IDAPCM_quantization_temp None),
             72%positive)::(72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDAPCM_quantization_i
             (Some (EAdd (EVar IDAPCM_quantization_i) (ENum (1))))),
             74%positive)::(74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDAPCM_quantization_z
             (Some (EAdd (ENum (1)) (EVar IDAPCM_quantization_z)))),
             77%positive)::(77%positive,AWeaken,52%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AAssign IDAPCM_quantization_itest None),
             80%positive)::
             (80%positive,(AAssign IDAPCM_quantization_temp None),
             81%positive)::(81%positive,AWeaken,82%positive)::
             (82%positive,ANone,85%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,AWeaken,84%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_itest) s) =
             (eval (ENum (0)) s))%Z)),89%positive)::
             (87%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_itest) s) <>
             (eval (ENum (0)) s))%Z)),88%positive)::
             (88%positive,AWeaken,91%positive)::
             (89%positive,AWeaken,90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,(AAssign IDAPCM_quantization_i
             (Some (EAdd (EVar IDAPCM_quantization_i) (ENum (1))))),
             93%positive)::(93%positive,ANone,94%positive)::
             (94%positive,ANone,95%positive)::
             (95%positive,(AAssign IDAPCM_quantization_z
             (Some (EAdd (ENum (1)) (EVar IDAPCM_quantization_z)))),
             96%positive)::(96%positive,AWeaken,13%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,(AAssign IDAPCM_quantization_temp None),
             99%positive)::(99%positive,AWeaken,100%positive)::
             (100%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) <
             (eval (ENum (0)) s))%Z)),103%positive)::
             (100%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) >=
             (eval (ENum (0)) s))%Z)),101%positive)::
             (101%positive,AWeaken,102%positive)::
             (102%positive,ANone,110%positive)::
             (103%positive,AWeaken,104%positive)::
             (104%positive,(AGuard (fun s => True)),108%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,ANone,106%positive)::
             (106%positive,(AGuard (fun s => True)),107%positive)::
             (107%positive,AWeaken,110%positive)::
             (108%positive,AWeaken,109%positive)::
             (109%positive,ANone,110%positive)::
             (110%positive,(AAssign IDAPCM_quantization_temp None),
             111%positive)::(111%positive,AWeaken,112%positive)::
             (112%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) >
             (eval (EVar IDAPCM_quantization_xmax) s))%Z)),114%positive)::
             (112%positive,(AGuard
             (fun s => ((eval (EVar IDAPCM_quantization_temp) s) <=
             (eval (EVar IDAPCM_quantization_xmax) s))%Z)),113%positive)::
             (113%positive,AWeaken,117%positive)::
             (114%positive,AWeaken,115%positive)::
             (115%positive,(AAssign IDAPCM_quantization_xmax
             (Some (EVar IDAPCM_quantization_temp))),116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,ANone,118%positive)::
             (118%positive,(AAssign IDAPCM_quantization_i
             (Some (EAdd (EVar IDAPCM_quantization_i) (ENum (1))))),
             119%positive)::(119%positive,ANone,120%positive)::
             (120%positive,ANone,121%positive)::
             (121%positive,(AAssign IDAPCM_quantization_z
             (Some (EAdd (ENum (1)) (EVar IDAPCM_quantization_z)))),
             122%positive)::(122%positive,AWeaken,6%positive)::nil
|}.

Definition APCM_quantization_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0)%Z
    | 3%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_xmax) <= 0 /\ -1 * (s IDAPCM_quantization_xmax) <= 0)%Z
    | 4%positive => (-1 * (s IDAPCM_quantization_xmax) <= 0 /\ 1 * (s IDAPCM_quantization_xmax) <= 0 /\ 1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 5%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_xmax) <= 0 /\ -1 * (s IDAPCM_quantization_xmax) <= 0)%Z
    | 6%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 7%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 13 <= 0)%Z
    | 8%positive => (-1 * (s IDAPCM_quantization_i) + 13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 9%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 13 <= 0)%Z
    | 10%positive => (-1 * (s IDAPCM_quantization_i) + 13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ 1 * (s IDAPCM_quantization_itest) <= 0 /\ -1 * (s IDAPCM_quantization_itest) <= 0)%Z
    | 11%positive => (-1 * (s IDAPCM_quantization_itest) <= 0 /\ 1 * (s IDAPCM_quantization_itest) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 12%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_itest) <= 0 /\ -1 * (s IDAPCM_quantization_itest) <= 0)%Z
    | 13%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 14%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 15%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 16%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 17%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 18%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 19%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 20%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 21%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 22%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 23%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 24%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_temp) + 12 <= 0)%Z
    | 25%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0)%Z
    | 26%positive => (1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 27%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 29%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0)%Z
    | 30%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 31%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 32%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 33%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 34%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 35%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 36%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 37%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 38%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 39%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 40%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 41%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 42%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 43%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 44%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 45%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 46%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 47%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 48%positive => (1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 49%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0)%Z
    | 50%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 51%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_temp) + -11 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0)%Z
    | 52%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 53%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 13 <= 0)%Z
    | 54%positive => (-1 * (s IDAPCM_quantization_i) + 13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 55%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 13 <= 0)%Z
    | 56%positive => (-1 * (s IDAPCM_quantization_i) + 13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 57%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 13 <= 0)%Z
    | 58%positive => (-1 * (s IDAPCM_quantization_i) + 13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 59%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 60%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 61%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0)%Z
    | 63%positive => (-1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 64%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) + 16 <= 0)%Z
    | 65%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 66%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 67%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0)%Z
    | 68%positive => (1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 69%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0)%Z
    | 70%positive => (1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 71%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0)%Z
    | 72%positive => (1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 73%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0)%Z
    | 74%positive => (1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 75%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0)%Z
    | 76%positive => (1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0)%Z
    | 77%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ -1 * (s IDAPCM_quantization_temp1) <= 0 /\ 1 * (s IDAPCM_quantization_temp1) + -15 <= 0 /\ -1 * (s IDAPCM_quantization_z) + 1 <= 0)%Z
    | 78%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 79%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 80%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 81%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 82%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 83%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 84%positive => (1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0)%Z
    | 85%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 86%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 87%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 88%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 89%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ 1 * (s IDAPCM_quantization_itest) <= 0 /\ -1 * (s IDAPCM_quantization_itest) <= 0)%Z
    | 90%positive => (-1 * (s IDAPCM_quantization_itest) <= 0 /\ 1 * (s IDAPCM_quantization_itest) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 91%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -5 <= 0)%Z
    | 92%positive => (1 * (s IDAPCM_quantization_i) + -5 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 93%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0)%Z
    | 94%positive => (-1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0)%Z
    | 95%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -6 <= 0 /\ -1 * (s IDAPCM_quantization_z) + 1 <= 0)%Z
    | 97%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 98%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 99%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 100%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 101%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_temp) <= 0)%Z
    | 102%positive => (-1 * (s IDAPCM_quantization_temp) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 103%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + 1 <= 0)%Z
    | 104%positive => (1 * (s IDAPCM_quantization_temp) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 105%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + 1 <= 0)%Z
    | 106%positive => (1 * (s IDAPCM_quantization_temp) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 107%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + 1 <= 0)%Z
    | 108%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ 1 * (s IDAPCM_quantization_temp) + 1 <= 0)%Z
    | 109%positive => (1 * (s IDAPCM_quantization_temp) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 110%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 111%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 112%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 113%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ 1 * (s IDAPCM_quantization_temp)+ -1 * (s IDAPCM_quantization_xmax) <= 0)%Z
    | 114%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_temp)+ 1 * (s IDAPCM_quantization_xmax) + 1 <= 0)%Z
    | 115%positive => (-1 * (s IDAPCM_quantization_temp)+ 1 * (s IDAPCM_quantization_xmax) + 1 <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 116%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 117%positive => (-1 * (s IDAPCM_quantization_i) <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -12 <= 0)%Z
    | 118%positive => (1 * (s IDAPCM_quantization_i) + -12 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0 /\ -1 * (s IDAPCM_quantization_i) <= 0)%Z
    | 119%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0)%Z
    | 120%positive => (-1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) <= 0)%Z
    | 121%positive => (-1 * (s IDAPCM_quantization_z) <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_i) + 1 <= 0)%Z
    | 122%positive => (-1 * (s IDAPCM_quantization_i) + 1 <= 0 /\ 1 * (s IDAPCM_quantization_i) + -13 <= 0 /\ -1 * (s IDAPCM_quantization_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition APCM_quantization_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((32 # 1))%Q
    | 2%positive => ((32 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 3%positive => ((32 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 4%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                     + max0((s IDAPCM_quantization_z)))%Q
    | 5%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                     + max0((s IDAPCM_quantization_z)))%Q
    | 6%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                     + max0((s IDAPCM_quantization_z)))%Q
    | 7%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                     + max0((s IDAPCM_quantization_z)))%Q
    | 8%positive => ((19 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 9%positive => ((19 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 10%positive => ((19 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 11%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 12%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 13%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 14%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 15%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 16%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 17%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 18%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 19%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 20%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 21%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 22%positive => ((76 # 77) - (76 # 77) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 23%positive => ((125 # 93) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (12 # 23) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i)))%Q
    | 24%positive => ((125 # 93) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (12 # 23) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i)))%Q
    | 25%positive => ((125 # 93) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (12 # 23) * max0(6 - (s IDAPCM_quantization_i))
                      + (51 # 62) * max0(12 - (s IDAPCM_quantization_i)))%Q
    | 26%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 27%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 28%positive => ((s IDAPCM_quantization_z))%Q
    | 29%positive => ((s IDAPCM_quantization_z))%Q
    | 30%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 31%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 32%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 33%positive => ((13 # 1) + (s IDAPCM_quantization_z))%Q
    | 34%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 35%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 36%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 37%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 38%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 39%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 40%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 41%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 42%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 43%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 44%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 45%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 46%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 47%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 48%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 49%positive => ((13 # 1) + max0((s IDAPCM_quantization_z)))%Q
    | 50%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 51%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 52%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 53%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 54%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 55%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 56%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 57%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 58%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 59%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 60%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 61%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 62%positive => (max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 63%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 64%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 65%positive => ((13 # 12) - (1 # 12) * (s IDAPCM_quantization_i)
                      - (1 # 12) * max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 66%positive => ((13 # 12) - (1 # 12) * (s IDAPCM_quantization_i)
                      - (1 # 12) * max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 67%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 68%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 69%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 70%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 71%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 72%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 73%positive => ((1 # 1) + max0(12 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 74%positive => ((1 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 75%positive => ((1 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 76%positive => ((1 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 77%positive => ((1 # 1) + max0(-1 + (s IDAPCM_quantization_z))
                      + max0(13 - (s IDAPCM_quantization_i)))%Q
    | 78%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 79%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 80%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 81%positive => ((209 # 42) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 82%positive => (-(38 # 21) + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 83%positive => (-(38 # 21) + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 84%positive => ((s IDAPCM_quantization_z))%Q
    | 85%positive => (-(38 # 21) + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 86%positive => (-(38 # 21) + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 87%positive => ((19 # 3) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 88%positive => ((19 # 3) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 89%positive => ((19 # 3) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 90%positive => ((95 # 14) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 91%positive => ((95 # 14) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 92%positive => ((95 # 14) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (19 # 14) * max0(5 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(12 - (s IDAPCM_quantization_i))
                      + (13 # 6) * max0((s IDAPCM_quantization_i)))%Q
    | 93%positive => ((57 # 7) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (13 # 6) * max0(-1 + (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i)))%Q
    | 94%positive => ((57 # 7) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (13 # 6) * max0(-1 + (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i)))%Q
    | 95%positive => ((57 # 7) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (13 # 6) * max0(-1 + (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i)))%Q
    | 96%positive => ((50 # 7) - (19 # 14) * (s IDAPCM_quantization_i)
                      + (s IDAPCM_quantization_z)
                      + (13 # 6) * max0(-1 + (s IDAPCM_quantization_i))
                      + (19 # 14) * max0(6 - (s IDAPCM_quantization_i))
                      + (19 # 42) * max0(13 - (s IDAPCM_quantization_i)))%Q
    | 97%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 98%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 99%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                      + max0((s IDAPCM_quantization_z)))%Q
    | 100%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 101%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 102%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 103%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 104%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 105%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 106%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 107%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 108%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 109%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 110%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 111%positive => ((19 # 1) + max0(13 - (s IDAPCM_quantization_i))
                       + max0((s IDAPCM_quantization_z)))%Q
    | 112%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 113%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 114%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + max0((s IDAPCM_quantization_z)))%Q
    | 115%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 116%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 117%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 118%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 119%positive => ((33 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 120%positive => ((33 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 121%positive => ((33 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | 122%positive => ((32 # 1) - (s IDAPCM_quantization_i)
                       + (s IDAPCM_quantization_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition APCM_quantization_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (13
                                                            - (s IDAPCM_quantization_i)) (12
                                                                    - (s IDAPCM_quantization_i)));
                     (*-1 0*) F_max0_ge_0 (12 - (s IDAPCM_quantization_i))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
    | 13%positive => []
    | 14%positive => [(*-0.452381 0*) F_max0_pre_decrement (13
                                                            - (s IDAPCM_quantization_i)) (1);
                      (*-0.37013 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (12
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (12
                                                                    - (s IDAPCM_quantization_i)))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-3.16667 0*) F_max0_monotonic (F_check_ge (6
                                                                   - 
                                                                   (s IDAPCM_quantization_i)) (5
                                                                    - (s IDAPCM_quantization_i)));
                      (*-3.16667 0*) F_max0_ge_0 (5
                                                  - (s IDAPCM_quantization_i));
                      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_arg (12
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (12
                                                                    - (s IDAPCM_quantization_i)) (0));
                      (*0 1.80952*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (6
                                                                    - (s IDAPCM_quantization_i)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-3.28415e-12 1*) F_max0_monotonic (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (5
                                                                    - (s IDAPCM_quantization_i)));
                      (*0 2.16667*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_i))) (F_check_ge ((s IDAPCM_quantization_i)) (0));
                      (*0 0.164502*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (6
                                                                    - (s IDAPCM_quantization_i)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                - (s IDAPCM_quantization_i))) (F_check_ge (0) (0))]
    | 23%positive => []
    | 24%positive => [(*-4.34553e-12 0.521645*) F_max0_monotonic (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (5
                                                                    - (s IDAPCM_quantization_i)));
                      (*-1.34416 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*0 1.34416*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_i)));
                      (*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_0 (12
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*-0.521645 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (0) (0))]
    | 25%positive => [(*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_arg (12
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (12
                                                                    - (s IDAPCM_quantization_i)) (0));
                      (*-0.521645 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (0))]
    | 26%positive => []
    | 27%positive => [(*-13 0*) F_one]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_z)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_z)))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-13 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-13 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
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
    | 58%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (13
                                                             - (s IDAPCM_quantization_i)) (12
                                                                    - (s IDAPCM_quantization_i)));
                      (*-1 0*) F_max0_ge_0 (12 - (s IDAPCM_quantization_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-0.0833333 1*) F_max0_pre_decrement (13
                                                             - (s IDAPCM_quantization_i)) (1);
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*0 0.0833333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_i)));
                      (*-1.08333 0*) F_binom_monotonic 1 (F_max0_ge_0 (12
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (0) (0))]
    | 62%positive => [(*0 1*) F_max0_pre_decrement (13
                                                    - (s IDAPCM_quantization_i)) (1)]
    | 63%positive => []
    | 64%positive => [(*-0.0833333 0*) F_max0_pre_decrement (13
                                                             - (s IDAPCM_quantization_i)) (1);
                      (*-1.08333 0*) F_max0_ge_0 (12
                                                  - (s IDAPCM_quantization_i));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_i)))]
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDAPCM_quantization_i)))]
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
    | 77%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_z)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDAPCM_quantization_z))) (F_check_ge (-1
                                                                    + (s IDAPCM_quantization_z)) (0))]
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => [(*-1.35714 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDAPCM_quantization_i)))]
    | 82%positive => []
    | 83%positive => [(*-1.35714 0*) F_max0_pre_decrement (6
                                                           - (s IDAPCM_quantization_i)) (1);
                      (*-0.452381 0*) F_max0_pre_decrement (13
                                                            - (s IDAPCM_quantization_i)) (1);
                      (*-0.452381 0*) F_max0_ge_0 (12
                                                   - (s IDAPCM_quantization_i));
                      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDAPCM_quantization_i))) (F_check_ge (0) (0));
                      (*-2.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (0) (0))]
    | 84%positive => []
    | 85%positive => []
    | 86%positive => [(*-1.35714 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                                    - (s IDAPCM_quantization_i))) (F_check_ge (6
                                                                    - (s IDAPCM_quantization_i)) (0))]
    | 87%positive => []
    | 88%positive => [(*-0.452381 0*) F_max0_pre_decrement (13
                                                            - (s IDAPCM_quantization_i)) (1)]
    | 89%positive => [(*-0.452381 0*) F_max0_pre_decrement (13
                                                            - (s IDAPCM_quantization_i)) (1)]
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => [(*-2.16667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_i)));
                      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDAPCM_quantization_i))) (F_check_ge (-1
                                                                    + (s IDAPCM_quantization_i)) (0))]
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                                   - 
                                                                   (s IDAPCM_quantization_i))) (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))]
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDAPCM_quantization_i)))]
    | 108%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDAPCM_quantization_i)))]
    | 109%positive => []
    | 110%positive => []
    | 111%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                                   - 
                                                                   (s IDAPCM_quantization_i))) (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))]
    | 112%positive => []
    | 113%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
    | 114%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDAPCM_quantization_z))) (F_check_ge ((s IDAPCM_quantization_z)) (0))]
    | 115%positive => []
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDAPCM_quantization_z)) (0))) (F_max0_ge_0 ((s IDAPCM_quantization_z)));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDAPCM_quantization_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDAPCM_quantization_i)))]
    | _ => []
  end.


Theorem APCM_quantization_ai_correct:
  forall s p' s', steps (g_start APCM_quantization) s (g_edges APCM_quantization) p' s' -> APCM_quantization_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem APCM_quantization_pot_correct:
  forall s p' s',
    steps (g_start APCM_quantization) s (g_edges APCM_quantization) p' s' ->
    (APCM_quantization_pot (g_start APCM_quantization) s >= APCM_quantization_pot p' s')%Q.
Proof.
  check_lp APCM_quantization_ai_correct APCM_quantization_hints.
Qed.

