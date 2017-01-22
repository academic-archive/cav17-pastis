Require Import pasta.Pasta.

Notation IDcmd_put_color_mapping_z := 1%positive.
Notation IDcmd_put_color_mapping__tmp := 2%positive.
Notation IDcmd_put_color_mapping__tmp1 := 3%positive.
Notation IDcmd_put_color_mapping_all_same := 4%positive.
Notation IDcmd_put_color_mapping_code := 5%positive.
Notation IDcmd_put_color_mapping_i := 6%positive.
Notation IDcmd_put_color_mapping_which := 7%positive.
Notation IDcmd_put_color_mapping_cldev := 8%positive.
Notation IDcmd_put_color_mapping_pis := 9%positive.
Notation IDcmd_put_color_mapping_write_rgb_to_cmyk := 10%positive.
Definition cmd_put_color_mapping : graph := {|
  g_start := 1%positive;
  g_end := 117%positive;
  g_edges := (1%positive,(AAssign IDcmd_put_color_mapping_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcmd_put_color_mapping__tmp1
             (Some (EVar IDcmd_put_color_mapping_write_rgb_to_cmyk))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,ANone,8%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,14%positive)::
             (8%positive,(AAssign IDcmd_put_color_mapping_code None),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) <
             (eval (ENum (0)) s))%Z)),113%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) >=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),16%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping__tmp1) s) =
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,26%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDcmd_put_color_mapping_code None),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) <
             (eval (ENum (0)) s))%Z)),109%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) >=
             (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDcmd_put_color_mapping_code None),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) <
             (eval (ENum (0)) s))%Z)),105%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) >=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDcmd_put_color_mapping_which
             (Some (ENum (0)))),27%positive)::
             (27%positive,(AAssign IDcmd_put_color_mapping_all_same
             (Some (ENum (1)))),28%positive)::
             (28%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (ENum (0)))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) <
             (eval (ENum (4)) s))%Z)),90%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) >=
             (eval (ENum (4)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_which) s) =
             (eval (ENum (0)) s))%Z)),86%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_which) s) <>
             (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_which) s) =
             (eval (ENum (15)) s))%Z)),37%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_which) s) <>
             (eval (ENum (15)) s))%Z)),36%positive)::
             (36%positive,AWeaken,40%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_all_same) s) <>
             (eval (ENum (0)) s))%Z)),61%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_all_same) s) =
             (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (ENum (0)))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) <
             (eval (ENum (4)) s))%Z)),46%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) >=
             (eval (ENum (4)) s))%Z)),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,72%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AAssign IDcmd_put_color_mapping_code None),
             48%positive)::(48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) <
             (eval (ENum (0)) s))%Z)),57%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) >=
             (eval (ENum (0)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (EAdd (EVar IDcmd_put_color_mapping_i) (ENum (1))))),
             53%positive)::(53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDcmd_put_color_mapping_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_put_color_mapping_z)))),
             56%positive)::(56%positive,AWeaken,43%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (EVar IDcmd_put_color_mapping_code))),59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,AWeaken,117%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AAssign IDcmd_put_color_mapping_code None),
             63%positive)::(63%positive,AWeaken,64%positive)::
             (64%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) <
             (eval (ENum (0)) s))%Z)),82%positive)::
             (64%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_code) s) >=
             (eval (ENum (0)) s))%Z)),65%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (ENum (1)))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) <
             (eval (ENum (4)) s))%Z)),75%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_color_mapping_i) s) >=
             (eval (ENum (4)) s))%Z)),70%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (ENum (0)))),73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,AWeaken,117%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (EAdd (EVar IDcmd_put_color_mapping_i) (ENum (1))))),
             78%positive)::(78%positive,ANone,79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,(AAssign IDcmd_put_color_mapping_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_put_color_mapping_z)))),
             81%positive)::(81%positive,AWeaken,69%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (EVar IDcmd_put_color_mapping_code))),84%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,AWeaken,117%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (ENum (0)))),88%positive)::
             (88%positive,ANone,89%positive)::
             (89%positive,AWeaken,117%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,ANone,93%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,AWeaken,96%positive)::
             (93%positive,(AAssign IDcmd_put_color_mapping_which None),
             94%positive)::(94%positive,ANone,95%positive)::
             (95%positive,AWeaken,96%positive)::
             (96%positive,ANone,97%positive)::
             (96%positive,ANone,99%positive)::
             (97%positive,(AAssign IDcmd_put_color_mapping_all_same
             (Some (ENum (0)))),98%positive)::
             (98%positive,ANone,99%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,(AAssign IDcmd_put_color_mapping_i
             (Some (EAdd (EVar IDcmd_put_color_mapping_i) (ENum (1))))),
             101%positive)::(101%positive,ANone,102%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,(AAssign IDcmd_put_color_mapping_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_put_color_mapping_z)))),
             104%positive)::(104%positive,AWeaken,31%positive)::
             (105%positive,AWeaken,106%positive)::
             (106%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (EVar IDcmd_put_color_mapping_code))),107%positive)::
             (107%positive,ANone,108%positive)::
             (108%positive,AWeaken,117%positive)::
             (109%positive,AWeaken,110%positive)::
             (110%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (EVar IDcmd_put_color_mapping_code))),111%positive)::
             (111%positive,ANone,112%positive)::
             (112%positive,AWeaken,117%positive)::
             (113%positive,AWeaken,114%positive)::
             (114%positive,(AAssign IDcmd_put_color_mapping__tmp
             (Some (EVar IDcmd_put_color_mapping_code))),115%positive)::
             (115%positive,ANone,116%positive)::
             (116%positive,AWeaken,117%positive)::nil
|}.

Definition cmd_put_color_mapping_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 11%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 14%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp1) <= 0 /\ -1 * (s IDcmd_put_color_mapping__tmp1) <= 0)%Z
    | 16%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 17%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 18%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 20%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 22%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 24%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 25%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 26%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0)%Z
    | 28%positive => (-1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_all_same) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDcmd_put_color_mapping_all_same) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 30%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_all_same) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 32%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 33%positive => (-1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 34%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 35%positive => (-1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 37%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0)%Z
    | 38%positive => (-1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 39%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) <= 0 /\ -1 * (s IDcmd_put_color_mapping_all_same) <= 0)%Z
    | 40%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 41%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 42%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 43%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 44%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 45%positive => (-1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 46%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 47%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 48%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 49%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 50%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 51%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 52%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 53%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 54%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 55%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 56%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) + 1 <= 0)%Z
    | 57%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0)%Z
    | 58%positive => (1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 59%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0)%Z
    | 60%positive => (1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 61%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 62%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 63%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 64%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 65%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 66%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 67%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 69%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 70%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 71%positive => (-1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 72%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 73%positive => (-1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ -1 * (s IDcmd_put_color_mapping__tmp) <= 0)%Z
    | 74%positive => (-1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0)%Z
    | 75%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 76%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0)%Z
    | 77%positive => (-1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 78%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 2 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 79%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 2 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | 80%positive => (-1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 2 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 81%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 2 <= 0 /\ -1 * (s IDcmd_put_color_mapping_code) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) + 1 <= 0)%Z
    | 82%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0)%Z
    | 83%positive => (1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 84%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0)%Z
    | 85%positive => (1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) + 15 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) + -15 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 86%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0)%Z
    | 87%positive => (-1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 88%positive => (1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ -1 * (s IDcmd_put_color_mapping__tmp) <= 0)%Z
    | 89%positive => (-1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ -1 * (s IDcmd_put_color_mapping_which) <= 0 /\ 1 * (s IDcmd_put_color_mapping_which) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0)%Z
    | 90%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 91%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 92%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 93%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 94%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 95%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 96%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 97%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 98%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) <= 0 /\ -1 * (s IDcmd_put_color_mapping_all_same) <= 0)%Z
    | 99%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -3 <= 0)%Z
    | 100%positive => (1 * (s IDcmd_put_color_mapping_i) + -3 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 101%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0)%Z
    | 102%positive => (-1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0)%Z
    | 103%positive => (1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) + 1 <= 0)%Z
    | 104%positive => (-1 * (s IDcmd_put_color_mapping_i) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_i) + -4 <= 0 /\ 1 * (s IDcmd_put_color_mapping_all_same) + -1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0)%Z
    | 106%positive => (1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 107%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0)%Z
    | 108%positive => (1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 109%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0)%Z
    | 110%positive => (1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 111%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0)%Z
    | 112%positive => (1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 113%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0)%Z
    | 114%positive => (1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 115%positive => (-1 * (s IDcmd_put_color_mapping_i) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0)%Z
    | 116%positive => (1 * (s IDcmd_put_color_mapping__tmp) + 1 <= 0 /\ 1 * (s IDcmd_put_color_mapping_code) + 1 <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0 /\ 1 * (s IDcmd_put_color_mapping_z) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0)%Z
    | 117%positive => (1 * (s IDcmd_put_color_mapping__tmp) <= 0 /\ -1 * (s IDcmd_put_color_mapping_i) <= 0 /\ -1 * (s IDcmd_put_color_mapping_z) <= 0)%Z
    | _ => False
  end.

Definition cmd_put_color_mapping_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 3%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 4%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 5%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 6%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 7%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 8%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 9%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 10%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 11%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 12%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 13%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 14%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 15%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 16%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 17%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 18%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 19%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 20%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 21%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 22%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 23%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 24%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 25%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 26%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 27%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 28%positive => ((8 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 29%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 30%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 31%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 32%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 33%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 34%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 35%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 36%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 37%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 38%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 39%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 40%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 41%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 42%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 43%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 44%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 45%positive => ((s IDcmd_put_color_mapping_z))%Q
    | 46%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 47%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 48%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 49%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 50%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 51%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 52%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 53%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 54%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 55%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 56%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 57%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 58%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 59%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 60%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 61%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 62%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 63%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i))
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 64%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 65%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 66%positive => ((3 # 1) + (s IDcmd_put_color_mapping_z))%Q
    | 67%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 68%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 69%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 70%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 71%positive => ((s IDcmd_put_color_mapping_z))%Q
    | 72%positive => ((s IDcmd_put_color_mapping_z))%Q
    | 73%positive => ((s IDcmd_put_color_mapping_z))%Q
    | 74%positive => ((s IDcmd_put_color_mapping_z))%Q
    | 75%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 76%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 77%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 78%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 79%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 80%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 81%positive => ((s IDcmd_put_color_mapping_z)
                      + max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 82%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 83%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 84%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 85%positive => ((4 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 86%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 87%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 88%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 89%positive => ((1 # 1) + (s IDcmd_put_color_mapping_z)
                      + max0(-1 + (s IDcmd_put_color_mapping_i))
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 90%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 91%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 92%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 93%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 94%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 95%positive => ((s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 96%positive => ((2 # 1) + (s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 97%positive => ((2 # 1) + (s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 98%positive => ((2 # 1) + (s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 99%positive => ((2 # 1) + (s IDcmd_put_color_mapping_i)
                      + (s IDcmd_put_color_mapping_z)
                      + (2 # 1) * max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 100%positive => ((2 # 1) + (s IDcmd_put_color_mapping_i)
                       + (s IDcmd_put_color_mapping_z)
                       + (2 # 1) * max0(3 - (s IDcmd_put_color_mapping_i)))%Q
    | 101%positive => ((1 # 1) + (s IDcmd_put_color_mapping_i)
                       + (s IDcmd_put_color_mapping_z)
                       + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 102%positive => ((1 # 1) + (s IDcmd_put_color_mapping_i)
                       + (s IDcmd_put_color_mapping_z)
                       + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 103%positive => ((1 # 1) + (s IDcmd_put_color_mapping_i)
                       + (s IDcmd_put_color_mapping_z)
                       + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 104%positive => ((s IDcmd_put_color_mapping_i)
                       + (s IDcmd_put_color_mapping_z)
                       + (2 # 1) * max0(4 - (s IDcmd_put_color_mapping_i)))%Q
    | 105%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 106%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 107%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 108%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 109%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 110%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 111%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 112%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 113%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 114%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 115%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 116%positive => ((8 # 1) + max0((s IDcmd_put_color_mapping_z)))%Q
    | 117%positive => ((s IDcmd_put_color_mapping_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_put_color_mapping_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))) (F_max0_ge_0 ((s IDcmd_put_color_mapping_z)))]
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_color_mapping_z))) (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_color_mapping_z))) (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcmd_put_color_mapping_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcmd_put_color_mapping_i)))]
    | 33%positive => []
    | 34%positive => [(*0 1*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDcmd_put_color_mapping_i)) (3
                                                                    - (s IDcmd_put_color_mapping_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDcmd_put_color_mapping_i))) (F_check_ge (-1
                                                                    + (s IDcmd_put_color_mapping_i)) (0))]
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDcmd_put_color_mapping_i))) (F_check_ge (4
                                                                    - (s IDcmd_put_color_mapping_i)) (0))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-1 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDcmd_put_color_mapping_i))) (F_check_ge (4
                                                                    - (s IDcmd_put_color_mapping_i)) (0))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*0 1*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDcmd_put_color_mapping_i)) (3
                                                                    - (s IDcmd_put_color_mapping_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                 - (s IDcmd_put_color_mapping_i))) (F_check_ge (0) (0))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDcmd_put_color_mapping_i)) (1)]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => [(*-1 0*) F_max0_ge_0 (4 - (s IDcmd_put_color_mapping_i))]
    | 61%positive => []
    | 62%positive => []
    | 63%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                   - 
                                                                   (s IDcmd_put_color_mapping_i))) (F_check_ge (4
                                                                    - (s IDcmd_put_color_mapping_i)) (0))]
    | 64%positive => []
    | 65%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i))]
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDcmd_put_color_mapping_i)) (3
                                                                    - (s IDcmd_put_color_mapping_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i))]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDcmd_put_color_mapping_i)) (1)]
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => [(*-1 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i));
                      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcmd_put_color_mapping_which))) (F_check_ge (0) (0));
                      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_put_color_mapping_which)) (0))) (F_max0_ge_0 ((s IDcmd_put_color_mapping_which)));
                      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - (s IDcmd_put_color_mapping_which))) (F_check_ge (0) (0));
                      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (15
                                                                    - (s IDcmd_put_color_mapping_which)) (0))) (F_max0_ge_0 (15
                                                                    - (s IDcmd_put_color_mapping_which)))]
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => [(*-2 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDcmd_put_color_mapping_i)) (3
                                                                    - (s IDcmd_put_color_mapping_i)));
                      (*-2 0*) F_max0_ge_0 (3 - (s IDcmd_put_color_mapping_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcmd_put_color_mapping_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_put_color_mapping_i)) (0))) (F_max0_ge_0 ((s IDcmd_put_color_mapping_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDcmd_put_color_mapping_i))) (F_check_ge (-1
                                                                    + (s IDcmd_put_color_mapping_i)) (0))]
    | 90%positive => []
    | 91%positive => []
    | 92%positive => [(*-2 0*) F_max0_pre_decrement (4
                                                     - (s IDcmd_put_color_mapping_i)) (1)]
    | 93%positive => []
    | 94%positive => []
    | 95%positive => [(*-2 0*) F_max0_pre_decrement (4
                                                     - (s IDcmd_put_color_mapping_i)) (1)]
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
    | 108%positive => [(*-8 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_color_mapping_z))) (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))]
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => [(*-8 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_color_mapping_z))) (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))]
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => [(*-8 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_color_mapping_z))) (F_check_ge ((s IDcmd_put_color_mapping_z)) (0))]
    | 117%positive => []
    | _ => []
  end.


Theorem cmd_put_color_mapping_ai_correct:
  forall s p' s', steps (g_start cmd_put_color_mapping) s (g_edges cmd_put_color_mapping) p' s' -> cmd_put_color_mapping_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_put_color_mapping_pot_correct:
  forall s p' s',
    steps (g_start cmd_put_color_mapping) s (g_edges cmd_put_color_mapping) p' s' ->
    (cmd_put_color_mapping_pot (g_start cmd_put_color_mapping) s >= cmd_put_color_mapping_pot p' s')%Q.
Proof.
  check_lp cmd_put_color_mapping_ai_correct cmd_put_color_mapping_hints.
Qed.

